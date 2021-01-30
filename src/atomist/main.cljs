;; Copyright © 2021 Atomist, Inc.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns atomist.main
  (:require [atomist.api :as api]
            [atomist.cljs-log :as log]
            [atomist.container :as container]
            [goog.string :as gstring]
            [cljs-node-io.core :as io]
            [cljs-node-io.proc :as proc]
            [cljs.core.async :refer [<! >! chan timeout]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn create-ref-from-event
  [handler]
  (fn [request]
    (let [[commit] (-> request :subscription :result first)
          repo (:git.commit/repo commit)
          org (:git.repo/org repo)]
      (handler (assoc request :ref {:repo (:git.repo/name repo)
                                    :owner (:git.org/name org)
                                    :sha (:git.commit/sha commit)}
                      :token (:github.org/installation-token org))))))

(defn -js->clj+
  "For cases when built-in js->clj doesn't work. Source: https://stackoverflow.com/a/32583549/4839573"
  [x]
  (into {} (for [k (js-keys x)] [k (aget x k)])))

(defn ->tx
  "Add each dep to a new commit for the `many`"
  [n v]
  (let [entity-id (str n ":" v)]
    {:schema/entity-type :npm/package
     :npm.package/name n
     :npm.package/version v
     :schema/entity entity-id}))

(defn transact-deps
  [request std-out]
  (go
    (try
      (let [[commit] (-> request :subscription :result first)
            repo (:git.commit/repo commit)
            org (:git.repo/org repo)
            deps-tx (->>
                     std-out
                     edn/read-string
                     (map first)
                     (map #(take 2 %))
                     (map ->tx))]

        (<! (api/transact request (concat [{:schema/entity-type :git/repo
                                            :schema/entity "$repo"
                                            :git.provider/url (:git.provider/url org)
                                            :git.repo/source-id (:git.repo/source-id repo)}
                                           {:schema/entity-type :git/commit
                                            :git.provider/url (:git.provider/url org)
                                            :git.commit/sha (:git.commit/sha commit)
                                            :project.dependencies/npm {:add (map :schema/entity deps-tx)}
                                            :git.commit/repo "$repo"}]
                                          deps-tx))))
      (catch :default ex
        (log/errorf ex "Unable to transact %s" std-out)))))

(defn run-deps-tree [handler]
  (fn [request]
    (go
      (let [cwd (io/file (-> request :project :path))]
        (cond
          (.exists (io/file cwd "package.json"))
          (let [[err stdout stderr] (<! (proc/aexec "npm ls --json --prod --depth=10"
                                                    {:cwd (.getPath cwd)}))]
            (cond

              err
              (assoc request :atomist/status {:code 1 :reason stderr})

              :else
              (do
                (<! (transact-deps request stdout))
                (<! (handler (assoc request
                                    :atomist/status {:code 0 :reason "tracking Node.js project" :visibility :hidden}))))))
          :else
          (<! (handler (assoc request :atomist/status {:code 1 :reason "no package.json file" :visibility :hidden}))))))))

(defn ^:export handler
  "no arguments because this handler runs in a container that should fulfill the Atomist container contract
   the context is extract fro the environment using the container/mw-make-container-request middleware"
  []
  ((-> (api/finished)
       (run-deps-tree)
       (api/clone-ref)
       (create-ref-from-event)
       (api/log-event)
       (api/status)
       (container/mw-make-container-request))
   {}))
