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
            [atomist.local-runner :as lr]
            [atomist.cljs-log :as log]
            [atomist.container :as container]
            [goog.string :as gstring]
            [cljs-node-io.core :as io]
            [cljs-node-io.proc :as proc]
            [cljs.core.async :refer [<! >! chan timeout]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [atomist.json :as json])
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

(defn ->tx
  "Add each dep to a new commit for the `many`"
  [[n v depth]]
  (let [entity-id (str n ":" v)]
    {:schema/entity-type :npm/package
     :npm.package/name n
     :npm.package/version v
     :schema/entity entity-id}))

(defn- flatten-deps
  ([m package depth coll]
   (-> coll
       (concat (and package (when-let [v (get m "version")]
                              [[package v (< depth 2)]])))
       (concat
        (when-let [deps (get m "dependencies")]
          (mapcat (fn [[package m]] (flatten-deps m package (inc depth) coll)) (seq deps))))))
  ([m]
   (flatten-deps m nil 0 [])))

(comment
  (-> (io/slurp "resources/npm-ls-stdout.json")
      (json/->obj :keywordize-keys false)
      (flatten-deps)))

(defn transact-deps
  [request stdout]
  (go
    (try
      (let [[commit] (-> request :subscription :result first)
            repo (:git.commit/repo commit)
            org (:git.repo/org repo)
            deps-tx (-> stdout
                        (json/->obj :keywordize-keys false)
                        (flatten-deps)
                        (as-> all-deps (->> all-deps
                                            ;; TODO record only top-level deps
                                            (filter (fn [[_ _ top-level?]] top-level?))
                                            (map ->tx)
                                            (sort-by :schema/entity)
                                            (partition-by :schema/entity)
                                            (map first))))]

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
        (println "exception " ex)
        (log/errorf ex "Unable to transact %s" (->> stdout (take 80) (apply str)))))))

(defn run-npm-ls [handler]
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

(defn run-npm-ci [handler]
  (fn [request]
    (go
      (let [cwd (io/file (-> request :project :path))]
        (if (.exists (io/file cwd "package.json"))
          (let [[err stdout stderr] (<! (proc/aexec "npm ci"
                                                    {:cwd (.getPath cwd)}))]
            (if err
              (assoc request :atomist/status {:code 1 :reason stderr})
              (<! (handler request))))
          (<! (handler (assoc request :atomist/status {:code 1 :reason "no package.json file" :visibility :hidden}))))))))

(comment
  (enable-console-print!)
  (go (-> (<! ((-> #(go %)
                   (run-npm-ls)
                   (run-npm-ci))
               {:project
                {:path "/Users/slim/skills/gcr-integration"}
                :api_version "1"
                :correlation_id "corrid"
                :sendreponse (fn [& args] (go (-> args
                                                  first
                                                  (js->clj :keywordize-keys true)
                                                  :entities
                                                  (edn/read-string)
                                                  pprint)))}))
          :atomist/status
          println)))

(enable-console-print!)

(defn ^:export handler
  "no arguments because this handler runs in a container that should fulfill the Atomist container contract
   the context is extract fro the environment using the container/mw-make-container-request middleware"
  []
  ((-> (api/finished)
       (run-npm-ls)
       (run-npm-ci)
       (api/clone-ref)
       (create-ref-from-event)
       (api/log-event)
       (api/status)
       (container/mw-make-container-request))
   {}))

(comment
  (lr/set-env :prod-github-auth)
  (def event {:subscription
              {:result [[{:git.commit/repo
                          {:git.repo/name "gcr-integration"
                           :git.repo/org
                           {:git.org/name "atomist-skills"}}
                          :git.commit/sha "1729d396216fa9c8cc29e9fe0dbce2da829fe1f0"}]]}
              :team {:id "T29E48P34"}
              :correlation_id "corrid"
              :api_version "1"
              :secrets [{:uri "atomist://api-key" :value (lr/token)}]
              :ref {:owner "atomist-skills"}})
  (go (-> (<! ((-> (api/finished)
                   (run-npm-ls)
                   (api/clone-ref)
                   (create-ref-from-event)
                   (lr/add-token-to-subscription)
                   (api/status))
               event))
          (:atomist/status)
          (println))))

