FROM gcr.io/atomist-container-skills/clojure-base:0.0.1@sha256:cd6e9de3de5b121dc7aef0cb3f6a1b58423049ce6db08fd7161cac7b39722f55 AS build

WORKDIR /usr/src

COPY deps.edn shadow-cljs.edn /usr/src/
COPY package*.json /usr/src/
COPY src /usr/src/src

RUN npm ci --no-optional && \
    npm run build && \
    rm -rf node_modules .git

FROM ubuntu:focal@sha256:9101220a875cee98b016668342c489ff0674f247f6ca20dfc91b91c0f28581ae

# tools
RUN apt-get update && apt-get install -y \
    curl=7.68.0-1ubuntu2.5 \
    git=1:2.25.1-1ubuntu3.1 \
    wget=1.20.3-1ubuntu1 \
 && rm -rf /var/lib/apt/lists/*

# node
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

WORKDIR "/skill"

COPY package.json package-lock.json ./

RUN bash -c "npm ci --no-optional \
    && npm cache clean --force"

COPY --from=build /usr/src/ .

ENV NODE_NO_WARNINGS 1

ENTRYPOINT ["node", "--no-deprecation", "--no-warnings", "--expose_gc", "--optimize_for_size", "--always_compact", "--max_old_space_size=512" ]
CMD ["index.js"]
 
