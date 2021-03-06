FROM gcr.io/atomist-container-skills/clojure-base:0.0.1 AS build

WORKDIR /usr/src

COPY deps.edn shadow-cljs.edn /usr/src/
COPY package*.json /usr/src/
COPY src /usr/src/src

RUN npm ci --no-optional && \
    npm run build && \
    rm -rf node_modules .git

FROM ubuntu:focal

# tools
RUN apt-get update && apt-get install -y \
        curl \
        wget \
        git \
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
 
