FROM fpco/stack-build-small:lts-16.27 AS builder
LABEL maintainer="Freckle Engineering <engineering@freckle.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /src
WORKDIR /src
COPY stack.yaml package.yaml /src/
RUN stack install --dependencies-only
COPY src /src/src
COPY rcl /src/rcl
COPY rcl-stack-dependencies /src/rcl-stack-dependencies
COPY rcl-web /src/rcl-web
RUN stack install

FROM ubuntu:18.04
LABEL maintainer="Freckle Engineering <engineering@freckle.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    gcc \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /app
WORKDIR /app
COPY --from=builder /root/.local/bin/rcl-web /app/rcl-web
CMD ["/app/rcl-web"]
