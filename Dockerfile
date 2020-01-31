FROM openjdk:8u181-jdk
WORKDIR /workdir

RUN apt-get update && curl -sSL \
    https://get.haskellstack.org/ | sh

RUN apt-get install -y scala

ENV PATH=$PATH:/root/.local/bin
