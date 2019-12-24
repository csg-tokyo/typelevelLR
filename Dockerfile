FROM haskell:8.0.2
WORKDIR /workdir
ENTRYPOINT [ "./docker-entrypoint.sh" ]
