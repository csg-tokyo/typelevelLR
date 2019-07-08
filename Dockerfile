
FROM haskell:8.0.2
RUN git clone https://github.com/csg-tokyo/typelevelLR.git
RUN cd typelevelLR && stack install
WORKDIR /workdir
ENTRYPOINT ["typelevelLR"]
