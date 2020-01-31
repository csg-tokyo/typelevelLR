#!/bin/bash
FILE=./bin/typelevelLR
if [[ -f "$FILE" ]]; then
  echo "$FILE exists, use build cache."
  cp ./bin/typelevelLR /usr/local/bin
  /bin/bash
  exit 0
fi

echo "building typelevelLR..."
stack install && mkdir -p bin  && cp $(which typelevelLR) ./bin/
/bin/bash
