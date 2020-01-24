#!/bin/bash
FILE=./bin/typelevelLR
if [[ -f "$FILE" ]]; then
  echo "$FILE exists, use build cache."
  mkdir -p /root/.local/bin/
  cp ./bin/typelevelLR /root/.local/bin/
  /bin/bash
  exit 0
fi

echo "building typelevelLR..."
stack install && mkdir -p bin  && cp $(which typelevelLR) ./bin/
/bin/bash
