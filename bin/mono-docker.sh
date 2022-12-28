#!/bin/bash

docker build -t mono-with-java ./bin

docker run \
  -v $PWD:/data \
  -it mono-with-java bash
