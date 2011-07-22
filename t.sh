#!/bin/bash
rm vec3.h vec3.c
cabal-dev build && ./dist/build/daemon/daemon vec3.class && echo "--------" && g++ -c -g -o vec3.o  vec3.c && echo 'Success!!'
