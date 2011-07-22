#!/bin/bash
rm vec3.h vec3.c
cabal-dev build && ./dist/build/cheap/cheap vec3.class && echo "--------" && g++ -c -g -o vec3.o  vec3.c && echo 'Success!!'
