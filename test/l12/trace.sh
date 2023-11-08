#!/bin/zsh

# git checkout bril
# cd bril
# deno install brili-log.ts

###bril2json < benchmarks/core/gcd.bril | brili-log 4 20
bril2json < benchmarks/core/gcd.bril | brili 4 20

