#!/bin/zsh
#
# cd bril
cd benchmarks
bril2json < core/ackermann.bril | brili -i core/ackermann.args

bril2json < core/ackermann.bril | brili 3 6
bril2json < core/ackermann.bril | brili 1 2
bril2json < core/ackermann.bril | brili 2 2
