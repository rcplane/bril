#!/bin/zsh

# git checkout bril
# cd bril
# deno install brili.ts
# # vim $(which brili)
# ... deno run --allow-read --allow-write ...

###bril2json < benchmarks/core/gcd.bril | brili-log 4 20
bril2json < benchmarks/core/gcd.bril | brili -t benchmarks/core/gcd.trace 4 20

# todo next : resolve non-writing (gcd) and non-termination (ack) with tracing on

# stitch in trace with guards

# notice multiples to hit fast trace then multiply to get gcd answer
# bril2json < benchmarks/core/gcd.bril | brili 16 80

# not clean multiples hit guard fall out of trace
# bril2json < benchmarks/core/gcd.bril | brili 3 20
