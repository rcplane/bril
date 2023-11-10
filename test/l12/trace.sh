#!/bin/zsh

set -x

# git checkout bril
# cd bril
# deno install brili.ts
# # vim $(which brili)
# ... deno run --allow-read --allow-write ...

###bril2json < benchmarks/core/gcd.bril | brili-log 4 20
#bril2json < benchmarks/core/gcd.bril | brili -t benchmarks/core/gcd.trace 4 20
# todo next : resolve non-writing (gcd) and non-termination (ack) with tracing on
# stitch in trace with guards
# notice multiples to hit fast trace then multiply to get gcd answer
# bril2json < benchmarks/core/gcd.bril | brili 16 80
# not clean multiples hit guard fall out of trace
# bril2json < benchmarks/core/gcd.bril | brili 3 20
# stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn

bril2json < benchmarks/core/birthday.bril | brili -p 23 | tee -a birthday.out
bril2json < benchmarks/core/birthday.bril | brili -p -t benchmarks/core/birthday.trace 23 | tee -a birthday.out
cat benchmarks/core/birthday.trace | brili -p 23 | tee -a birthday.t.out
cat benchmarks/core/birthday.trace | brili -p 22 | tee -a birthday.t2.out
bril2json < benchmarks/core/birthday.bril | brili -p 22 | tee -a birthday.out

#cd bril-hs
cat benchmarks/core/birthday.trace | stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn | brili -p 23 | tee -a birthday.l.out

cat benchmarks/core/birthday.bril | bril2json | stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn | brili -p 23 | tee -a birthday.l2.out
cat benchmarks/core/birthday.trace | stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn | brili -p 23 | tee -a birthday.l.out

