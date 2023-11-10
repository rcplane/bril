#!/bin/zsh

echo "CS 6120 L12 dynamic compilers: trace executions of bril programs, stitch lvn dce and run checking correctness and dynamic instruction count"


echo " birthday 23 record trace"
echo " birthday 23 from trace dce lvn"
echo " birthday 22 from trace dce lvn miss"
echo " birthday 50 from trace dce lvn miss"
echo " and last three again default profiling" 
echo " "
echo " gcd 4 20 record trace"
echo " gcd 4 20 from trace dce lvn"
echo " gcd 4 80 from trace dce lvn ?hit"
echo " gcd 3 21 from trace dce lvn miss"
echo " and last three again default profiling"
echo " "
echo " perfect 496 record trace"
echo " perfect 496 from trace dce lvn"
echo " perfect 28  from trace dce lvn ?hit"
echo " perfect 12  from trace dce lvn miss"
echo " and last three again default profiling"

echo " prerequisite usual brili deno and haskell ghc and stack installs"
echo " https://github.com/rcplane/bril/tree/l12#reference-interpreter"
echo " https://github.com/rcplane/bril/tree/l12/bril-hs#usage "

set -x

bril2json < benchmarks/core/birthday.bril | brili -p -t benchmarks/core/birthday.trace 23
cat benchmarks/core/birthday.trace | stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn | brili -p -i benchmarks/core/birthday.args
bril2json < benchmarks/core/birthday.bril | brili -p -i benchmarks/core/birthday.args

bril2json < benchmarks/core/gcd.bril | brili -p -t benchmarks/core/gcd.trace 4 20
cat benchmarks/core/gcd.trace | stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn | brili -p -i benchmarks/core/gcd.args
bril2json < benchmarks/core/gcd.bril | brili -p -i benchmarks/core/gcd.args

bril2json < benchmarks/core/perfect.bril | brili -p -t benchmarks/core/perfect.trace 496  # todo head -n 1 args
cat benchmarks/core/perfect.trace | stack --stack-yaml bril-hs/stack.yaml run -- --dce --lvn | brili -p -i benchmarks/core/perfect.args
bril2json < benchmarks/core/perfect.bril | brili -p -i benchmarks/core/perfect.args

