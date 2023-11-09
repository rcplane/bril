#!/bin/zsh

# vim $(which brili)
# ... deno run --allow-read --allow-write ...
# cd bril

cd benchmarks
turnt -e brili core/*.bril

# todo turnt with -t tracing or -i more args to test
