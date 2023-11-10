`bril-hs`: A Haskell libary for Bril
====================================

Usage
-----

You will need `ghc` and `stack` installed to build this library. To build, run `stack build`. To run, invoke `stack run -- [args]` The executable accepts Bril in JSON format through `stdin`, performs optimizations, and writes the optimized bril to `stdout`. Currently available options:

### `--dce`

Perform dead code elimination

### `--tdce`

Perform local and global trivial dead code elimination, iterated to convergence.

### `--lvn`

Perform local value number with support for algebraic identities, constant propagation, copy propagation, and constant folding.

### `--ssa`

Roundtrip SSA conversion
