@rcplane and @zachary-kent worked together

# Summary
We successfully captured, optimized, and measured dynamic instruction counts for interpreted bril programs.

# Implementation
- We [modified brili.ts](https://github.com/rcplane/bril/blob/l12/brili.ts) to:
  - accept optional argument to capture traces to a file
  - Like TraceMonkey, we wanted to trace hot inner loop bodies. That is, we wanted to begin tracing after traversing a back-edge in the CFG. However, without precise dominators information in the interpreter, we would have had to re-implement dominator computation in typescript (or implement a Bril interpreter in Haskell) to gain this information. As a heuristic, we decided that branches "upwards" in the program are likely to be back edges, so we begin tracing when traversing an upwards jump or branch. We stop recording once we traverse another back edge, as this means we have likely traced the entire loop body. For every function in the program, we maintain a mapping between labels and traces beginning at those labels. If, during tracing, we execute a label found in this mapping, this means that we are likely
  tracing an outer loop, so we abort; we reasoned that tracing hot inner loops would be more fruitful, and managing nested traces is difficult. Further, if we encounter an instruction (like `print` or memory operations), we abort tracing. Building the trace itself using speculation was fairly straightforward and followed the recipe presented in class; the trace begins with a `speculate` instruction and ends with a `commit`, followed by an unconditional jump to the target of the final back edge in the trace. Jumps do not appear in this trace, and branches are translated into guards.
  - After profiling an entire Bril program, we have accumulated all the traces we want to stitch into that program.
    For every function in the program, we do the following:
    1) Insert an explicit return at the end of the function. We place our traces at the end of the function, so any implicit return would cause control flow to inadvertently fall into the traces.
    2) For every trace `t` starting at label `l` in this function, do the following:
      1) After label `l`, insert an unconditional jump to `l.trace`, the label where the instructions of the trace will be located.
      2) After this jump, insert a label `l.recover`. A failing guard in the trace will jump to `l.recover`.
      3) At the end of the function, insert the label `l.trace`. Then, insert all of the instructions in the trace `t`.
  - accept more arguments for multiple input sets, after creating an args file like [benchmarks/core/birthday.args](https://github.com/rcplane/bril/blob/l12/benchmarks/core/birthday.args) with one line per argument set, brili -i benchmarks/core/birthday.args will repeat the program evaluation for each input argument set reusing input bril program and other provided argument flags
- Leveraged Haskell bril-hs dce and lvn to optimize traces. We already had an effective implementation of LVN and DCE in Haskell, and applying them to the stitched program to optimize every trace.

# Tests and Results
- initial tests with benchmarks/core/gcd.bril and mem/mat-mul.bril using test/l12/trace.sh
- [compare.sh script](https://github.com/rcplane/bril/blob/l12/test/l12/compare.sh) and [raw compare output](https://github.com/rcplane/bril/blob/l12/test/l12/compare.out) shows correct outputs running benchmark default and from emitted stitched trace bril json.
- Unfortunately, we found that the overhead due to tracing was large. Although LVN was in _theory_ effective, upon examining the traces produced we found that they contained few additional opportunities for copy/constant propagation.
- Even if the code always stays "on trace", there is still the overhead present from jumping to the trace and then back out.
- In the future, we would likely want to support the "trace trees" described in TraceMonkey to allow for nested traces and hot "side exits".

| Benchmark Name | Input Value(s) | Results | Dyn Instr Count Default | Dyn Instr Count Default + DCE + LVN | Dyn Instr Count from Trace + DCE + LVN |
|----------------|----------------|---------|-----------|------------------|-------------------|
| birthday       | 23 traced      | match   | 484     | **254** '*' | 325                   |
| birthday       | 22             | match   | 463     | **243** '*' | 311                   |
| birthday       | 50             | match   | 1051    | **551** '*' | 703                   |
| gcd            | 4 20 traced    | match   | **46**  '*' | **46**  '*' | 63                    |
| gcd            | 4 80           | match   | **181** '*' | **181** '*' | 243                   |
| gcd            | 3 21           | match   | **64**  '*' | **64**  '*' | 87                    |
| perfect        | 496 traced     | match   | 232     | **231** '*' | 371                   |
| perfect        | 28             | match   | 58      | **57**  '*' | 98                    |
| perfect        | 12             | match   | 37      | **36**  '*' | 67                    |
 

# Difficulties
- We initially started development on a simpler form of brili.ts trace logging with a procedural context planning to stitch traces later with python but then adapted to a tracing scheme triggered on loop back edges aiming to optimize for concentration of branching or loopins control flow.  This changed our interpreter tracing conditions, and while working further in typescript we decided that simply stitching and emitted full trace files at once made the most sense for ease of use.
- We initially attempted memory heap copy upon speculation but ran into difficulties with parital incorrect output of mat-mul even on same traced input.  As this was out of scope for the assignment we decided to focus on core tracing features and proof of correctness instead.



# Generative AI
- Both of us used GitHub Copilot throughout the task, which was fairly helpful, especially for generating documentation. For example, the following spec was generated using Copilot:
  ```
  /**
  * Stitch all traces of `state.traceMap` into `prog`, modifying `prog` in place.
  * 
  * @param prog The program to modify
  * @param state the state produced by executing `prog`
  */
  ```
- Copilot was also able to generate the implementation of very simple functions, such as the following:
  ```
  const addTracePostfix = (label: string) => `${label}.trace`;
  ```
  It faired worse, however, on more complex code (like the `stitch` function). Its suggestions were frequently incorrect.
- We used ChatGPT to figure out how to run a `stack` executable from a different directory; it told us you can use the `--stack-yaml` option to specify a path to the project, which was correct.

- Chat GPT4 April 2023 was used for Deno typescript function formatting and I/O function call advice.
