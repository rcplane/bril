
import { pushIfUnique } from 'https://esm.sh/typescript@5.0.4';
import * as bril from './bril-ts/bril.ts';
import { readStdin, unreachable } from './bril-ts/util.ts';

/**
 * An interpreter error to print to the console.
 */
class BriliError extends Error {
  constructor(message?: string) {
    super(message);
    Object.setPrototypeOf(this, new.target.prototype);
    this.name = BriliError.name;
  }
}

/**
 * Create an interpreter error object to throw.
 */
function error(message: string): BriliError {
  return new BriliError(message);
}

/**
 * An abstract key class used to access the heap.
 * This allows for "pointer arithmetic" on keys,
 * while still allowing lookups based on the based pointer of each allocation.
 */
export class Key {
    readonly base: number;
    readonly offset: number;

    constructor(b: number, o: number) {
        this.base = b;
        this.offset = o;
    }

    add(offset: number) {
        return new Key(this.base, this.offset + offset);
    }
}

/**
 * A Heap maps Keys to arrays of a given type.
 */
export class Heap<X> {
    private readonly storage: Map<number, X[]>
    constructor(other?: Heap<X> | undefined) {
        if (other) {
            this.storage = structuredClone(other.storage);
        } else {
            this.storage = new Map();
        }
    }

    isEmpty(): boolean {
        return this.storage.size == 0;
    }

    private count = 0;
    private getNewBase():number {
        const val = this.count;
        this.count++;
        return val;
    }

    private freeKey(key:Key) {
      return;
    }

    alloc(amt:number): Key {
        if (amt <= 0) {
            throw error(`cannot allocate ${amt} entries`);
        }
        const base = this.getNewBase();
        this.storage.set(base, new Array(amt))
        return new Key(base, 0);
    }

    free(key: Key) {
        if (this.storage.has(key.base) && key.offset == 0) {
            this.freeKey(key);
            this.storage.delete(key.base);
        } else {
            throw error(`Tried to free illegal memory location base: ${key.base}, offset: ${key.offset}. Offset must be 0.`);
        }
    }

    write(key: Key, val: X) {
        const data = this.storage.get(key.base);
        if (data && data.length > key.offset && key.offset >= 0) {
            data[key.offset] = val;
        } else {
            throw error(`Uninitialized heap location ${key.base} and/or illegal offset ${key.offset}`);
        }
    }

    read(key: Key): X {
        const data = this.storage.get(key.base);
        if (data && data.length > key.offset && key.offset >= 0) {
            return data[key.offset];
        } else {
            throw error(`Uninitialized heap location ${key.base} and/or illegal offset ${key.offset}`);
        }
    }
}

const argCounts: {[key in bril.OpCode]: number | null} = {
  add: 2,
  mul: 2,
  sub: 2,
  div: 2,
  id: 1,
  lt: 2,
  le: 2,
  gt: 2,
  ge: 2,
  eq: 2,
  not: 1,
  and: 2,
  or: 2,
  fadd: 2,
  fmul: 2,
  fsub: 2,
  fdiv: 2,
  flt: 2,
  fle: 2,
  fgt: 2,
  fge: 2,
  feq: 2,
  print: null,  // Any number of arguments.
  br: 1,
  jmp: 0,
  ret: null,  // (Should be 0 or 1.)
  nop: 0,
  call: null,
  alloc: 1,
  free: 1,
  store: 2,
  load: 1,
  ptradd: 2,
  phi: null,
  speculate: 0,
  guard: 1,
  commit: 0,
  ceq: 2,
  clt: 2,
  cle: 2,
  cgt: 2,
  cge: 2,
  char2int: 1,
  int2char: 1,
};

type Pointer = {
  loc: Key;
  type: bril.Type;
}

type Value = boolean | bigint | Pointer | number | string;
type Env = Map<bril.Ident, Value>;

/**
 * Check whether a run-time value matches the given static type.
 */
function typeCheck(val: Value, typ: bril.Type): boolean {
  if (typ === "int") {
    return typeof val === "bigint";
  } else if (typ === "bool") {
    return typeof val === "boolean";
  } else if (typ === "float") {
    return typeof val === "number";
  } else if (typeof typ === "object" && Object.prototype.hasOwnProperty.call(typ, "ptr")) {
    return Object.prototype.hasOwnProperty.call(val, "loc");
  } else if (typ === "char") {
    return typeof val === "string";
  }
  throw error(`unknown type ${typ}`);
}

/**
 * Check whether the types are equal.
 */
function typeCmp(lhs: bril.Type, rhs: bril.Type): boolean {
  if (lhs === "int" || lhs == "bool" || lhs == "float" || lhs == "char") {
    return lhs == rhs;
  } else {
    if (typeof rhs === "object" && Object.prototype.hasOwnProperty.call(rhs, "ptr")) {
      return typeCmp(lhs.ptr, rhs.ptr);
    } else {
      return false;
    }
  }
}

function get(env: Env, ident: bril.Ident) {
  const val = env.get(ident);
  if (typeof val === 'undefined') {
    throw error(`undefined variable ${ident}`);
  }
  return val;
}

function findFunc(func: bril.Ident, funcs: readonly bril.Function[]) {
  const matches = funcs.filter(function (f: bril.Function) {
    return f.name === func;
  });

  if (matches.length == 0) {
    throw error(`no function of name ${func} found`);
  } else if (matches.length > 1) {
    throw error(`multiple functions of name ${func} found`);
  }

  return matches[0];
}

function alloc(ptrType: bril.ParamType, amt:number, heap:Heap<Value>): Pointer {
  if (typeof ptrType != 'object') {
    throw error(`unspecified pointer type ${ptrType}`);
  } else if (amt <= 0) {
    throw error(`must allocate a positive amount of memory: ${amt} <= 0`);
  } else {
    const loc = heap.alloc(amt)
    const dataType = ptrType.ptr;
    return {
      loc: loc,
      type: dataType
    }
  }
}

/**
 * Ensure that the instruction has exactly `count` arguments,
 * throw an exception otherwise.
 */
function checkArgs(instr: bril.Operation, count: number) {
  const found = instr.args ? instr.args.length : 0;
  if (found != count) {
    throw error(`${instr.op} takes ${count} argument(s); got ${found}`);
  }
}

function getPtr(instr: bril.Operation, env: Env, index: number): Pointer {
  const val = getArgument(instr, env, index);
  if (typeof val !== 'object' || val instanceof BigInt) {
    throw `${instr.op} argument ${index} must be a Pointer`;
  }
  return val;
}

function getArgument(instr: bril.Operation, env: Env, index: number, typ?: bril.Type) {
  const args = instr.args || [];
  if (args.length <= index) {
    throw error(`${instr.op} expected at least ${index+1} arguments; got ${args.length}`);
  }
  const val = get(env, args[index]);
  if (typ && !typeCheck(val, typ)) {
    throw error(`${instr.op} argument ${index} must be a ${typ}`);
  }
  return val;
}

function getInt(instr: bril.Operation, env: Env, index: number): bigint {
  return getArgument(instr, env, index, 'int') as bigint;
}

function getBool(instr: bril.Operation, env: Env, index: number): boolean {
  return getArgument(instr, env, index, 'bool') as boolean;
}

function getFloat(instr: bril.Operation, env: Env, index: number): number {
  return getArgument(instr, env, index, 'float') as number;
}

function getChar(instr: bril.Operation, env: Env, index: number): string {
  return getArgument(instr, env, index, 'char') as string;
}

function getLabel(instr: bril.Operation, index: number): bril.Ident {
  if (!instr.labels) {
    throw error(`missing labels; expected at least ${index+1}`);
  }
  if (instr.labels.length <= index) {
    throw error(`expecting ${index+1} labels; found ${instr.labels.length}`);
  }
  return instr.labels[index];
}

function getFunc(instr: bril.Operation, index: number): bril.Ident {
  if (!instr.funcs) {
    throw error(`missing functions; expected at least ${index+1}`);
  }
  if (instr.funcs.length <= index) {
    throw error(`expecting ${index+1} functions; found ${instr.funcs.length}`);
  }
  return instr.funcs[index];
}

/**
 * The thing to do after interpreting an instruction: this is how `evalInstr`
 * communicates control-flow actions back to the top-level interpreter loop.
 */
type Action =
  {"action": "next"} |  // Normal execution: just proceed to next instruction.
  {"action": "jump", "label": bril.Ident} |
  {"action": "end", "ret": Value | null} |
  {"action": "speculate"} |
  {"action": "commit"} |
  {"action": "abort", "label": bril.Ident};
const NEXT: Action = {"action": "next"};

/**
 * The interpreter state that's threaded through recursive calls.
 */
type State = {
  env: Env,
  heap: Heap<Value>,
  readonly funcs: readonly bril.Function[],

  // For profiling: a total count of the number of instructions executed.
  icount: bigint,

  // For Tracing:
  // file to write trace to, otherwise logs to console
  tracingFile: string,
  // buffer to store trace json string, emitted at end of function
  // if it gets too large, we stop capturing the trace
  traceBuffer: bril.Instruction[],
  // whether we are currently tracing
  tracing: boolean,
  // The label we are currently tracing from
  headerLabel: string | null,
  // whether to emit the captured trace.
  emitTrace: boolean,

  // TraceMap a mapping from functions to traced starting labels
  traceMap: Map<string, { headerLabel: string, trace: bril.Instruction[] }[]>,
  
  // For SSA (phi-node) execution: keep track of recently-seen labels.j
  curlabel: string | null,
  lastlabel: string | null,

  // For speculation: the state at the point where speculation began.
  specparent: State | null,
}

// Abort recording of the current trace
const abortTracing = (state: State) => {
  state.traceBuffer = [];
  state.tracing = false;
  state.headerLabel = null;
}

/**
 * Interpet a call instruction.
 */
function evalCall(instr: bril.Operation, state: State): Action {
  // Which function are we calling?
  const funcName = getFunc(instr, 0);
  const func = findFunc(funcName, state.funcs);
  if (func === null) {
    throw error(`undefined function ${funcName}`);
  }

  const newEnv: Env = new Map();

  // Check arity of arguments and definition.
  const params = func.args || [];
  const args = instr.args || [];
  if (params.length !== args.length) {
    throw error(`function expected ${params.length} arguments, got ${args.length}`);
  }

  for (let i = 0; i < params.length; i++) {
    // Look up the variable in the current (calling) environment.
    const value = get(state.env, args[i]);

    // Check argument types
    if (!typeCheck(value, params[i].type)) {
      throw error(`function argument type mismatch`);
    }

    // Set the value of the arg in the new (function) environment.
    newEnv.set(params[i].name, value);
  }

  // TODO can we also memoize using argument and result traces to avoid exact repeat computations?
  // Invoke the interpreter on the function.
  const newState: State = {
    env: newEnv,
    heap: state.heap,
    funcs: state.funcs,
    icount: state.icount,
    tracingFile: state.tracingFile,
    traceBuffer: state.traceBuffer,
    tracing: state.tracing,  // TODO can we re-enable tracing after buffer size exceeded, function white list?
    headerLabel: state.headerLabel,
    emitTrace: state.emitTrace,
    traceMap: state.traceMap,
    lastlabel: null,
    curlabel: null,
    specparent: null,  // Speculation not allowed.
  }
  const retVal = evalFunc(func, newState);
  state.icount = newState.icount;

  emitTrace(state, funcName);

  // Dynamically check the function's return value and type.
  if (!('dest' in instr)) {  // `instr` is an `EffectOperation`.
     // Expected void function
    if (retVal !== null) {
      throw error(`unexpected value returned without destination`);
    }
    if (func.type !== undefined) {
      throw error(`non-void function (type: ${func.type}) doesn't return anything`);
    }
  } else {  // `instr` is a `ValueOperation`.
    // Expected non-void function
    if (instr.type === undefined) {
      throw error(`function call must include a type if it has a destination`);
    }
    if (instr.dest === undefined) {
      throw error(`function call must include a destination if it has a type`);
    }
    if (retVal === null) {
      throw error(`non-void function (type: ${func.type}) doesn't return anything`);
    }
    if (!typeCheck(retVal, instr.type)) {
      throw error(`type of value returned by function does not match destination type`);
    }
    if (func.type === undefined) {
      throw error(`function with void return type used in value call`);
    }
    if (!typeCmp(instr.type, func.type)) {
      throw error(`type of value returned by function does not match declaration`);
    }
    state.env.set(instr.dest, retVal);
  }
  return NEXT;
}

/**
 * Uniques each local variable by adding th enclosing function name to the end of 
 * the variable name. For example, a variable `x` in function `f` becomes `x.f`.
 */
const uniqueLocals = ({ functions }: bril.Program) => {
  for (const func of functions) {
    for (const arg of func.args ?? []) {
      arg.name += `.${func.name}`;
    }
    for (const instr of func.instrs) {
      if ('dest' in instr) {
        instr.dest += `.${func.name}`;
      }
      if ('args' in instr) {
        instr.args = instr.args?.map(arg => `${arg}.${func.name}`) ?? [];
      }
    }
  }
}

const addRecoveryPostfix = (label: string) => `${label}.recover`;

/**
 * Interpret an instruction in a given environment, possibly updating the
 * environment. If the instruction branches to a new label, return that label;
 * otherwise, return "next" to indicate that we should proceed to the next
 * instruction or "end" to terminate the function.
 */
function evalInstr(instr: bril.Instruction, state: State): Action {
  //console.log(JSON.stringify(instr)); // instead conditionally emit trace at end of function
  state.icount++;

  // Check that we have the right number of arguments.
  if (instr.op !== "const") {
    const count = argCounts[instr.op];
    if (count === undefined) {
      throw error("unknown opcode " + instr.op);
    } else if (count !== null) {
      checkArgs(instr, count);
    }
  }

  // Function calls are not (currently) supported during speculation.
  // It would be cool to add, but aborting from inside a function call
  // would require explicit stack management.
  if (state.specparent && ['call', 'ret'].includes(instr.op)) {
    throw error(`${instr.op} not allowed during speculation`);
  }

  // call and ret are never emitted to streamline interprocedural trace inlining
  if (state.tracing && instr.op !== 'call' && instr.op !== 'ret') {
    //console.error(`Capturing trace for ${instr.op} instruction`);
    const encoder = new TextEncoder();
    Deno.stderr.write(encoder.encode(`${instr.op} `));
    state.traceBuffer.push(instr);
    if (BigInt(state.icount) % BigInt(100) === BigInt(0)) {
     if (state.traceBuffer.length > 10000) {
       // trace too large, stop capturing and reset
       state.traceBuffer = [];
       state.tracing = false;
       state.emitTrace = false;
       console.error('Trace buffer size 10000 instructions exceeded, disabling tracing for remainder of program');
      }
    }

        if (instr.op === 'br' || instr.op === 'jmp') {
          if (instr.op === 'br') {
            // generate appropriate guards based on the condition
            const conditionValue = getBool(instr, state.env, 0);
            if (state.headerLabel === null) {
              throw new Error("guard label null");
            }
            const recoveryLabel = addRecoveryPostfix(state.headerLabel);
            if (conditionValue) {
              state.traceBuffer.push({
                op: "guard",
                args: instr.args, 
                labels: [recoveryLabel]
              });
            } else {
              const [ condition ] = instr.args!;
              const dest = `${condition}.negated`;
              // negate the branch condition
              state.traceBuffer.push({
                args: instr.args,
                dest,
                op: "not", 
                type: "bool"
              });
              state.traceBuffer.push({
                args: [dest], 
                labels: [recoveryLabel], 
                op: "guard"
              });
            }
          }
        // jmp is not conditional , we'll just capture it in the trace
        }
    }
    

  switch (instr.op) {
  case "const": {
    // Interpret JSON numbers as either ints or floats.
    let value: Value;
    if (typeof instr.value === "number") {
      if (instr.type === "float")
        value = instr.value;
      else
        value = BigInt(Math.floor(instr.value))
    } else if (typeof instr.value === "string") {
      if([...instr.value].length !== 1) throw error(`char must have one character`);
      value = instr.value;
    } else {
      value = instr.value;
    }
    state.env.set(instr.dest, value);
    return NEXT;
  }

  case "id": {
    const val = getArgument(instr, state.env, 0);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "add": {
    let val = getInt(instr, state.env, 0) + getInt(instr, state.env, 1);
    val = BigInt.asIntN(64, val);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "mul": {
    let val = getInt(instr, state.env, 0) * getInt(instr, state.env, 1);
    val = BigInt.asIntN(64, val);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "sub": {
    let val = getInt(instr, state.env, 0) - getInt(instr, state.env, 1);
    val = BigInt.asIntN(64, val);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "div": {
    const lhs = getInt(instr, state.env, 0);
    const rhs = getInt(instr, state.env, 1);
    if (rhs === BigInt(0)) {
      throw error(`division by zero`);
    }
    let val = lhs / rhs;
    val = BigInt.asIntN(64, val);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "le": {
    const val = getInt(instr, state.env, 0) <= getInt(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "lt": {
    const val = getInt(instr, state.env, 0) < getInt(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "gt": {
    const val = getInt(instr, state.env, 0) > getInt(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "ge": {
    const val = getInt(instr, state.env, 0) >= getInt(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "eq": {
    const val = getInt(instr, state.env, 0) === getInt(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "not": {
    const val = !getBool(instr, state.env, 0);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "and": {
    const val = getBool(instr, state.env, 0) && getBool(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "or": {
    const val = getBool(instr, state.env, 0) || getBool(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fadd": {
    const val = getFloat(instr, state.env, 0) + getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fsub": {
    const val = getFloat(instr, state.env, 0) - getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fmul": {
    const val = getFloat(instr, state.env, 0) * getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fdiv": {
    const val = getFloat(instr, state.env, 0) / getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fle": {
    const val = getFloat(instr, state.env, 0) <= getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "flt": {
    const val = getFloat(instr, state.env, 0) < getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fgt": {
    const val = getFloat(instr, state.env, 0) > getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "fge": {
    const val = getFloat(instr, state.env, 0) >= getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "feq": {
    const val = getFloat(instr, state.env, 0) === getFloat(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "print": {
    // Can't handle side effects in speculative state.
    abortTracing(state);
    const args = instr.args || [];
    const values = args.map(function (i) {
      const val = get(state.env, i);
      if (Object.is(-0, val)) { return "-0.00000000000000000" }
      if (typeof val == "number") { return val.toFixed(17) } else {return val.toString()}}
    );
    console.log(...values);
    return NEXT;
  }

  case "jmp": {
    return {"action": "jump", "label": getLabel(instr, 0)};
  }

  case "br": {
    const cond = getBool(instr, state.env, 0);
    if (cond) {
      return {"action": "jump", "label": getLabel(instr, 0)};
    } else {
      return {"action": "jump", "label": getLabel(instr, 1)};
    }
  }

  case "ret": {
    const args = instr.args || [];
    if (args.length == 0) {
      return {"action": "end", "ret": null};
    } else if (args.length == 1) {
      const val = get(state.env, args[0]);
      return {"action": "end", "ret": val};
    } else {
      throw error(`ret takes 0 or 1 argument(s); got ${args.length}`);
    }
  }

  case "nop": {
    return NEXT;
  }

  case "call": {
    // TODO: implement interprocedural tracing
    abortTracing(state);
    return evalCall(instr, state);
  }

  case "alloc": {
    const amt = getInt(instr, state.env, 0);
    const typ = instr.type;
    if (!(typeof typ === "object" && Object.prototype.hasOwnProperty.call(typ, 'ptr'))) {
      throw error(`cannot allocate non-pointer type ${instr.type}`);
    }
    const ptr = alloc(typ, Number(amt), state.heap);
    state.env.set(instr.dest, ptr);
    return NEXT;
  }

  case "free": {
    const val = getPtr(instr, state.env, 0);
    state.heap.free(val.loc);
    return NEXT;
  }

  case "store": {
    const target = getPtr(instr, state.env, 0);
    state.heap.write(target.loc, getArgument(instr, state.env, 1, target.type));
    return NEXT;
  }

  case "load": {
    const ptr = getPtr(instr, state.env, 0);
    const val = state.heap.read(ptr.loc);
    if (val === undefined || val === null) {
      throw error(`Pointer ${instr.args![0]} points to uninitialized data`);
    } else {
      state.env.set(instr.dest, val);
    }
    return NEXT;
  }

  case "ptradd": {
    const ptr = getPtr(instr, state.env, 0)
    const val = getInt(instr, state.env, 1)
    state.env.set(instr.dest, { loc: ptr.loc.add(Number(val)), type: ptr.type })
    return NEXT;
  }

  case "phi": {
    const labels = instr.labels || [];
    const args = instr.args || [];
    if (labels.length != args.length) {
      throw error(`phi node has unequal numbers of labels and args`);
    }
    if (!state.lastlabel) {
      throw error(`phi node executed with no last label`);
    }
    const idx = labels.indexOf(state.lastlabel);
    if (idx === -1) {
      // Last label not handled. Leave uninitialized.
      state.env.delete(instr.dest);
    } else {
      // Copy the right argument (including an undefined one).
      if (!instr.args || idx >= instr.args.length) {
        throw error(`phi node needed at least ${idx+1} arguments`);
      }
      const src = instr.args[idx];
      const val = state.env.get(src);
      if (val === undefined) {
        state.env.delete(instr.dest);
      } else {
        state.env.set(instr.dest, val);
      }
    }
    return NEXT;
  }

  // Begin speculation.
  case "speculate": {
    return {"action": "speculate"};
  }

  // Abort speculation if the condition is false.
  case "guard": {
    if (getBool(instr, state.env, 0)) {
      return NEXT;
    } else {
      return {"action": "abort", "label": getLabel(instr, 0)};
    }
  }

  // Resolve speculation, making speculative state real.
  case "commit": {
    return {"action": "commit"};
  }

  case "ceq": {
    const val = getChar(instr, state.env, 0) === getChar(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "clt": {
    const val = getChar(instr, state.env, 0) < getChar(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "cle": {
    const val = getChar(instr, state.env, 0) <= getChar(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "cgt": {
    const val = getChar(instr, state.env, 0) > getChar(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "cge": {
    const val = getChar(instr, state.env, 0) >= getChar(instr, state.env, 1);
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "char2int": {
    const code = getChar(instr, state.env, 0).codePointAt(0);
    const val = BigInt.asIntN(64, BigInt(code as number));
    state.env.set(instr.dest, val);
    return NEXT;
  }

  case "int2char": {
    const i = getInt(instr, state.env, 0);
    if (i > 1114111 || i < 0 || (55295 < i && i < 57344)) {
      throw error(`value ${i} cannot be converted to char`);
    }
    const val = String.fromCodePoint(Number(i));
    state.env.set(instr.dest, val);
    return NEXT;
  }

  }
  unreachable(instr);
  throw error(`unhandled opcode ${(instr as any).op}`);
}

function evalFunc(func: bril.Function, state: State): Value | null {
  for (let i = 0; i < func.instrs.length; ++i) {
    const line = func.instrs[i];
    if ('op' in line) {
      // Run an instruction.
      const action = evalInstr(line, state);
      // Take the prescribed action.
      switch (action.action) {
      case 'end': {
        // Return from this function.
        return action.ret;
      }
      case 'speculate': {
        // Begin speculation.
        state.specparent = {...state};
        state.env = new Map(state.env);
        state.heap = new Heap(state.heap);
        break;
      }
      case 'commit': {
        // Resolve speculation.
        if (!state.specparent) {
          throw error(`commit in non-speculative state`);
        }
        state.specparent = null;
        break;
      }
      case 'abort': {
        // Restore state.
        if (!state.specparent) {
          throw error(`abort in non-speculative state`);
        }
        // We do *not* restore `icount` from the saved state to ensure that we
        // count "aborted" instructions.
        Object.assign(state, {
          env: state.specparent.env,
          heap: state.specparent.heap,
          lastlabel: state.specparent.lastlabel,
          curlabel: state.specparent.curlabel,
          specparent: state.specparent.specparent,
        });
        break;
      }
      case 'next':
      case 'jump':
        break;
      default:
        unreachable(action);
        throw error(`unhandled action ${(action as any).action}`);
      }
      // Move to a label.
      if ('label' in action) {
        const currentIndex = i;
        // Search for the label and transfer control.
        for (i = 0; i < func.instrs.length; ++i) {
          const sLine = func.instrs[i];
          if ('label' in sLine && sLine.label === action.label) {
            --i;  // Execute the label next.
            // if jmping upward in instruction count (decreasing) then start or stop tracing
            if (i < currentIndex) {
              if (!state.tracing) {
                // begin tracing
                state.headerLabel = action.label; //= ".recover"; //l_recover
                // TODO stitch on // state.traceBuffer.push({"label": action.label + ".trace"});
                state.traceBuffer.push({"op":"speculate"}) ;
              } else {
                if (state.headerLabel === null) {
                  throw new Error("Error: header label null when emitting trace");
                }
                // We have traversed another back edge, hopefully to the loop header
                // When the heursistic is correct, this trace should comprise the loop body
                // Commit the stateful updates performed by the loop body
                state.traceBuffer.push({ op: "commit" });
                // After executing the loop body, we should be back to the target of the back edge
                state.traceBuffer.push({ op: "jmp", labels: [action.label] }); // l2, may equal l1 the headerLabel
                state.traceMap.get(func.name)?.push({ 
                  headerLabel: state.headerLabel, 
                  trace: state.traceBuffer
                });     
                state.traceBuffer = [];
              }
              state.tracing = !state.tracing;
            }
            break;
          }
        }
        if (i === func.instrs.length) {
          throw error(`label ${action.label} not found`);
        }
      }
    } else if ('label' in line) {
      // Update CFG tracking for SSA phi nodes.
      state.lastlabel = state.curlabel;
      state.curlabel = line.label;
    }
  }

  // Reached the end of the function without hitting `ret`.
  if (state.specparent) {
    throw error(`implicit return in speculative state`);
  }

  emitTrace(state, func.name);

  return null;
}

function parseChar(s: string): string {
  const c = s;
  if ([...c].length === 1) {
    return c;
  } else {
    throw error(`char argument to main must have one character; got ${s}`);
  }
}

function parseBool(s: string): boolean {
  if (s === 'true') {
    return true;
  } else if (s === 'false') {
    return false;
  } else {
    throw error(`boolean argument to main must be 'true'/'false'; got ${s}`);
  }
}

function parseNumber(s: string): number {
  const f = parseFloat(s);
  // parseFloat and Number have subtly different behaviors for parsing strings
    // parseFloat ignores all random garbage after any valid number
    // Number accepts empty/whitespace only strings and rejects numbers with seperators
  // Use both and only accept the intersection of the results?
  const f2 = Number(s);
  if (!isNaN(f) && f === f2) {
    return f;
  } else {
    throw error(`float argument to main must not be 'NaN'; got ${s}`);
  }
}

function parseMainArguments(expected: bril.Argument[], args: string[]) : Env {
  const newEnv: Env = new Map();
  if (args.length !== expected.length) {
    throw error(`mismatched main argument arity: expected ${expected.length}; got ${args.length}`);
  }
  for (let i = 0; i < args.length; i++) {
    const type = expected[i].type;
    switch (type) {
      case "int": {
        const n = BigInt(Number(args[i]));
        newEnv.set(expected[i].name, n as Value);
        break;
      }
      case "float": {
        const f: number = parseNumber(args[i]);
        newEnv.set(expected[i].name, f as Value);
        break;
      }
      case "bool": {
        const b: boolean = parseBool(args[i]);
        newEnv.set(expected[i].name, b as Value);
        break;
      }
      case "char": {
        const c: string = parseChar(args[i]);
        newEnv.set(expected[i].name, c as Value);
        break;
      }
    }
  }
  return newEnv;
}

function evalProg(prog: bril.Program) {
  const heap = new Heap<Value>()
  const main = findFunc("main", prog.functions);
  if (main === null) {
    console.warn(`no main function defined, doing nothing`);
    return;
  }

  // Parse command line arguments
  const args: string[] = Array.from(Deno.args);
  
  // Check for -p flag to enable optional dynamic instruction count profiling
  let profiling = false;
  if (args.includes('-p')) {
    profiling = true;
    args.splice(args.indexOf('-p'), 1); // remove after handling
  }

  // Check for -t flag to specify tracing file or -t without argument to log to console
  let tracingFile = "";
  let captureTrace = false;
  const tracingIndex = args.indexOf('-t');
  if (tracingIndex > -1) {
    console.error("should trace");
    captureTrace = true;
    tracingFile = args[tracingIndex + 1];
    if (!tracingFile) {
      //tracingFile = "";
      //args.splice(tracingIndex, 1);
      throw new Error('Tracing file option given but path not provided');
    } else {
      // remove -t and tracing file path from args
      args.splice(tracingIndex, 2);
    }
    console.error(`Tracing to file ${tracingFile}`);
    //test write to trace file 
    //Deno.writeTextFileSync(tracingFile, "test\n");
    //console.error(`args remaining: ${args}`);
  }

  // Allow inline args for backward compatibility, or arguments from a -i file one set per line, not both
  const expected = main.args || [];
  // Check for -i flag to specify input file
  const inputIndex = args.indexOf('-i');
  if (inputIndex > -1) {
    const inputFile = args[inputIndex + 1];
    if (!inputFile) {
      throw new Error('Input main function args per line file option given but path not provided');
    }
    Deno.readTextFile(inputFile).then((input) => {
      const inputLines = input.trim().split('\n');
      inputLines.forEach(argSet => {
        let newEnv: Env;
        if (inputIndex > -1) {
          const inputArgs = argSet.trim().split(/\s+/);
          newEnv = parseMainArguments(expected, inputArgs);
          const state: State = createState(prog.functions, heap, newEnv, tracingFile);
          evalFunc(main, state);
          checkHeap(heap);
          printProfiling(profiling, state.icount);
        }
      });
    }).catch((error) => {
      console.error(error);
    });
  } else {
    // Parse inline args remaining after flags as main bril function arguments per usual
    const newEnv = parseMainArguments(expected, args);
    const state: State = createState(prog.functions, heap, newEnv, tracingFile);
    evalFunc(main, state);
    checkHeap(heap);
    printProfiling(profiling, state.icount);
  }
}

function createState(funcs: bril.Function[], heap: Heap<Value>, env: Env, tracingFile: string): State {
  const traceMap = new Map();
  for (const func of funcs) {
    traceMap.set(func.name, []);
  }
  return {
    funcs,
    heap,
    env,
    icount: BigInt(0),
    traceMap,
    tracingFile: tracingFile,
    traceBuffer: [],
    tracing: false,
    headerLabel: null,
    emitTrace: false,
    lastlabel: null,
    curlabel: null,
    specparent: null,
  };
}

function checkHeap(heap: Heap<Value>): void {
  if (!heap.isEmpty()) {
    throw error(`Some memory locations have not been freed by end of execution.`);
  }
}

function printProfiling(profiling: boolean, icount: bigint): void {
  if (profiling) {
    console.error(`total_dyn_inst: ${icount}`);
  }
}

function emitTrace(state: State, funcName: string): void {
  if (state.tracing) {
    if (state.emitTrace) {
      // emit trace to tracing file or console
      console.log("tracefile is " + state.tracingFile);
      if (state.tracingFile != "") {
        const toWrite = JSON.stringify(`{TRACE_START:${funcName}},\n${state.traceBuffer.join('')},\n{TRACE_END:${funcName}},\n`);
        if(JSON.parse(toWrite)){
          Deno.writeTextFileSync(state.tracingFile!, toWrite);
        } else {
          throw new Error('Invalid trace JSON');
        }
      } else {
        console.log(JSON.stringify(`{TRACE_START:${funcName}},\n${state.traceBuffer.join('')},\n{TRACE_END:${funcName}},`));
      }
      console.error(`Emitted ${state.traceBuffer.length} trace lines for function ${funcName}`);
      state.traceBuffer = []; // reset trace buffer
    }
  }
}

async function main() {
  try {
    const prog = JSON.parse(await readStdin()) as bril.Program;
    uniqueLocals(prog);
    evalProg(prog);
  }
  catch(e) {
    if (e instanceof BriliError) {
      console.error(`error: ${e.message}`);
      Deno.exit(2);
    } else {
      throw e;
    }
  }
}

main();
