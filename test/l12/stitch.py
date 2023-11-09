# accept benchmark name, traced function name to stitch in to, and input original benchmark file bril json
# output stitched bril json with function modified to speculate fast trace path when possible via per benchmark special case guards

import sys
import json
import argparse
import os

def stitch(benchmark_name, traced_function, bril):
    """stitch in fast path speculation for traced function in bril json, print modified bril json to stdout"""
    res_bril = ""
    if benchmark_name == "gcd":
        assert(traced_function == "main")
        res_bril = stitch_gcd(traced_function, bril)
    elif benchmark_name == "matmul":
        assert(traced_function == "matmul")
        res_bril = stitch_matmul(traced_function, bril)
    else:
        print(json.dumps(bril)) # print out unmodified function bril json
    print(json.dumps(res_bril))

def main():
    # parse arguments
    argparse = argparse.ArgumentParser()
    argparse.add_argument("benchmark_name", help="benchmark name")
    argparse.add_argument("traced_function", help="traced function name")
    args = argparse.parse_args()

    # read bril json from stdin
    bril = json.load(sys.stdin)

    # emit bril until traced function to modify
    for func in bril["functions"]:
        if func["name"] == args.traced_function:
            stitch(benchmark_name=args.benchmark_name, traced_function=args.traced_function, bril=func)
        print(json.dumps(func)) 

if __name__ == "__main__":
    sys.exit(1) # incomplete
    main()
