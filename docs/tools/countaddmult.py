import json
import sys

def count_operations(json_data):
    fadd_count = 0
    fmul_count = 0
    
    for function in json_data.get('functions', []):
        for instr in function.get('instrs', []):
            if instr.get('op') == 'fadd':
                fadd_count += 1
            elif instr.get('op') == 'fmul':
                fmul_count += 1
                
    return fadd_count, fmul_count

def main():
    if len(sys.argv) > 1:
        # Read JSON data from a file
        filename = sys.argv[1]
        with open(filename, 'r') as f:
            json_data = json.load(f)
    else:
        # Read JSON data from stdin
        json_data = json.load(sys.stdin)
        
    fadd_count, fmul_count = count_operations(json_data)
    
    print(f"Number of fadd operations: {fadd_count}")
    print(f"Number of fmul operations: {fmul_count}")

if __name__ == "__main__":
    main()
