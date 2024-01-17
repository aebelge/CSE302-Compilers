#! /usr/bin/env python3

# --------------------------------------------------------------------
# Requires Python3 >= 3.10

# --------------------------------------------------------------------
import argparse
import os
import subprocess as sp
import sys

from bxlib.bxast        import *
from bxlib.bxerrors     import Reporter, DefaultReporter
from bxlib.bxparser     import Parser
from bxlib.bxmm         import MM
from bxlib.bxtychecker  import check as tycheck
from bxlib.bxasmgen     import AsmGen
from bxlib.bxtac        import *
from bxlib.bxcfg        import tac2cfg, cfg2tac, uce, jthreading

# ====================================================================
# Parse command line arguments

def parse_args():
    parser = argparse.ArgumentParser(prog = os.path.basename(sys.argv[0]))

    parser.add_argument('input', help = 'input file (.bx)')

    aout = parser.parse_args()

    if os.path.splitext(aout.input)[1].lower() != '.bx':
        parser.error('input filename must end with the .bx extension')

    return aout

# ====================================================================
# Main entry point

def _main():
    args = parse_args()

    try:
        with open(args.input, 'r') as stream:
            prgm = stream.read()

    except IOError as e:
        print(f'cannot read input file {args.input}: {e}')
        exit(1)

    reporter = DefaultReporter(source = prgm)
    prgm = Parser(reporter = reporter).parse(prgm)

    if prgm is None:
        exit(1)

    if not tycheck(prgm, reporter = reporter):
        exit(1)

    tac = MM.mm(prgm)

    for decl in tac:
        match decl:
            case TACProc(tac = ptac):
                # We here do TAC -> CFG -> JTHREADING -> UCE -> TAC
                # Other CFG-based optimizations should be inserted here
                decl.tac = cfg2tac(uce(jthreading(tac2cfg(ptac))))

    abk = AsmGen.get_backend('x64-linux')
    asm = abk.lower(tac)

    basename = os.path.splitext(args.input)[0]
    basename = os.path.basename(basename)

    try:
        with open(f'{basename}.s', 'w') as stream:
            stream.write(asm)

    except IOError as e:
        print(f'cannot write outpout file {args.output}: {e}')
        exit(1)

    bxruntime = os.path.join(os.path.dirname(__file__), 'bxlib', 'bxruntime.c')

    sp.call(['C:\\msys64\\mingw64\\bin\\gcc.exe', '-g', '-c', '-o', f'{basename}.o', f'{basename}.s'])
    sp.call(['C:\\msys64\\mingw64\\bin\\gcc.exe', '-g', '-o', f'{basename}.exe', bxruntime, f'{basename}.o'])



# --------------------------------------------------------------------
if __name__ == '__main__':
    _main()
