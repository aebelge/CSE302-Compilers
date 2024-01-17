# --------------------------------------------------------------------
import dataclasses as dc

from typing import Optional as Opt

# ====================================================================
# Three-Address Code

OPCODES = {
    'opposite'            : 'neg',
    'addition'            : 'add',
    'subtraction'         : 'sub',
    'multiplication'      : 'mul',
    'division'            : 'div',
    'modulus'             : 'mod',
    'bitwise-negation'    : 'not',
    'bitwise-and'         : 'and',
    'bitwise-or'          :  'or',
    'bitwise-xor'         : 'xor',
    'logical-left-shift'  : 'shl',
    'logical-right-shift' : 'shr',
    'load'  : 'load',
    'store' : 'store',
}

# --------------------------------------------------------------------
@dc.dataclass
class TAC:
    opcode    : str
    arguments : list[str | int]
    result    : Opt[str | int] = None

    def tojson(self):
        return dict(
            opcode = self.opcode   ,
            args   = self.arguments,
            result = self.result   ,
        )

    def __repr__(self):
        aout = self.opcode
        if self.arguments:
            aout = f"{aout} {', '.join(map(repr, self.arguments))}"
        if self.result:
            aout = f"{self.result} = {aout}"
        return aout

# --------------------------------------------------------------------
class TACProc:
    __match_args__ = ('name', 'arguments', 'tac')

    def __init__(self, name: str, arguments: list[str]):
        self.name      = name
        self.arguments = arguments
        self.tac       = []
        self.temp_sizes = {}

    def add_temp_size(self, temp_name: str, bytesize: int):
        self.temp_sizes[temp_name] = bytesize

    def __repr__(self):
        aout = f"proc @{self.name}"
        if self.arguments:
            aout = f"{aout}({', '.join(map(repr, self.arguments))})"
        aout = [f"{aout}:"]
        for tac in self.tac:
            aout.append(f"    {tac};")
        return "\n".join(aout) + "\n"

# --------------------------------------------------------------------
class TACVar:
    __match_args__ = ('name', 'value')

    def __init__(self, name: str, value: int):
        self.name  = name
        self.value = value

    def __repr__(self):
        return f"var @{self.name} = {self.value};"
