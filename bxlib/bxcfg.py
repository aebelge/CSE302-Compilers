# --------------------------------------------------------------------
from .bxtac import *
from .bxmm  import MM

# --------------------------------------------------------------------
class CFGNode:
    def __init__(self):
        self.label  = None      # Basic block label
        self.body   = []        # Basic block body (excluding labels, (c)jumps & ret)
        self.cjumps = []        # conditional jumps
        self.jump   = None      # Final jump (if the block terminates with "ret", it is set to False)

# --------------------------------------------------------------------
class CFG:
    def __init__(self, init: str, cfg: dict[str, CFGNode]):
        self.init = init
        self.cfg  = cfg

# --------------------------------------------------------------------
def tac2cfg(tac : list[str | TAC]):
    blocks, i = [], 0

    while i < len(tac):
        blocks.append(CFGNode())

        if isinstance(tac[i], str):
            blocks[-1].label = tac[i][:-1]
            i += 1
        else:
            blocks[-1].label = MM.fresh_label()

        if len(blocks) > 1:
            if blocks[-2].jump is None:
                blocks[-2].jump = ('jmp', blocks[-1].label)

        while i < len(tac):
            if isinstance(tac[i], str):
                break

            itac = tac[i]; i += 1

            if itac.opcode == 'ret':
                blocks[-1].jump = ('ret', itac.arguments)
                break

            if itac.opcode == 'jmp':
                blocks[-1].jump = ('jmp', itac.arguments[0])
                break

            if itac.opcode in ('jz', 'jnz', 'jgt', 'jge', 'jlt', 'jle'):
                blocks[-1].cjumps.append((itac.opcode, itac.arguments))
                break

            blocks[-1].body.append(itac)

    if not blocks:
        blocks.append(CFGNode())
        blocks.label = MM.fresh_label()

    if blocks[-1].jump is None:
        blocks[-1].jump = ('ret', [])

    return CFG(blocks[0].label, { b.label: b for b in blocks })

# --------------------------------------------------------------------
def cfg2tac(cfg: CFG):
    tac, visited = [], set()

    def block2tac(name: str):
        if name in visited:
            return

        visited.add(name)
        node = cfg.cfg[name]

        tac.append(f'{node.label}:')
        tac.extend(node.body)

        for cjump, args in node.cjumps:
            tac.append(TAC(cjump, args))

        match node.jump[0]:
            case 'ret':
                tac.append(TAC('ret', node.jump[1]))

            case 'jmp':
                tac.append(TAC('jmp', [node.jump[1]]))

            case _:
                assert(False)

        if node.jump[0] == 'jmp':
            if node.jump[1] not in visited:
                tac.pop()
                block2tac(node.jump[1])

        for cjump in node.cjumps:
            block2tac(cjump[1][1])

    block2tac(cfg.init)
    for name in cfg.cfg.keys():
        block2tac(name)

    return tac

# --------------------------------------------------------------------
def jthreading(cfg: CFG) -> CFG:
    dests = {}

    def visit(name: str):
        if name in dests:
            return

        node = cfg.cfg[name]
        dests[name] = name

        if isinstance(node.jump, str):
            visit(node.jump)
        for cjump in node.cjumps:
            visit(cjump[1][1])

        if isinstance(node.jump, str) and len(node.body) == 1:
            dests[name] = dests[node.jump]

    for name in cfg.cfg.keys():
        visit(name)

    for block in cfg.cfg.values():
        if block.jump[0] == 'jmp':
            block.jump = ('jmp', dests[block.jump[1]])
        block.cjumps = [
            (cjump[0], (cjump[1][0], dests[cjump[1][1]]))
            for cjump in block.cjumps
        ]

    return cfg

# --------------------------------------------------------------------
def uce(cfg: CFG) -> CFG:
    visited = set()

    def visit(name: str):
        if name in visited:
            return
        visited.add(name)
        node = cfg.cfg[name]
        if node.jump[0] == 'jmp':
            visit(node.jump[1])
        for cjump in node.cjumps:
            visit(cjump[1][1])

    visit(cfg.init)

    return CFG(cfg.init, { x: cfg.cfg[x] for x in visited })
