# --------------------------------------------------------------------
import contextlib as cl

from typing import Optional as Opt

from .bxast   import *
from .bxscope import Scope
from .bxtac   import *

# ====================================================================
# Maximal munch

class MM:
    _counter = -1

    PRINTS = {
        Type.INT  : 'print_int',
        Type.BOOL : 'print_bool',
    }

    def __init__(self):
        self._proc    = None
        self._tac     = []
        self._scope   = Scope()
        self._loops   = []

    tac = property(lambda self: self._tac)

    @staticmethod
    def mm(prgm: Program):
        mm = MM(); mm.for_program(prgm)
        return mm._tac

    def compute_address(self, assignable: Assignable):
        if isinstance(assignable, ArrayAssignable):
            base_address = self.for_expression(assignable.argument)
            index = self.for_expression(assignable.index)
            element_size = MM.type_size(assignable.argument.type_.target)
            offset = self.fresh_temporary()
            self.push('mul', index, element_size, result=offset)
            address = self.fresh_temporary()
            self.push('add', base_address, offset, result=address)
            return address

        elif isinstance(assignable, DereferenceAssignable):
            return self.for_expression(assignable.argument)

    def type_size(type_: Type): 
        if isinstance(type_, Type):
            return 8

        if isinstance(type_, PointerType):
            return 8

        elif isinstance(type_, ArrayType):
            return MM.type_size(type_.target) * type_.size

        else:
            raise ValueError("Unhandled type in type_size")
        

    @classmethod
    def fresh_temporary(cls):
        cls._counter += 1
        return f'%{cls._counter}'

    @classmethod
    def fresh_label(cls):
        cls._counter += 1
        return f'.L{cls._counter}'

    def push(
            self,
            opcode     : str,
            *arguments : str | int,
            result     : Opt[str] = None,
    ):
        self._proc.tac.append(TAC(opcode, list(arguments), result))

    def push_label(self, label: str):
        self._proc.tac.append(f'{label}:')

    @cl.contextmanager
    def in_loop(self, labels: tuple[str, str]):
        self._loops.append(labels)
        try:
            yield
        finally:
            self._loops.pop()

    def for_program(self, prgm: Program):
        for decl in prgm:
            match decl:
                case GlobVarDecl(name, init, type_):
                    assert(isinstance(init, IntExpression))
                    self._tac.append(TACVar(name.value, init.value))
                    self._scope.push(name.value, f'@{name.value}')

        for decl in prgm:
            match decl:
                case ProcDecl(name, arguments, retty, body):
                    assert(self._proc is None)
                    with self._scope.in_subscope():
                        self._proc = TACProc(
                            name      = name.value,
                            arguments = [f'%{x[0].value}' for x in arguments],
                        )

                        for argument in arguments:
                            self._scope.push(argument[0].value, f'%{argument[0].value}')

                        self.for_statement(body)

                        if name.value == 'main':
                            self.for_statement(ReturnStatement(IntExpression(0)));

                        self._tac.append(self._proc)
                        self._proc = None

    def for_block(self, block: Block):
        with self._scope.in_subscope():
            for stmt in block:
                self.for_statement(stmt)

    def for_statement(self, stmt: Statement):
        match stmt:
            case VarDeclStatement(name, init, type):
                fresh_temp = self.fresh_temporary()
                self._scope.push(name.value, fresh_temp)
                assert(self._proc is not None)
                self._proc.add_temp_size(fresh_temp, MM.type_size(type))  

                if isinstance(type, ArrayType):
                    array_address = self.fresh_temporary()
                    self.push("ref", self._scope[name.value], result=array_address)
                    array_size = type.size * MM.type_size(type.target)  
                    self.push("zero_out", array_address, array_size)
                    return

                temp = self.for_expression(init)
                self.push('copy', temp, result=self._scope[name.value])

            case AssignStatement(target, exp):
                exp_val = self.for_expression(exp)

                if isinstance(target, VarAssignable):
                    #direct assignment to a var
                    name_val = self._scope[target.name.value]
                    self.push('copy', exp_val, result=name_val)

                elif isinstance(target, (DereferenceAssignable, ArrayAssignable)):
                    #assignment to a derefd pointer or an array element
                    target_address = self.compute_address(target)
                    self.push('store', exp_val, target_address)

                else:
                    raise RuntimeError(f"Unhandled assignment target type: {type(target).__name__}")

            case ExprStatement(expr):
                self.for_expression(expr)

            case PrintStatement(value):
                temp = self.for_expression(value)
                self.push('print', temp)

            case IfStatement(condition, then, else_):
                tlabel = self.fresh_label()
                flabel = self.fresh_label()
                olabel = self.fresh_label()

                self.for_bexpression(condition, tlabel, flabel)
                self.push_label(tlabel)
                self.for_statement(then)
                self.push('jmp', olabel)
                self.push_label(flabel)
                if else_ is not None:
                    self.for_statement(else_)
                self.push_label(olabel)

            case WhileStatement(condition, body):
                clabel = self.fresh_label()
                blabel = self.fresh_label()
                olabel = self.fresh_label()

                with self.in_loop((clabel, olabel)):
                    self.push_label(clabel)
                    self.for_bexpression(condition, blabel, olabel)
                    self.push_label(blabel)
                    self.for_statement(body)
                    self.push('jmp', clabel)
                    self.push_label(olabel)

            case ContinueStatement():
                self.push('jmp', self._loops[-1][0])

            case BreakStatement():
                self.push('jmp', self._loops[-1][1])

            case BlockStatement(body):
                self.for_block(body)

            case ReturnStatement(expr):
                if expr is None:
                    self.push('ret')
                else:
                    temp = self.for_expression(expr)
                    self.push('ret', temp)

            case _:
                assert(False)

    def for_expression(self, expr: Expression, force = False) -> str:
        target = None

        if not force and expr.type_ == Type.BOOL:
            target = self.fresh_temporary()
            tlabel = self.fresh_label()
            flabel = self.fresh_label()

            self.push('const', 0, result = target)
            self.for_bexpression(expr, tlabel, flabel)
            self.push_label(tlabel)
            self.push('const', 1, result = target)
            self.push_label(flabel)

        else:
            match expr:
                case VarExpression(name):
                    target = self._scope[name.value]

                case IntExpression(value):
                    target = self.fresh_temporary()
                    self.push('const', value, result = target)

                case BoolExpression(value):
                    target = self.fresh_temporary()
                    self.push('const', 1 if value else 0, result=target)

                case NullExpression():
                    temp = self.fresh_temporary()
                    self.push('const', 0, result=temp)
                    return temp

                case OpAppExpression(operator, arguments):
                    target    = self.fresh_temporary()
                    arguments = [self.for_expression(e) for e in arguments]
                    self.push(OPCODES[operator], *arguments, result = target)

                case CallExpression(proc, arguments):
                    for i, argument in enumerate(arguments):
                        temp = self.for_expression(argument)
                        self.push('param', i+1, temp)
                    if expr.type_ != Type.VOID:
                        target = self.fresh_temporary()
                    self.push('call', proc.value, len(arguments), result = target)

                case PrintExpression(argument):
                    temp = self.for_expression(argument)
                    self.push('param', 1, temp)
                    proc = self.PRINTS[argument.type_]
                    self.push('call', proc, 1)

                case AllocExpression(allocated, size):
                    size_temp = self.for_expression(size)
                    element_size = MM.type_size(allocated) 
                    total_size_temp = self.fresh_temporary()
                    self.push('mul', size_temp, element_size, result=total_size_temp) 
                    result_temp = self.fresh_temporary()
                    self.push('call', 'alloc', total_size_temp, result=result_temp)
                    return result_temp

                case VarAssignable(name):
                    if name.value in self._scope:
                        return self._scope[name.value]
                    else:
                        raise NotImplementedError(f"Variable {name.value} not found in scope")

                case DereferenceAssignable(argument):
                    address = self.for_expression(argument)
                    dereference_temp = self.fresh_temporary()
                    self.push('load', address, result=dereference_temp)
                    return dereference_temp

                case ArrayAssignable(argument, index):
                    base_address = self.for_expression(argument)
                    index = self.for_expression(index)
                    element_size = MM.type_size(argument.type_.target)
                    offset = self.fresh_temporary()
                    self.push('mul', index, element_size, result=offset)
                    address = self.fresh_temporary()
                    self.push('add', base_address, offset, result=address)
                    load_temp = self.fresh_temporary()
                    self.push('load', address, result=load_temp)
                    return load_temp
                
                case ReferenceExpression(argument):
                    argument = expr.argument
                    if isinstance(argument, VarAssignable):
                        var_name = argument.name.value
                        address_temp = self.fresh_temporary()
                        self.push('addr', self._scope[var_name], result=address_temp)
                        return address_temp
                    
                    elif isinstance(argument, ArrayAssignable):
                        base_address = self.for_expression(argument.argument)
                        index = self.for_expression(argument.index)
                        element_size = MM.type_size(argument.argument.type_.target)
                        offset = self.fresh_temporary()
                        self.push('mul', index, element_size, result=offset)
                        address = self.fresh_temporary()
                        self.push('add', base_address, offset, result=address)
                        return address
                    
                    else:
                        address_temp = self.fresh_temporary()
                        self.push('addr', self._scope[argument.value], result = address_temp)
                        return address_temp
                

                case _:
                    print(f"unhandled expression type: {type(expr).__name__}")
                    assert(False)
        return target

    CMP_JMP = {
        'cmp-equal'                 : 'jz',
        'cmp-not-equal'             : 'jnz',
        'cmp-lower-than'            : 'jgt',
        'cmp-lower-or-equal-than'   : 'jge',
        'cmp-greater-than'          : 'jlt',
        'cmp-greater-or-equal-than' : 'jle',
    }

    def for_bexpression(self, expr: Expression, tlabel: str, flabel: str):
        assert(expr.type_ == Type.BOOL)

        match expr:
            case VarExpression(name):
                temp = self._scope[name.value]
                self.push('jz', temp, flabel)
                self.push('jmp', tlabel)

            case BoolExpression(True):
                self.push('jmp', tlabel)

            case BoolExpression(False):
                self.push('jmp', flabel)

            case OpAppExpression(
                    'cmp-equal'                |
                    'cmp-not-equal'            |
                    'cmp-lower-than'           |
                    'cmp-lower-or-equal-than'  |
                    'cmp-greater-than'         |
                    'cmp-greater-or-equal-than',
                    [e1, e2]):

                t1 = self.for_expression(e1)
                t2 = self.for_expression(e2)
                t  = self.fresh_temporary()
                self.push(OPCODES['subtraction'], t2, t1, result = t)

                self.push(self.CMP_JMP[expr.operator], t, tlabel)
                self.push('jmp', flabel)

            case OpAppExpression('boolean-and', [e1, e2]):
                olabel = self.fresh_label()
                self.for_bexpression(e1, olabel, flabel)
                self.push_label(olabel)
                self.for_bexpression(e2, tlabel, flabel)

            case OpAppExpression('boolean-or', [e1, e2]):
                olabel = self.fresh_label()
                self.for_bexpression(e1, tlabel, olabel)
                self.push_label(olabel)
                self.for_bexpression(e2, tlabel, flabel)

            case OpAppExpression('boolean-not', [e]):
                self.for_bexpression(e, flabel, tlabel)

            case CallExpression(_):
                temp = self.for_expression(expr, force = True)
                self.push('jz', temp, flabel)
                self.push('jmp', tlabel)

            case DereferenceAssignable(argument):
                address = self.for_expression(argument)
                dereferenced_temp = self.fresh_temporary()
                self.push('load', address, result=dereferenced_temp)
                self.push('jz', dereferenced_temp, flabel)
                self.push('jmp', tlabel)

            case ArrayAssignable(argument, index):
                element_address = self.compute_address(expr)
                element_temp = self.fresh_temporary()
                self.push('load', element_address, result=element_temp)
                self.push('jz', element_temp, flabel)
                self.push('jmp', tlabel)

            case _:
                assert(False)



