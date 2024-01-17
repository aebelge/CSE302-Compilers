# --------------------------------------------------------------------
import contextlib as cl
import typing as tp

from .bxerrors import Reporter
from .bxast    import *
from .bxscope  import Scope

# ====================================================================
SigType    = tuple[tuple[Type], Opt[Type]]
ProcSigMap = dict[str, SigType]

# --------------------------------------------------------------------
class PreTyper:
    def __init__(self, reporter : Reporter):
        self.reporter = reporter

    def pretype(self, prgm : Program) -> tuple[Scope, ProcSigMap]:
        scope = Scope()
        procs = dict()

        for topdecl in prgm:
            match topdecl:
                case ProcDecl(name, arguments, rettype, body):
                    if name.value in procs:
                        self.reporter(
                            f'duplicated procedure name: {name.value}',
                            position = name.position
                        )
                        continue

                    procs[name.value] = (
                        tuple(x[1] for x in arguments),
                        Type.VOID if rettype is None else rettype
                    )

                case GlobVarDecl(name, init, type_):
                    if name.value in scope:
                        self.reporter(
                            f'duplicated global variable name: {name.value}',
                            position = name.position
                        )
                        continue

                    scope.push(name.value, type_)

                case _:
                    assert(False)

        if 'main' not in procs:
            self.reporter('this program is missing a main subroutine')
        elif procs['main'] != ((), Type.VOID):
            self.reporter(
                '"main" should not take any argument and should not return any value'
            )

        return scope, procs

# --------------------------------------------------------------------
class TypeChecker:
    B : Type = Type.BOOL
    I : Type = Type.INT

    SIGS = {
        'opposite'                 : ([I   ], I),
        'bitwise-negation'         : ([B   ], B),
        'boolean-not'              : ([B   ], B),
        'addition'                 : ([I, I], I),
        'subtraction'              : ([I, I], I),
        'multiplication'           : ([I, I], I),
        'division'                 : ([I, I], I),
        'modulus'                  : ([I, I], I),
        'logical-right-shift'      : ([I, I], I),
        'logical-left-shift'       : ([I, I], I),
        'bitwise-and'              : ([I, I], I),
        'bitwise-or'               : ([I, I], I),
        'bitwise-xor'              : ([I, I], I),
        'boolean-and'              : ([B, B], B),
        'boolean-or'               : ([B, B], B),
        'cmp-equal'                : ([I, I], B),
        'cmp-not-equal'            : ([I, I], B),
        'cmp-lower-than'           : ([I, I], B),
        'cmp-lower-or-equal-than'  : ([I, I], B),
        'cmp-greater-than'         : ([I, I], B),
        'cmp-greater-or-equal-than': ([I, I], B),
    }

    def __init__(self, scope : Scope, procs : ProcSigMap, reporter : Reporter):
        self.scope    = scope
        self.procs    = procs
        self.loops    = 0
        self.proc     = None
        self.reporter = reporter

    def report(self, msg: str, position: Opt[Range] = None):
        self.reporter(msg, position = position)

    @cl.contextmanager
    def in_loop(self):
        self.loops += 1
        try:
            yield self
        finally:
            self.loops -= 1

    @cl.contextmanager
    def in_proc(self, proc: ProcDecl):
        assert(self.proc is None)

        self.proc = proc
        self.scope.open()
        try:
            yield self
        finally:
            self.proc = None
            self.scope.close()

    def check_local_free(self, name : Name):
        if self.scope.islocal(name.value):
            self.report(f'duplicated variable declaration for {name.value}')
            return False
        return True

    def check_local_bound(self, name : Name):
        if name.value not in self.scope:
            self.report(
                f'missing variable declaration for {name.value}',
                position = name.position,
            )
            return None
        return self.scope[name.value]

    def check_integer_constant_range(self, value : int):
        if value not in range(-(1 << 63), 1 << 63):
            self.report(f'integer literal out of range: {value}')
            return False
        return True

    def for_expression(self, expr : Expression, etype : tp.Optional[Type] = None):
        type_ = None

        match expr:
            case VarExpression(name):
                if self.check_local_bound(name):
                    type_ = self.scope[name.value]

            case BoolExpression(_):
                type_ = Type.BOOL

            case IntExpression(value):
                self.check_integer_constant_range(value)
                type_ = Type.INT

            case NullExpression():
                if etype is not None: 
                    if isinstance(etype, PointerType):
                        type_ = etype
                else : 
                    type_ = Type.NULL

            case ReferenceExpression(argument):
                self.for_expression(argument)
                if isinstance(argument, Assignable):
                    type_ = PointerType(target=argument.type_)
                else:
                    self.report("Invalid type for reference operation", position=expr.position)

            case OpAppExpression(opname, arguments):
                if opname in ['cmp-equal', 'cmp-not-equal']:
                    #type each argument
                    self.for_expression(arguments[0])
                    self.for_expression(arguments[1])
                    
                    #pointer comparison
                    args_types = [arguments[0].type_, arguments[1].type_]
                    if all(isinstance(t, PointerType) or t == Type.NULL for t in args_types):
                        type_ = Type.BOOL
                    elif all(t in [Type.INT, Type.BOOL] for t in args_types):
                        type_ = Type.BOOL
                    else:
                        self.report("Invalid types for comparison", position=expr.position)
                else:
                    #handling for other operators
                    opsig = self.SIGS.get(opname)
                    if opsig:
                        for atype, argument in zip(opsig[0], arguments):
                            self.for_expression(argument, etype=atype)
                        type_ = opsig[1]
                    else:
                        self.report(f"Unknown operator: {opname}", position=expr.position)

            case AllocExpression(allocated, size):
                self.for_expression(size, etype = Type.INT)
                type_ = PointerType(target=allocated)

            case VarAssignable(name):
                if name.value not in self.scope:
                    self.report(f"Undefined variable: {name.value}", position=expr.position)
                else:
                    type_ = self.scope[name.value]  

            case DereferenceAssignable(argument):
                self.for_expression(argument)
                if isinstance(argument.type_, PointerType):
                    type_ = argument.type_.target
                else:
                    self.report("Dereferencing a non-pointer type", position=expr.position)

            case ArrayAssignable(argument, index):
                self.for_expression(argument)
                self.for_expression(index, etype=Type.INT)
                if isinstance(argument.type_, (ArrayType, PointerType)):
                    type_ = argument.type_.target
                else:
                    self.report("Indexing a non-array and non-pointer type", position=expr.position)

            case CallExpression(name, arguments):
                atypes, retty = [], None

                if name.value not in self.procs:
                    self.report(
                        f'unknown procedure: {name.value}',
                        position = name.position,
                    )
                else:
                    atypes, retty = self.procs[name.value]

                    if len(atypes) != len(arguments):
                        self.report(
                            f'invalid number of arguments: expected {len(atypes)}, got {len(arguments)}',
                            position = expr.position,
                        )

                for i, a in enumerate(arguments):
                    self.for_expression(a, atypes[i] if i in range(len(atypes)) else None)

                type_ = retty

            case PrintExpression(e):
                self.for_expression(e);

                if e.type_ is not None:
                    if e.type_ not in (Type.INT, Type.BOOL):
                        self.report(
                            f'can only print integers and booleans, not {e.type_}',
                            position = e.position,
                        )

                type_ = Type.VOID

            case _:
                print(expr)
                assert(False)

        if type_ is not None:
            if etype is not None:
                if type_ != etype:
                    self.report(
                        f'invalid type: get {type_}, expected {etype}',
                        position = expr.position,
                    )

        expr.type_ = type_

    def for_statement(self, stmt : Statement):
        match stmt:
            case VarDeclStatement(name, init, type_):
                if self.check_local_free(name): 
                    self.scope.push(name.value, type_)

                if isinstance(type_, ArrayType):
                    stmt.type_ = type_
                    if isinstance(init, IntExpression) and init.value == 0:
                        pass  
                    else:
                        self.report(
                            'Arrays can be initialized only with literal 0',
                            position=stmt.position,
                        )
                else:
                    stmt.type_ = type_
                    self.for_expression(init, etype=type_)
                    
            case AssignStatement(name, expression):
                    if isinstance(name, VarAssignable):
                        lhstype = self.check_local_bound(name.name)
                    elif isinstance(name, DereferenceAssignable):
                        self.for_expression(name.argument)
                        lhstype = name.argument.type_.target if isinstance(name.argument.type_, PointerType) else None
                    elif isinstance(name, ArrayAssignable):
                        self.for_expression(name.argument)
                        self.for_expression(name.index, etype=self.I)
                        lhstype = name.argument.type_.target if isinstance(name.argument.type_, (ArrayType, PointerType)) else None
                    else:
                        lhstype = None

                    self.for_expression(expression, etype=lhstype)

            case ExprStatement(expression):
                self.for_expression(expression)

            case BlockStatement(block):
                self.for_block(block)

            case IfStatement(condition, iftrue, iffalse):
                self.for_expression(condition, etype = Type.BOOL)
                self.for_statement(iftrue)
                if iffalse is not None:
                    self.for_statement(iffalse)

            case WhileStatement(condition, body):
                self.for_expression(condition, etype = Type.BOOL)
                with self.in_loop():
                    self.for_statement(body)

            case BreakStatement() | ContinueStatement():
                if self.loops == 0:
                    self.report(
                        'break/continue statement outside of a loop',
                        position = stmt.position,
                    )

            case PrintStatement(init):
                self.for_expression(init, etype = Type.INT)

            case ReturnStatement(e):
                if e is None:
                    if self.proc.rettype is not None:
                        self.report(
                            'value-less return statement in a function',
                            position = stmt.position,
                        )
                    self.for_expression(e, etype = self.proc.retty)
                else:
                    if self.proc.rettype is None:
                        self.report(
                            'return statement in a subroutine',
                            position = stmt.position,
                        )

            case _:
                print(stmt)
                assert(False)

    def for_block(self, block : Block):
        with self.scope.in_subscope():
            for stmt in block:
                self.for_statement(stmt)

    def for_topdecl(self, decl : TopDecl):
        match decl:
            case ProcDecl(name, arguments, retty, body):
                with self.in_proc(decl):
                    for vname, vtype_ in arguments:
                        if self.check_local_free(vname):
                            self.scope.push(vname.value, vtype_)
                    self.for_statement(body)

                    if retty is not None:
                        if not self.has_return(body):
                            self.report(
                                'this function is missing a return statement',
                                position = decl.position,
                            )

            case GlobVarDecl(name, init, type_):
                print(f"Debug: Checking global variable '{name.value}'")
                self.for_expression(init)

                if isinstance(type_, ArrayType):
                    if isinstance(init, IntExpression) and init.value == 0:
                        init.type_ = type_  #set the initializer's type to arraytype
                    else:
                        self.report(
                            f'Invalid initializer for array type {type_}',
                            position = init.position,
                        )
                elif not (init.type_ == type_ or (isinstance(type_, PointerType) and isinstance(init, AllocExpression))):
                    self.report(
                        f"Type mismatch in global variable '{name.value}' initialization",
                        position = init.position,
                    )

    def for_program(self, prgm : Program):
        for decl in prgm:
            self.for_topdecl(decl)

    def check_constant(self, expr: Expression):
        match expr:
            case IntExpression(_):
                return True

            case _:
                return False

    def has_return(self, stmt: Statement):
        match stmt:
            case ReturnStatement(_):
                return True

            case IfStatement(_, iftrue, iffalse):
                return \
                    self.has_return(iftrue) and \
                    self.has_return(iffalse)

            case BlockStatement(block):
                return any(self.has_return(b) for b in block)

            case _:
                return False

    def check(self, prgm : Program):
        self.for_program(prgm)

# --------------------------------------------------------------------
def check(prgm : Program, reporter : Reporter):
    with reporter.checkpoint() as checkpoint:
        scope, procs = PreTyper(reporter).pretype(prgm)
        TypeChecker(scope, procs, reporter).check(prgm)
        return bool(checkpoint)
