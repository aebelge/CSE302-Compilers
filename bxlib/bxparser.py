# --------------------------------------------------------------------
import ply.yacc

from .bxast    import *
from .bxerrors import Reporter
from .bxlexer  import Lexer

# ====================================================================
# BX parser definition

class Parser:
    UNIOP = {
        '-' : 'opposite'        ,
        '~' : 'bitwise-negation',
        '!' : 'boolean-not'     ,
    }

    BINOP = {
        '+'  : 'addition'                 ,
        '-'  : 'subtraction'              ,
        '*'  : 'multiplication'           ,
        '/'  : 'division'                 ,
        '%'  : 'modulus'                  ,
        '>>' : 'logical-right-shift'      ,
        '<<' : 'logical-left-shift'       ,
        '&'  : 'bitwise-and'              ,
        '|'  : 'bitwise-or'               ,
        '^'  : 'bitwise-xor'              ,
        '&&' : 'boolean-and'              ,
        '||' : 'boolean-or'               ,
        '==' : 'cmp-equal'                ,
        '!=' : 'cmp-not-equal'            ,
        '<'  : 'cmp-lower-than'           ,
        '<=' : 'cmp-lower-or-equal-than'  ,
        '>'  : 'cmp-greater-than'         ,
        '>=' : 'cmp-greater-or-equal-than',
    }

    tokens = Lexer.tokens
    
    start = 'prgm'

    precedence = (
        ('left'    , 'PIPEPIPE'                ),
        ('left'    , 'AMPAMP'                  ),
        ('left'    , 'PIPE'                    ),
        ('left'    , 'HAT'                     ),
        ('left'    , 'AMP'                     ),
        ('nonassoc', 'EQEQ', 'BANGEQ'          ),
        ('nonassoc', 'LT', 'LTEQ', 'GT', 'GTEQ'),
        ('left'    , 'LTLT', 'GTGT'            ),
        ('left'    , 'PLUS', 'DASH'            ),
        ('left'    , 'STAR', 'SLASH', 'PCENT'  ),
        ('right'   , 'BANG', 'UMINUS'          ),
        ('right'   , 'UNEG'                    ),
    )

    def __init__(self, reporter: Reporter):
        self.lexer    = Lexer(reporter = reporter)
        self.parser   = ply.yacc.yacc(module = self)
        self.reporter = reporter

    def parse(self, program: str):
        with self.reporter.checkpoint() as checkpoint:
            ast = self.parser.parse(
                program,
                lexer    = self.lexer.lexer,
                tracking = True,
            )

            return ast if checkpoint else None

    def _position(self, p) -> Range:
        n = len(p) - 1
        return Range(
            start = (p.linespan(1)[0], self.lexer.column_of_pos(p.lexspan(1)[0])    ),
            end   = (p.linespan(n)[1], self.lexer.column_of_pos(p.lexspan(n)[1]) + 1),
        )

    def p_name(self, p):
        """name : IDENT"""
        p[0] = Name(
            value = p[1],
            position = self._position(p)
        )

    def p_type_bool(self, p):
        """type : BOOL"""
        p[0] = Type.BOOL

    def p_type_int(self, p):
        """type : INT"""
        p[0] = Type.INT

    def p_type_pointer(self, p):
        '''type : type STAR'''
        p[0] = PointerType(
        target = p[1]
        )

    def p_type_array(self, p):
        '''type : type LBRACKET NUMBER RBRACKET'''
        p[0] = ArrayType(
        target = p[1],
        size = p[3]
        )

    def p_expression_var(self, p):
        """expr : name"""
        p[0] = VarExpression(
            name     = p[1],
            position = self._position(p)
        )

    def p_expression_bool(self, p):
        """expr : TRUE
                | FALSE"""
        p[0] = BoolExpression(
            value    = (p[1] == 'true'),
            position = self._position(p),
        )

    def p_expression_int(self, p):
        """expr : NUMBER"""
        p[0] = IntExpression(
            value    = p[1],
            position = self._position(p),
        )

    def p_expression_alloc(self, p):
        '''expr : ALLOC type
                | ALLOC type LBRACKET expr RBRACKET'''
        if len(p) == 3:
            # fixed-size
            assert(isinstance(p[2], ArrayType))
            p[0] = AllocExpression(
                allocated = p[2].target,
                size = IntExpression(p[2].size),
                position = self._position(p)
            )
        else:
            # variable-lenght
            p[0] = AllocExpression(
                allocated = p[2],
                size = p[4],
                position = self._position(p)
            )


    def p_expression_reference(self, p):
        '''expr : AMP assignable
                | AMP LPAREN assignable RPAREN'''
        inside = p[2] if len(p) == 3 else p[3]
        p[0] = ReferenceExpression(
            argument = inside,
            position = self._position(p)
        )

    def p_expression_null(self, p):
        '''expr : NULL'''
        p[0] = NullExpression(
            position = self._position(p)
        )


    def p_expression_uniop(self, p):
        """expr : DASH expr %prec UMINUS
                | TILD expr %prec UNEG
                | BANG expr"""

        p[0] = OpAppExpression(
            operator  = self.UNIOP[p[1]],
            arguments = [p[2]],
            position  = self._position(p),
        )

    def p_expression_binop(self, p):
        """expr : expr PLUS     expr
                | expr DASH     expr
                | expr STAR     expr
                | expr SLASH    expr
                | expr PCENT    expr
                | expr AMP      expr
                | expr PIPE     expr
                | expr HAT      expr
                | expr LTLT     expr
                | expr GTGT     expr

                | expr AMPAMP   expr
                | expr PIPEPIPE expr

                | expr EQEQ     expr
                | expr BANGEQ   expr
                | expr LT       expr
                | expr LTEQ     expr
                | expr GT       expr
                | expr GTEQ     expr"""

        p[0] = OpAppExpression(
            operator  = self.BINOP[p[2]],
            arguments = [p[1], p[3]],
            position  = self._position(p),
        )

    def p_expression_group(self, p):
        """expr : LPAREN expr RPAREN"""
        p[0] = p[2]

    def p_expression_call(self, p):
        """expr : name LPAREN exprs_comma RPAREN"""
        p[0] = CallExpression(
            proc      = p[1],
            arguments = p[3],
            position  = self._position(p),
        )

    def p_expression_print(self, p):
        """expr : PRINT LPAREN expr RPAREN"""
        p[0] = PrintExpression(
            argument = p[3],
            position = self._position(p),
        )

    def p_expression_assignable(self, p):
        """expr : assignable"""
        p[0] = p[1]

    def p_assignable_var(self, p):
        '''assignable : name'''
        p[0] = VarAssignable(
                name = p[1],
                position = self._position(p),
        )

    def p_assignable_dereference(self, p):
        '''assignable : STAR assignable'''
        p[0] = DereferenceAssignable(
                argument = p[2],
                position = self._position(p),
        )
    
    def p_assignable_array(self, p):
        '''assignable : assignable LBRACKET expr RBRACKET'''
        p[0] = ArrayAssignable(
                argument = p[1],
                index = p[3],
                position = self._position(p),
        )

    def p_exprs_comma_1(self, p):
        """exprs_comma_1 : expr
                        | exprs_comma_1 COMMA expr"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1]
            p[0].append(p[3])

    def p_exprs_comma(self, p):
        """exprs_comma :
                       | exprs_comma_1"""
        p[0] = [] if len(p) == 1 else p[1]

    def p_stmt_vardecl(self, p):
        """stmt : VAR name EQ expr COLON type SEMICOLON"""
        p[0] = VarDeclStatement(
            name     = p[2],
            init     = p[4],
            type_    = p[6],
            position = self._position(p),
        )

    def p_stmt_assign(self, p):
        """stmt : assignable EQ expr SEMICOLON"""
        p[0] = AssignStatement(
            name = p[1],
            expression = p[3],
            position = self._position(p),
        )

    def p_stmt_expr(self, p):
        """stmt : expr SEMICOLON"""
        p[0] = ExprStatement(
            expression = p[1],
            position   = self._position(p),
        )

    def p_stmt_if(self, p):
        """stmt : IF LPAREN expr RPAREN sblock stmt_elif"""
        p[0] = IfStatement(
            condition = p[3],
            then      = p[5],
            else_     = p[6],
            position  = self._position(p),
        )

    def p_stmt_elif_empty(self, p):
        """stmt_elif : """
        p[0] = None

    def p_stmt_elif_else(self, p):
        """stmt_elif : ELSE sblock"""
        p[0] = p[2]

    def p_stmt_elif_elseif(self, p):
        """stmt_elif : ELSE IF LPAREN expr RPAREN sblock stmt_elif"""
        p[0] = IfStatement(
            condition = p[4],
            then      = p[6],
            else_     = p[7],
            position  = self._position(p),
        )

    def p_stmt_while(self, p):
        """stmt : WHILE LPAREN expr RPAREN sblock"""
        p[0] = WhileStatement(
            condition = p[3],
            body      = p[5],
            position  = self._position(p),
        )

    def p_stmt_break(self, p):
        """stmt : BREAK SEMICOLON"""
        p[0] = BreakStatement(position = self._position(p))

    def p_stmt_continue(self, p):
        """stmt : CONTINUE SEMICOLON"""
        p[0] = ContinueStatement(position = self._position(p))

    def p_stmt_return_none(self, p):
        """stmt : RETURN SEMICOLON"""
        p[0] = ReturnStatement(expr = None, position = self._position(p))

    def p_stmt_return_some(self, p):
        """stmt : RETURN expr SEMICOLON"""
        p[0] = ReturnStatement(expr = p[2], position = self._position(p))

    def p_stmt_block(self, p):
        """stmt : sblock"""
        p[0] = p[1]
        
    def p_stmts(self, p):
        """stmts :
                 | stmts stmt"""

        if len(p) == 1:
            p[0] = []
        else:
            p[0] = p[1]
            p[0].append(p[2])

    def p_stmt(self, p):
        """stmts : stmts error SEMICOLON"""
        p[0] = p[1]

    def p_block(self, p):
        """sblock : LBRACE stmts RBRACE"""
        p[0] = BlockStatement(
            body     = p[2],
            position = self._position(p),
        )

    def p_arg(self, p):
        """arg : name COLON type"""
        p[0] = (p[1], p[3])

    def p_args_1(self, p):
        """args_1 : arg
                  | args_1 COMMA arg"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1]
            p[1].append(p[3])

    def p_args(self, p):
        """args :
                | args_1"""
        p[0] = [] if len(p) == 1 else p[1]

    def p_rty(self, p):
        """rty :
               | COLON type"""
        p[0] = None if len(p) == 1 else p[2]

    def p_procdecl(self, p):
        """procdecl : DEF name LPAREN args RPAREN rty sblock"""
        p[0] = ProcDecl(
            name      = p[2],
            arguments = p[4],
            rettype   = p[6],
            body      = p[7],
            position  = self._position(p),
        )

    def p_globvardecl(self, p):
        """globvardecl : VAR name EQ expr COLON type SEMICOLON"""
        p[0] = GlobVarDecl(
            name     = p[2],
            init     = p[4],
            type_    = p[6],
            position = self._position(p),
        )

    def p_topdecl(self, p):
        """topdecl : procdecl
                   | globvardecl"""
        p[0] = p[1]

    def p_program(self, p):
        """prgm :
                | prgm topdecl"""
        if len(p) == 1:
            p[0] = []
        else:
            p[0] = p[1]
            p[0].append(p[2])

    def p_error(self, p):
        if p:
            position = Range.of_position(
                p.lineno,
                self.lexer.column_of_pos(p.lexpos),
            )

            self.reporter(
                f'syntax error',
                position = position,
            )
            # self.parser.errok()
        else:
            self.reporter('syntax error at end of file')