# --------------------------------------------------------------------
import abc

from .bxtac import *

# --------------------------------------------------------------------
class AsmGen(abc.ABC):
    BACKENDS   = {}

    def __init__(self):
        self._tparams = dict()
        self._temps   = dict()
        self._asm     = []
        self._current_index = 0
        self._temp_sizes = {}

    def assign_temp_sizes(self, temp_sizes: dict[str, int]):
        self._temp_sizes = temp_sizes

    def _temp(self, temp):
        if isinstance(temp, int):
            return f'${temp}'
        if temp.startswith('@'):
            return self._format_temp(temp[1:])
        if temp in self._tparams:
            return self._format_param(self._tparams[temp])

        temp_index = self._temps.get(temp)
        if temp_index is not None:
            return self._format_temp(temp_index)
        else:
            temp_size = self._temp_sizes.get(temp, 8) 
            self._current_index += temp_size // 8
            self._temps[temp] = self._current_index - 1
            return self._format_temp(self._current_index - 1)

    @abc.abstractmethod
    def _format_temp(self, index):
        pass

    @abc.abstractmethod
    def _format_param(self, index):
        pass

    def __call__(self, instr: TAC | str):
        if isinstance(instr, str):
            self._asm.append(instr)
            return

        opcode = instr.opcode
        args   = instr.arguments[:]

        if instr.result is not None:
            args.append(instr.result)

        getattr(self, f'_emit_{opcode}')(*args)

    def _get_asm(self, opcode, *args):
        if not args:
            return f'\t{opcode}'
        return f'\t{opcode}\t{", ".join(args)}'

    def _get_label(self, lbl):
        return f'{lbl}:'

    def _emit(self, opcode, *args):
        self._asm.append(self._get_asm(opcode, *args))

    def _emit_label(self, lbl):
        self._asm.append(self._get_label(lbl))

    @classmethod
    def get_backend(cls, name):
        return cls.BACKENDS[name]

# --------------------------------------------------------------------
class AsmGen_x64_Linux(AsmGen):
    PARAMS = ['%rdi', '%rsi', '%rdx', '%rcx', '%r8', '%r9']

    def __init__(self):
        super().__init__()
        self._params = []
        self._endlbl = None
        self._label_counter = 0

    def fresh_label(self):
        """
        generate a unique label name
        """
        label = f'.L{self._label_counter}'
        self._label_counter += 1
        return label

    def _format_temp(self, index):
        if isinstance(index, str):
            return f'{index}(%rip)'
        return f'-{8*(index+1)}(%rbp)'

    def _format_param(self, index):
        return f'{8*(index+2)}(%rbp)'

    def _emit_const(self, ctt, dst):
        self._emit('movq', f'${ctt}', self._temp(dst))

    def _emit_copy(self, src, dst):
        self._emit('movq', self._temp(src), '%r11')
        self._emit('movq', '%r11', self._temp(dst))

    def _emit_alu1(self, opcode, src, dst):
        self._emit('movq', self._temp(src), '%r11')
        self._emit(opcode, '%r11')
        self._emit('movq', '%r11', self._temp(dst))

    def _emit_neg(self, src, dst):
        self._emit_alu1('negq', src, dst)

    def _emit_not(self, src, dst):
        self._emit_alu1('notq', src, dst)

    def _emit_alu2(self, opcode, op1, op2, dst):
        self._emit('movq', self._temp(op1), '%r11')
        self._emit(opcode, self._temp(op2), '%r11')
        self._emit('movq', '%r11', self._temp(dst))

    def _emit_add(self, op1, op2, dst):
        self._emit_alu2('addq', op1, op2, dst)

    def _emit_sub(self, op1, op2, dst):
        self._emit_alu2('subq', op1, op2, dst)

    def _emit_mul(self, op1, op2, dst):
        op1_temp = f'%{self._current_index}' if isinstance(op1, int) else op1
        op2_temp = f'%{self._current_index + 1}' if isinstance(op2, int) else op2

        if isinstance(op1, int):
            self._emit('movq', f'${op1}', self._temp(op1_temp))
            self._current_index += 1
        if isinstance(op2, int):
            self._emit('movq', f'${op2}', self._temp(op2_temp))
            self._current_index += 1

        self._emit('movq', self._temp(op1_temp), '%rax')
        self._emit('imulq', self._temp(op2_temp))
        self._emit('movq', '%rax', self._temp(dst))


    def _emit_div(self, op1, op2, dst):
        self._emit('movq', self._temp(op1), '%rax')
        self._emit('cqto')
        self._emit('idivq', self._temp(op2))
        self._emit('movq', '%rax', self._temp(dst))

    def _emit_mod(self, op1, op2, dst):
        self._emit('movq', self._temp(op1), '%rax')
        self._emit('cqto')
        self._emit('idivq', self._temp(op2))
        self._emit('movq', '%rdx', self._temp(dst))

    def _emit_and(self, op1, op2, dst):
        self._emit_alu2('andq', op1, op2, dst)

    def _emit_or(self, op1, op2, dst):
        self._emit_alu2('orq', op1, op2, dst)

    def _emit_xor(self, op1, op2, dst):
        self._emit_alu2('xorq', op1, op2, dst)

    def _emit_shl(self, op1, op2, dst):
        self._emit('movq', self._temp(op1), '%r11')
        self._emit('movq', self._temp(op2), '%rcx')
        self._emit('salq', '%cl', '%r11')
        self._emit('movq', '%r11', self._temp(dst))

    def _emit_shr(self, op1, op2, dst):
        self._emit('movq', self._temp(op1), '%r11')
        self._emit('movq', self._temp(op2), '%rcx')
        self._emit('sarq', '%cl', '%r11')
        self._emit('movq', '%r11', self._temp(dst))

    def _emit_print(self, arg):
        self._emit('leaq', '.lprintfmt(%rip)', '%rdi')
        self._emit('movq', self._temp(arg), '%rsi')
        self._emit('xorq', '%rax', '%rax')
        self._emit('callq', 'printf@PLT')

    def _emit_jmp(self, lbl):
        self._emit('jmp', lbl)

    def _emit_cjmp(self, cd, op, lbl):
        self._emit('cmpq', '$0', self._temp(op))
        self._emit(cd, lbl)

    def _emit_jz(self, op, lbl):
        self._emit_cjmp('jz', op, lbl)

    def _emit_jnz(self, op, lbl):
        self._emit_cjmp('jnz', op, lbl)

    def _emit_jlt(self, op, lbl):
        self._emit_cjmp('jl', op, lbl)

    def _emit_jle(self, op, lbl):
        self._emit_cjmp('jle', op, lbl)

    def _emit_jgt(self, op, lbl):
        self._emit_cjmp('jg', op, lbl)

    def _emit_jge(self, op, lbl):
        self._emit_cjmp('jge', op, lbl)

    def _emit_param(self, i, arg):
        assert(len(self._params)+1 == i)
        self._params.append(arg)

    def _format_temp(self, index):
        if isinstance(index, str):
            return f'{index}(%rip)'
        return f'-{8 * (index + 1)}(%rbp)'

    def _format_param(self, index):
        return f'{8 * (index + 2)}(%rbp)'

    def _emit_load(self, address, dest):
        self._emit("movq", self._temp(address), '%rax')
        self._emit("movq", "(%rax)", "%rbx")
        self._emit("movq", "%rbx", self._temp(dest))

    def _emit_store(self, src, address):
        self._emit('movq', self._temp(src), '%r11')
        self._emit('movq', self._temp(address), '%rax')
        self._emit('movq', '%r11', '(%rax)')

    def _emit_alloc(self, size, dest):
        self._emit("movq", self._temp(size), "%rdi")
        self._emit("callq", "malloc@PLT")
        self._emit("movq", "%rax", self._temp(dest))
    
    def _emit_ref(self, src, dest):
        self._emit('leaq', self._temp(src), '%rax')
        self._emit('movq', '%rax', self._temp(dest))

    def _emit_zero_out(self, address, size):
        #generate unique labels for the loop and its end
        loop_label = self.fresh_label()
        end_label = self.fresh_label()

        #initialize counter R10 and end address R11
        self._emit('movq', self._temp(size), '%r10')       
        self._emit('movq', self._temp(address), '%r11')   
        self._emit('addq', '%r10', '%r11')                
        self._emit_label(loop_label)                        

        #zero out each byte
        self._emit('cmpq', '%r11', self._temp(address))
        self._emit('je', end_label)
        self._emit('movb', '$0', '-1(%r11)')
        self._emit('decq', '%r11')
        self._emit('jmp', loop_label)
        self._emit_label(end_label)                       

    def _emit_call(self, lbl, arg, ret=None):
        for i, x in enumerate(self._params[:6]):
            self._emit('movq', self._temp(x), self.PARAMS[i])

        additional_args = len(self._params) - 6
        if additional_args > 0:
            if additional_args & 1:
                self._emit('subq', '$8', '%rsp')
            for x in self._params[6:][::-1]:
                self._emit('pushq', self._temp(x))

        self._emit('callq', lbl)

        if additional_args > 0:
            stack_adjustment = 8 * additional_args + (additional_args & 1) * 8
            self._emit('addq', f'${stack_adjustment}', '%rsp')

        if ret is not None:
            self._emit('movq', '%rax', self._temp(ret))

        self._params = []

    def _emit_addr(self, src, dest):
        """
        emit code for an 'addr' operation. computes the address of a variable and stores it in another temporary variable
        """
        self._emit('leaq', self._temp(src), '%rax')
        self._emit('movq', '%rax', self._temp(dest))

    def _emit_ret(self, ret = None):
        if ret is not None:
            self._emit('movq', self._temp(ret), '%rax')
        self._emit('jmp', self._endlbl)

    @classmethod
    def lower1(cls, tac: TACProc | TACVar) -> list[str]:
        emitter = cls()

        match tac:
            case TACVar(name, init):
                emitter._emit('.data')
                emitter._emit('.globl', name)
                emitter._emit_label(name)
                emitter._emit('.quad', str(init))
                return emitter._asm

            case TACProc(name, arguments, ptac):
                emitter._endlbl = f'.E_{name}'
                emitter.assign_temp_sizes(tac.temp_sizes)

                for i in range(min(6, len(arguments))):
                    emitter._emit('movq', emitter.PARAMS[i], emitter._temp(arguments[i]))

                for i, arg in enumerate(arguments[6:]):
                    emitter._tparams[arg] = i

                for instr in ptac:
                    emitter(instr)

                nvars  = len(emitter._temps)
                nvars += nvars & 1

                return [
                    emitter._get_asm('.text'),
                    emitter._get_asm('.globl', name),
                    emitter._get_label(name),
                    emitter._get_asm('pushq', '%rbp'),
                    emitter._get_asm('movq', '%rsp', '%rbp'),
                    emitter._get_asm('subq', f'${8*nvars}', '%rsp'),
                ] + emitter._asm + [
                    emitter._get_label(emitter._endlbl),
                    emitter._get_asm('movq', '%rbp', '%rsp'),
                    emitter._get_asm('popq', '%rbp'),
                    emitter._get_asm('retq'),
                ]

    @classmethod
    def lower(cls, tacs: list[TACProc | TACVar]) -> str:
        aout = [cls.lower1(tac) for tac in tacs]
        aout = [x for tac in aout for x in tac]
        return "\n".join(aout) + "\n"

AsmGen.BACKENDS['x64-linux'] = AsmGen_x64_Linux
