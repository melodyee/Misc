#!/usr/bin/env python
from __future__ import print_function

import lldb, sys
import logging

STACK_MAX = 64
log = logging.getLogger('Trace')

class DebuggerHelper (object):
    # General methods for retrieving common types of registers
    def get_pc_name(self):
        return self.pc

    def get_pc(self):
        return self.get_register(self.pc)

    def get_sp_name(self):
        return self.sp

    def get_sp(self):
        return self.get_register(self.sp)

class LLDBHelper (DebuggerHelper):
    @staticmethod
    def has_target():
        registers = LLDBHelper.get_frame().GetRegisters()
        return len(registers) != 0

    @staticmethod
    def get_frame():
        return lldb.debugger.GetTargetAtIndex(0).process.selected_thread.GetFrameAtIndex(0)

    @staticmethod
    def get_arch():
        return lldb.debugger.GetTargetAtIndex(0).triple.split('-')[0]

    @staticmethod
    def helper():
        if LLDBHelper.has_target():
            arch = LLDBHelper.get_arch()
            for cls in LLDBHelper.__subclasses__():
                if hasattr(cls, 'archs') and arch in cls.archs:
                    inst = cls()
                    return inst
            raise LookupError('No helper found for arch {}'.format(arch))
        raise LookupError('No target')

    def get_next_instruction(self):
        target = lldb.debugger.GetTargetAtIndex(0)
        pc = lldb.SBAddress(self.get_pc(), target)
        inst = target.ReadInstructions(pc, 1)
        return str(inst).split(':')[1].strip()

    def get_registers(self):
        log.debug('Getting registers')

        regs = LLDBHelper.get_frame().GetRegisters()
        objs = []
        for i in xrange(len(regs)):
            objs += regs[i]

        regs = {}
        for reg in objs:
            val = 'n/a'
            if reg.value != None:
                try:
                    val = int(reg.value, 16)
                except:
                    try:
                        val = int(reg.value)
                    except Exception as e:
                        log.debug("Exception converting register value: " + str(e))
                        val = 0
            regs[reg.name] = val

        return regs

    def get_register(self, reg):
        log.debug('Getting register: ' + reg)
        return self.get_registers()[reg]

    def get_disasm(self):
        log.debug('Getting disasm')
        res = self.get_cmd_output('disassemble -c {}'.format(DISASM_MAX))
        return res

    def get_stack(self):
        log.debug('Getting stack')
        error = lldb.SBError()
        res = lldb.debugger.GetTargetAtIndex(0).process.ReadMemory(self.get_sp(), STACK_MAX*16, error)
        return res

    def get_memory(self, start, length):
        log.debug('Getting %x + %d' % (start, length))
        error = lldb.SBError()
        res = lldb.debugger.GetTargetAtIndex(0).process.ReadMemory(start, length, error)
        return res

    def get_backtrace(self):
        log.debug('Getting backtrace')
        res = self.get_cmd_output('bt')
        return res

    def get_cmd_output(self, cmd=None):
        if cmd:
            log.debug('Getting command output: ' + cmd)
            res = lldb.SBCommandReturnObject()
            lldb.debugger.GetCommandInterpreter().HandleCommand(cmd, res)
            res = res.GetOutput()
        else:
            res = "<No command>"
        return res

    def get_current_thread(self):
        return lldb.debugger.GetTargetAtIndex(0).process.GetSelectedThread().idx

class LLDBHelperX86 (LLDBHelper):
    archs = ['i386']
    arch_group = 'x86'
    pc = 'eip'
    sp = 'esp'

    def get_registers(self):
        regs = super(LLDBHelperX86, self).get_registers()
        for i in range(7):
            regs['st'+str(i)] = regs['stmm'+str(i)]
            return regs

class LLDBHelperX64 (LLDBHelperX86, LLDBHelper):
    archs = ['x86_64']
    arch_group = 'x64'
    pc = 'rip'
    sp = 'rsp'

class Trace(object) :
  def __init__(self):
    self.dbg = lldb.SBDebugger.Create()
    self.dbg.SetAsync(False)
    lldb.debugger = self.dbg

    self.ci = self.dbg.GetCommandInterpreter()
    self.dbgHelper = LLDBHelperX64()

  def run(self, binary):
    print(binary)
    self.exec_command('file /h/llvm/a.out')
    self.exec_command('b main')
    self.exec_command('r')
    print(self.dbgHelper.get_backtrace())
    print(self.dbgHelper.get_registers())
    print(self.dbgHelper.get_stack()) 
    self.exec_command('c')
    self.exec_command('exit')

  def exec_command(self, cmd):
    res = lldb.SBCommandReturnObject()
    self.ci.HandleCommand(cmd, res)

    if res.Succeeded():
      print('#' + res.GetOutput().strip() + '#')
    else:
      print(res.GetError().strip())

def main(arg):
  trace = Trace()
  trace.run(arg)

if __name__ == "__main__":
    main(sys.argv[1])
