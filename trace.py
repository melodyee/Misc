#!/usr/bin/env python
from __future__ import print_function

import lldb, sys
import logging

class Trace(object) :

  MAX_STDOUT = 100

  MAX_NUM_STEP = 300

  def __init__(self):
    self.dbg = lldb.SBDebugger.Create()
    self.dbg.SetAsync(False)
    lldb.debugger = self.dbg

    self.ci = self.dbg.GetCommandInterpreter()
    self.pytutor_trace = {}
    self.trace = []
    self.heap = {}
    self.stdout = ''

  def run(self, argv):
    src, binary = argv[1], argv[2]
    print((src, binary))

    self.pytutor_trace['code'] = open(src).read()

    self.exec_command('file ' + binary)
    self.exec_command('b main')
    self.exec_command('r')
    target = self.dbg.GetSelectedTarget()
    succeeded = True
    num_step = 0
    while succeeded:
      self.dump_status(target)
      succeeded = self.exec_command('s').Succeeded()
      num_step += 1
      if num_step >= Trace.MAX_NUM_STEP:
        break
      line_number = self.get_line_number()
      if line_number == 0:
        break
    target.GetProcess().Destroy()
    print('before exit')
    self.exec_command('exit')
    self.pytutor_trace['trace'] = self.trace
    print(str(self.pytutor_trace))
    open('/tmp/t.txt','w').write(str(self.pytutor_trace).replace('"','\\"').replace("'",'"').replace('True','true').replace('False','false'))

  def parse_sb_value(self, sb_value):
    value = sb_value.GetValue()
    type_ = sb_value.GetType()
    print (sb_value.GetName(), type_, type_.IsPointerType())
    if type_.IsPointerType() and int(value, 0) in self.heap:
      value = ["REF", int(value, 0)]
    elif type_ == type_.GetBasicType(lldb.eBasicTypeInt):
      value = int(value)
    return (sb_value.GetName(), value)

  def get_frame_description(self, frame, index):
    locals_ = {}
    sb_value_list = frame.GetVariables(1,1,0,0)
    for i in xrange(sb_value_list.GetSize()):
      sb_value = sb_value_list.GetValueAtIndex(i)
      if sb_value.GetName():
        (name, value) = self.parse_sb_value(sb_value)
        locals_[name] = value

    func_name = self.get_function_name(frame)

    desc = {}
    desc['frame_id'] = index + 1
    desc['encoded_locals'] = locals_
    desc['func_name'] = func_name
    desc['unique_hash'] = func_name + str(index)
    desc['ordered_varnames'] = locals_.keys()

    # ignore these fields
    desc['parent_frame_id_list'] = [] 
    desc['is_zombie'] = False 
    desc['is_parent'] = False
 
    return desc

  def get_stack_to_render(self, thread):
    frames = []
    num_frames = thread.GetNumFrames()
    for i in xrange(num_frames):
      frame = thread.GetFrameAtIndex(i)
      desc = self.get_frame_description(frame,i)
      desc['is_highlighted'] = (i == 0)
      frames += [desc]
      if desc['func_name'] == 'main':break
    return frames

  def get_globals(self, target):
    module = target.module_iter().next()
    globals_ = {}
    for sym in module:
      sb_value_list = target.FindGlobalVariables(sym.name,1)
      try:
        sb_value = sb_value_list.GetValueAtIndex(0)
        if sb_value.GetName():
          (name, value) = self.parse_sb_value(sb_value)
          globals_[name] = value
      except:
        print(("Unexpected error:", sys.exc_info()[0]))
    return globals_

  def get_function_name(self, frame):
    func_name = frame.GetFunctionName()
    if func_name == None:
      return ''
    else:
      return func_name

  def get_line_number(self):
    return self.dbg.GetSelectedTarget().GetProcess().GetSelectedThread().GetSelectedFrame().GetLineEntry().GetLine()

  def process_stdout(self, stdout, heap):
    ALLOC_TAG = 'Alloc = '
    FREE_TAG = 'free' 
    if stdout.startswith(ALLOC_TAG):
      fields = stdout.split()
      heap[int(fields[2],0)] = ["LIST"] + [0] * int(fields[4])
      return '\r\n'.join(stdout.split('\r\n')[1:])
    elif stdout.startswith(FREE_TAG):
      fields = stdout.split()
      del heap[int(fields[1],0)]
      return '\r\n'.join(stdout.split('\r\n')[1:])
    else:
      return stdout
  
  def dump_status(self, target):
    process = target.GetProcess()
    thread = process.GetSelectedThread()
    frame = thread.GetSelectedFrame()
    #print(frame.get_all_variables())
    #self.exec_command('fr v')
    #self.exec_command('ta v')

    stdout = self.process_stdout(process.GetSTDOUT(Trace.MAX_STDOUT), self.heap)
    self.stdout += stdout
    stack_to_render = self.get_stack_to_render(thread)
    globals_ = self.get_globals(target)
    ordered_globals = globals_.keys()
    line = self.get_line_number()
    event = thread.GetStopDescription(Trace.MAX_STDOUT)
    trace = {
      'ordered_globals' : ordered_globals, 
      'stdout' : self.stdout, 
      'func_name' : self.get_function_name(frame), 
      'stack_to_render' : stack_to_render, 
      'globals' : globals_, 
      'heap' : dict(self.heap), 
      'line' : line, 
      'event' : event, 
    };
    self.trace = self.trace + [trace]
    
  def exec_command(self, cmd):
    res = lldb.SBCommandReturnObject()
    self.ci.HandleCommand(cmd, res)
    if res.Succeeded():
      print('#' + res.GetOutput().strip() + '#')
    else:
      print(res.GetError().strip())
    return res

def main(argv):
  trace = Trace()
  trace.run(argv)

if __name__ == "__main__":
    main(sys.argv)
