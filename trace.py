#!/usr/bin/env python
from __future__ import print_function

import lldb, sys
import logging

class Trace(object) :

  MAX_STDOUT = 100

  def __init__(self):
    self.dbg = lldb.SBDebugger.Create()
    self.dbg.SetAsync(False)
    lldb.debugger = self.dbg

    self.ci = self.dbg.GetCommandInterpreter()
    self.pytutor_trace = {}
    self.trace = []
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
    while succeeded:
      self.dump_status(target)
      succeeded = self.exec_command('n').Succeeded()
    self.exec_command('exit')
    self.pytutor_trace['trace'] = self.trace
    print(str(self.pytutor_trace))
    open('/tmp/t.txt','w').write(str(self.pytutor_trace).replace('"','\\"').replace("'",'"').replace('True','true').replace('False','false'))

  def get_frame_description(self, frame, index):
    locals_ = {}
    sb_value_list = frame.GetVariables(1,1,0,0)
    for i in xrange(sb_value_list.GetSize()):
      sb_value = sb_value_list.GetValueAtIndex(i)
      if sb_value.GetName():
        locals_[sb_value.GetName()] = sb_value.GetValue()

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
      desc['is_highlighted'] = (i == num_frames - 1)
      frames += [desc]
    return frames

  def get_globals(self, target):
    module = target.module_iter().next()
    globals_ = {}
    for sym in module:
      sb_value_list = target.FindGlobalVariables(sym.name,1)
      try:
        sb_value = sb_value_list.GetValueAtIndex(0)
        if sb_value.GetName():
          globals_[sb_value.GetName()] = sb_value.GetValue()
      except:
        print(("Unexpected error:", sys.exc_info()[0]))
    return globals_

  def get_function_name(self, frame):
    func_name = frame.GetFunctionName()
    if func_name == None:
      return ''
    else:
      return func_name
    
  def dump_status(self, target):
    process = target.GetProcess()
    thread = process.GetSelectedThread()
    frame = thread.GetSelectedFrame()
    print(frame.get_all_variables())
    #self.exec_command('fr v')
    #self.exec_command('ta v')

    self.stdout += process.GetSTDOUT(Trace.MAX_STDOUT)
    stack_to_render = self.get_stack_to_render(thread)
    globals_ = self.get_globals(target)
    ordered_globals = globals_.keys()
    heap = {}
    print(frame)
    print(frame.GetLineEntry())
    line = frame.GetLineEntry().GetLine()
    print(line)
    event = thread.GetStopDescription(Trace.MAX_STDOUT)
    trace = {
      'ordered_globals' : ordered_globals, 
      'stdout' : self.stdout, 
      'func_name' : self.get_function_name(frame), 
      'stack_to_render' : stack_to_render, 
      'globals' : globals_, 
      'heap' : heap, 
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
