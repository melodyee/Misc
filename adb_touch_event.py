s1='''/dev/input/event2: 0003 0039 000038d2
/dev/input/event2: 0003 0035 0000032c
/dev/input/event2: 0003 0036 00000847
/dev/input/event2: 0003 003a 00000032
/dev/input/event2: 0003 0030 00000004
/dev/input/event2: 0000 0000 00000000
/dev/input/event2: 0003 0035 0000032b
/dev/input/event2: 0003 0036 00000846
/dev/input/event2: 0003 003a 00000033
/dev/input/event2: 0000 0000 00000000'''
s2='''/dev/input/event2: 0003 0039 ffffffff
/dev/input/event2: 0000 0000 00000000
/dev/input/event2: 0003 0039 000038d3
/dev/input/event2: 0003 0035 00000350
/dev/input/event2: 0003 0036 000008af
/dev/input/event2: 0000 0000 00000000
/dev/input/event2: 0003 0035 00000351
/dev/input/event2: 0003 0036 000008ad
/dev/input/event2: 0003 003a 00000034
/dev/input/event2: 0000 0000 00000000'''
def exe(s):
	cmds = []
	for line in s.split('\n'):
		cmd = 'adb shell sendevent '
		fields = line.split()
		cmd += fields[0][:-1]
		cmd += ' ' + ' '.join([str(int('0x' + s, 0)) for s in fields[1:]])
		cmds += [cmd]
	for cmd in cmds:
		print cmd
		os.system(cmd)
		os.system('sleep 0.001')
import os
exe(s1)
for i in xrange(5):
	exe(s2)
		
