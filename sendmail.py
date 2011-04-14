#!/usr/bin/python

from optparse import OptionParser
import os,sys,email,smtplib

debug = 0

def getServer():
	if options.server:
		return options.server
	else:
		return '159.226.39.7'
def nub(l):
	d = {}
	for i in l:d[i] = 0
	l = d.keys()

def getFrom():
	f = options.from_addr
	if not f:
		f = os.environ['USER'] + '@' + os.environ['HOSTNAME']
	else:f = f.strip()
	return f

def getSubject():
	f = options.subject
	if f:return f.strip()
	else:return ''

def getContent(prefix):
	content = prefix
	if options.content:content += options.content
	elif options.contentFilename:content += open(options.contentFilename,'rb').read()
	content = content.replace(':',';')
	print content
	try:
	    msg = email.message_from_string(content)
	except:
		msg = email.message_from_string(prefix+'ATT')
	return msg
def send_to(you,cc):
	server=getServer()
	s = smtplib.SMTP(server)
	#s = smtplib.SMTP('webmail.ict.ac.cn')
	if debug: s.set_debuglevel(1)
#	prefix = 'Subject:%s\nCC:%s\n'%(getSubject(),';'.join(cc))
	msg=getContent('')
	msg['Subject'] = getSubject ()
	msg['CC'] = ';'.join(cc)
	print msg.as_string()
	print '--------'
#s.connect()
	s.sendmail(getFrom(), [you], msg.as_string())
#s.close()
	s.quit()
		
usage = "Usage: %prog [-v server] [-f from_addr] [-s subject] [-c content] receivers"
parser = OptionParser(usage)
parser.add_option("-f", "--from", dest="from_addr", help="sender of the mail")
parser.add_option("-s", "--subject", dest="subject", help="subject of the mail")
parser.add_option("-c", "--content", dest="content", help="content of the mail")
parser.add_option("-n", "--contentFilename", dest="contentFilename", help="content file of the mail")
parser.add_option("-v", "--server", dest="server", help="mail server")
(options, args) = parser.parse_args()
if debug:
	print options.from_addr
	print options.subject
	print args

nub(args)
send_to(args,args)
