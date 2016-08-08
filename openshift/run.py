#!/usr/bin/env python
from __future__ import print_function
import os, sys
import time
import subprocess
from signal import SIGTERM, SIGINT, SIGKILL

assert len(sys.argv) == 1, "Too many args"

pid = os.getpid()

with open(os.environ['PID_PATH'], 'w') as pidfile:
	pidfile.write('%s\n' % pid)

# note: we need to explicitly invoke lib/ld because RHEL's ld-linux.so is usually too old
def export(name, var):
	print('+ export %s=%s' % (name, var))
	os.environ[name]=var

export('LD_LIBRARY_PATH', os.environ['OPENSHIFT_REPO_DIR'] + '/lib')
export('OCAMLRUNPARAM', 'b')
while True:
	print('\n---------------------------------\n*** Start: %s - PID %s' % (time.ctime(), pid,))
	proc = subprocess.Popen(
		[
			os.environ['OPENSHIFT_REPO_DIR'] + '/lib/ld.so',
			os.environ['OPENSHIFT_REPO_DIR'] + '/app/bin/passe-server',
			'--port', os.environ['SERVER_PORT'],
			'--host', os.environ['OPENSHIFT_DIY_IP'],
			'--root', os.environ['OPENSHIFT_REPO_DIR'] + '/docroot',
			'--data', os.environ['OPENSHIFT_DATA_DIR'],
			'--timestamp',
			'-vv',
	])
	time.sleep(2)
	if proc.poll() is not None:
		print("Error: process died within first 2 seconds. Exiting")
		sys.exit(proc.poll())
	code = proc.wait()
	signal = None
	if code < 0:
		signal = -code
		code = None
	print('ERROR: proc returned exit status %s, signal %s' % (code, signal))
	if signal in (SIGTERM, SIGINT, SIGKILL):
		sys.exit(1)
