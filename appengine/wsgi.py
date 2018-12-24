#!/usr/bin/env python3
import subprocess
import atexit
import urllib
from urllib import request
import http.client

class Application(object):
	def __init__(self, *a):
		self.p = None

	def spawn(self):
		assert(not self.running())
		self.p = subprocess.Popen(['./app/bin/run.sh'])
		print(f'Spawned process: {self.p.pid}')

	def running(self):
		if self.p is None:
			return False
		return self.p.poll() is None

	def stop(self):
		if self.running():
			print(f'killing {self.p.pid}')
			self.p.kill()
			self.p.wait()

	def __call__(self, env, start_fn):
		if not self.running():
			self.spawn()

		# print(repr(env))
		body_stream = env['wsgi.input']
		headers = {}
		for k,v in env.items():
			if k.startswith('HTTP_'):
				http_key = k[5:].replace('_', '-')
				headers[http_key] = v
		# print(repr(headers))

		conn = http.client.HTTPConnection("localhost:2055")
		conn.request(env['REQUEST_METHOD'], env['RAW_URI'], body=body_stream, headers=headers)
		response = conn.getresponse()

		start_fn(f'{response.status} {response.version}', response.getheaders())
		try:
			while True:
				# print('reading chunk')
				chunk = response.read(1024)
				# print('> ' + repr(chunk))
				if not chunk:
					break
				else:
					yield chunk
		finally:
			response.close()

app = Application()
atexit.register(app.stop)
app.spawn()
