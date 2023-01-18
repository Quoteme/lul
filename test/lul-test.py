#!/usr/bin/env python

from easyprocess import EasyProcess
from pyvirtualdisplay import Display
import subprocess
import os

current_dir = os.path.dirname(__file__)
os.putenv("PATH", current_dir+":"+os.getenv("PATH"))

print('starting test display')

with Display(visible=True) as disp:
	print("test-display started")
	print(current_dir+"/lul")
	# with EasyProcess([current_dir+"/lul"]) as proc:
	# 	proc.wait()
	subprocess.call([current_dir+"/lul"])
# vim: tabstop=2 shiftwidth=2 noexpandtab ft=python
