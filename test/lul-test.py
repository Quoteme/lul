#!/usr/bin/env python

from easyprocess import EasyProcess
from pyvirtualdisplay import Display
import os

current_dir = os.path.dirname(__file__)
os.putenv("PATH", current_dir+":"+os.getenv("PATH"))

print('starting test display')

with Display(visible=True) as disp:
	print("test-display started")
	with EasyProcess([current_dir+"/lul"]) as proc:
		proc.wait()
# vim: tabstop=2 shiftwidth=2 noexpandtab ft=python
