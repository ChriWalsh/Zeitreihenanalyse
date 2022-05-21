"""
Script to install required python packages 
heavily copy-pasted from 
https://www.activestate.com/resources/quick-reads/how-to-install-python-packages-using-a-script/
"""
import sys
import subprocess

# Updating pip:
subprocess.check_call([sys.executable, '-m', 'pip', 'install', '--upgrade', 'pip'])
# Installing modules from reqirements.txt:
subprocess.check_call([sys.executable, '-m', 'pip', 'install', '-r', 'requirements.txt'])

