#!/bin/python3
import json

lines = []
with open('inputTU.txt') as raw:
    lines = json.loads(raw.read())

opps = {'[': ']', '{': '}', '(': ')', '<': '>'}


for line in lines:
    ends = []
    def parse(line):
        for c in line:
            if c in opps.keys(): parse()
