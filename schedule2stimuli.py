#!/usr/bin/python

import csv
import pprint

p = 0

# read schedule (from SCRT)
schedule_f = 'schedule_' + str(p)
inf = open(schedule_f,'r')
for line in inf.readlines():
    line = line.rstrip()
    schedule = line.split(' ')
inf.close()

# allocate stimuli
a = 0
b = []
phase = ''
for session in range(1,36):
    print "%s" % session
    blocks   = ''
    previous = phase
    phase    = schedule[session - 1]
    if phase == 'B':
        if phase != previous:
            transition = session % 10
            b          = [transition]
            repeat     = 0
        if repeat == 3:
            b.append((b[-1] + 1) % 10)
            repeat = 0
        a = (b[-1] + 1) % 10
        repeat += 1
    else:
        a = session % 10
    print ',' . join(map(str,b))
    print str(a)
