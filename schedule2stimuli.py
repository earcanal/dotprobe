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

# allocate stimuli and write csv
a       = 0
b       = []
phase   = ''
csvfile = open('stimuli_' + str(p) + '.csv', 'wb')
writer  = csv.writer(csvfile, delimiter=',')
for session in range(1,36):
    writer.writerow([session])
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
    writer.writerow(b)
    writer.writerow([a])
