#!/usr/bin/python

import csv
import pprint
import pdb

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
phase   = 'A'
schedfile = 'stimuli_' + str(p) + '.csv'
csvfile = open(schedfile, 'wb')
writer  = csv.writer(csvfile, delimiter=',')
for session in range(0,35):
    writer.writerow([session])
    blocks   = ''
    previous = phase
    if session > 0: phase = schedule[session - 1]
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
csvfile.close()

# read csv
a = {}
b = {}
with open(schedfile,'rb') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    c = 0
    for row in reader:
        if c == 0:
            session = int(row[0])
            if session == transition: print "*** transition A -> B ***"
        elif c == 1:
            #pdb.set_trace()
            b[session] = map(int,row)
        else:
            a[session] = int(row[0])
        c += 1
        if c == 3:
            print 'B' + str(session) + ': ' + ',' . join(map(str,b[session]))
            print "A%d: %d" % (session, a[session])
            c = 0
