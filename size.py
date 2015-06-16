#!/usr/bin/python

# calculate the number of pixels for a stimulus
# fixed: viewer distance, vertical resolution, visual angle
# argv[1] = vertical screen height

from math import atan2, degrees
import sys

if sys.argv[1]:
    h = float(sys.argv[1])
else:
    h = 21.5         # Dell laptop
    h = 20.6         # Macbook Pro
    h = 28.7         # Dell monitor

d              = 60  # distance between monitor and participant in cm
r              = 768 # vertical resolution of monitor
size_in_px     = 0   # stimulus size in pixels
size_in_deg    = 0
target_degrees = 2

# calculate the number of degrees that correspond to a single pixel. This will
# generally be a very small value, something like 0.03.
deg_per_px = degrees(atan2(.5*h, d)) / (.5*r)
print '%s degrees correspond to a single pixel' % deg_per_px

# calculate the size of the stimulus in degrees
while size_in_deg < target_degrees:
    size_in_px += 1
    size_in_deg = size_in_px * deg_per_px
print 'The size of the stimulus is %s pixels and %s visual degrees' % (size_in_px, size_in_deg)
