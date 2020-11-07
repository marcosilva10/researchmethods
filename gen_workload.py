#!/usr/bin/env python3

# Synopsis
#           ./gen_workload.py num_procs mean_io_bursts mean_iat min_CPU max_CPU min_IO max_IO
# Description
#   Generate workload for CPU scheduler simulation.
#   Interarrival times follow an exponential distribution with mean lambda.
#   CPU and I/O bursts 
#
# Workload format: one line per process, each containing a sequence of
# floating-point numbers of even length. In each line, the first number
# represents the arrival time of the process, and the remaining numbers
# represent the length of the CPU and I/O bursts that result from running
# the process. Since the processes must start and end with a CPU burst, the
# total number of bursts must be odd (and the number of numbers in each line
# must be even).

import sys
import numpy as np

# FIXME
if len(sys.argv) == 8:
    num_procs = int(sys.argv[1])
    mean_io_bursts = int(sys.argv[2])
    mean_iat = float(sys.argv[3])
    min_CPU = float(sys.argv[4])
    max_CPU = float(sys.argv[5])
    min_IO = float(sys.argv[6])
    max_IO = float(sys.argv[7])
elif len(sys.argv) == 1: # default values
    print("# No arguments supplied, using default parameter values.")
    num_procs = 50
    mean_io_bursts = 10
    mean_iat = 25
    min_CPU = 1.
    max_CPU = 2.
    min_IO = 0.3
    max_IO = 0.5
else:
    raise Exception("The number of arguments should be 7.")

print("# num_procs = %d" % num_procs)
print("# mean_io_bursts = %g" % mean_io_bursts)
print("# mean_iat = %d" % mean_iat)
print("# min_CPU = %g" % min_CPU)
print("# max_CPU = %g" % max_CPU)
print("# min_IO = %g" % min_IO)
print("# max_IO = %g" % max_IO)

t = 0.

for i in range(num_procs):
    t += np.random.exponential(mean_iat)
    print(t, end=' ')
    io_bursts = np.random.poisson(mean_io_bursts) # Why Poisson? Why not?
    for j in range(io_bursts):
        print(np.random.uniform(min_CPU, max_CPU), end=' ')
        print(np.random.uniform(min_IO, max_IO), end=' ')
    print(np.random.uniform(min_CPU, max_CPU))

