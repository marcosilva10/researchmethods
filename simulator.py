#!/usr/bin/env python3
#
# Simulator for a CPU and I/O scheduling system assuming a single
# CPU and I/O. The CPU scheduler uses one of the following
# scheduling algorithms: First Come First Served (FCFS), Round Round
# (RR), Shortest Job First (SJF), and Shortest Remaining Time First
# (SRTF). Note that the last two require knowing the burst sizes in
# advance, which is not realistic in computer process scheduling. The
# I/O scheduler always follows a FCFS mechanism.
#
# Workload format: one line per process, each containing a sequence of
# floating-point numbers of even length. In each line, the first
# number represents the arrival time of the process, and the remaining
# numbers represent the length of the CPU and I/O bursts that result
# from running the process. Since the processes must start and end
# with a CPU burst, the total number of bursts must be odd (and the
# number of numbers in each line must be even).
#
# Output format: one line per process, each containing a sequence of
# numbers separated by spaces. The first number gives the process id
# (defined by the order in the workload file). The second number is
# the arrival time of the process. Then the next three numbers give
# the sum of all cpu bursts, the sum of all io bursts, and the sum all
# bursts respectively. The last three values given the Turn Around
# Time (TAT), i.e. the wall clock time, the Ready wait time, i.e. the
# time the process spent in the CPU scheduling queue ready to run, and
# the I/O wait time, i.e. the time the process spent in I/O.

import sys
import argparse
import salabim as sim
import numpy as np

def read_workload(file):
    if file is None:
        file = sys.stdin
    else:
        file = open(file, "r")

    pid = 0
    procs = []
    with file as f:
        l = f.readline()
        while l:
            if l[0] != "#":
                vals = [float(x) for x in l.split()]
                procs.append(Process(pid = pid, arrival = vals[0], bursts = vals[1:]))
                pid += 1
            l = f.readline()
    return procs

class Process:
    def __init__(self, pid, arrival, bursts):
        self.pid = pid
        self.arrival = arrival
        self.bursts = bursts

class Simulator:
    def __init__(self, processes, cpu_scheduler, quantum = None, ofile = None):
        if cpu_scheduler == "rr" and quantum is None:
            raise ValueError("Quantum parameter is required for round robin")
        if quantum is not None and quantum <= 0:
            raise ValueError("Quantum parameter needs to be a positive (non-zero) value")

        self.cpu_scheduler = cpu_scheduler
        self.quantum = quantum # for round robin scheduler

        self.processes = processes
        processes.sort(key = lambda x: x.arrival)

        self.ofile = ofile
        self.of = None

    def run(self):
        self.of = sys.stdout if self.ofile is None else open(self.ofile, "w")

        print("# Cpu scheduler: %s" % self.cpu_scheduler, file = self.of)
        print("# Quantum: %s" % self.quantum, file = self.of)
        print("pid arrival_time cpu_bursts_time io_bursts_time bursts_time tat ready_wait_time io_wait_time", file = self.of)

        self.env = sim.Environment(trace = False)
        self.cpu = sim.Resource("CPU", capacity = 1, preemptive = self.cpu_scheduler == "srtf", env = self.env)
        self.io = sim.Resource("I/O", capacity = 1, env = self.env)
        ProcessArrival(simulator = self)
        self.env.run()

        if self.of is not None and self.of != sys.stdout:
            self.of.close()
            self.of = None

class ProcessArrival(sim.Component):
    def setup(self, simulator):
        self.simulator = simulator

    def process(self):
        for p in self.simulator.processes:
            yield self.hold(till = p.arrival)
            ProcessComponent(simulator = self.simulator, pid = p.pid, arrival = p.arrival, bursts = p.bursts)

class ProcessComponent(sim.Component):
    def setup(self, simulator, pid, arrival, bursts):
        self.simulator = simulator
        self.pid = pid
        self.arrival = arrival
        self.bursts = bursts
        self.ready_wait_time = 0
        self.io_wait_time = 0

    def process(self):
        b = self.bursts
        clock_start = self.simulator.env.now()

        for i in range(1, len(b), 2):
            yield from self.__schedule_cpu_burst(b[i-1])
            yield from self.__schedule_io_burst(b[i])
        yield from self.__schedule_cpu_burst(b[-1])

        tat = self.simulator.env.now() - clock_start
        print(self.pid, end = " ", file = self.simulator.of)
        print(self.arrival, end = " ", file = self.simulator.of)
        print(np.sum(b[0:len(b):2]), end = " ", file = self.simulator.of)
        print(np.sum(b[1:len(b):2]), end = " ", file = self.simulator.of)
        print(np.sum(b), end = " ", file = self.simulator.of)
        print(tat, end = " ", file = self.simulator.of)
        print(self.ready_wait_time, end = " ", file = self.simulator.of)
        print(self.io_wait_time, end = "\n", file = self.simulator.of)

    def __schedule_cpu_burst(self, burst):
        if self.simulator.cpu_scheduler == "fcfs":
            yield from self.__queue_cpu(self.simulator.cpu)
            yield self.hold(duration = burst)
            self.release(self.simulator.cpu)
        elif self.simulator.cpu_scheduler == "rr":
            s = burst
            while s > self.simulator.quantum:
                yield from self.__queue_cpu(self.simulator.cpu)
                yield self.hold(duration = self.simulator.quantum)
                self.release(self.simulator.cpu)
                s -= self.simulator.quantum
            yield from self.__queue_cpu(self.simulator.cpu)
            yield self.hold(duration = s)
            self.release(self.simulator.cpu)
        elif self.simulator.cpu_scheduler == "sjf":
            yield from self.__queue_cpu((self.simulator.cpu, 1, burst))
            yield self.hold(duration = burst)
            self.release(self.simulator.cpu)
        elif self.simulator.cpu_scheduler == "srtf":
            s = burst
            while True:
                yield from self.__queue_cpu((self.simulator.cpu, 1, s))
                yield self.hold(duration = s, mode = "")
                if not self.isbumped():
                    break
                s -= self.simulator.env.now() - self.mode_time()
                yield self.standby()
            self.release(self.simulator.cpu)
        else:
            raise ValueError("Unknown cpu_scheduler")

    def __queue_cpu(self, arg):
        ready_wait_start = self.simulator.env.now()
        yield self.request(arg)
        self.ready_wait_time += self.simulator.env.now() - ready_wait_start

    def __schedule_io_burst(self, burst):
        io_start = self.simulator.env.now()
        yield self.request(self.simulator.io)
        yield self.hold(duration = burst)
        self.release(self.simulator.io)
        self.io_wait_time += self.simulator.env.now() - io_start

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description = "CPU and I/O scheduling simulator")
    parser.add_argument("--cpu-scheduler", choices = ["fcfs", "rr", "sjf", "srtf"], required = True, help = "CPU scheduler")
    parser.add_argument("--quantum", type=float, default = None, help = "Quantum paramater (required only by round robin cpu scheduler)")
    parser.add_argument("--input-file", metavar = "FILE", default = None, help = "Input file, if it is not the data is read from stdin")
    parser.add_argument("--output-file", metavar = "FILE", default = None, help = "Output file, if it is not set the data is printed to stdout")

    args = parser.parse_args(sys.argv[1:])
    output_file = sys.stdout if args.output_file is None else args.output_file

    processes = read_workload(file = args.input_file)

    simulator = Simulator(processes = processes, cpu_scheduler = args.cpu_scheduler, quantum = args.quantum, ofile = args.output_file)
    simulator.run()
