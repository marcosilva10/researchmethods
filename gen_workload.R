#!/usr/bin/env Rscript

# Synopsis
#   ./gen_workload.R num_procs mean_io_bursts mean_iat min_CPU max_CPU min_IO max_IO
#
# Description
#   Generate workload for CPU scheduler simulation.
#   Interarrival times follow an exponential distribution with mean lambda.
#   CPU and I/O bursts
#
# Workload format: one line per process, each containing a sequence of
# floating-point numbers of even length. In each line, the first number
# represents the arrival time of the process, and the remaining numbers
# represent the length of the CPU and I/O bursts that result from running the
# process. Since the processes must start and end with a CPU burst, the total
# number of bursts must be odd (and the number of numbers in each line must be
# even).

args <- commandArgs(trailingOnly = TRUE)

# Setting some default values
num_procs <- 50
mean_io_bursts <- 10
mean_iat <- 25
min_CPU <- 1.0
max_CPU <- 2.0
min_IO <- 0.3
max_IO <- 0.5

if(length(args) == 7) {
  num_procs <- as.numeric(args[1])
  mean_io_bursts <- as.numeric(args[2])
  mean_iat <- as.numeric(args[3])
  min_CPU <- as.numeric(args[4])
  max_CPU <- as.numeric(args[5])
  min_IO <- as.numeric(args[6])
  max_IO <- as.numeric(args[7])
} else if (length(args) == 0) {
  cat("# No arguments supplied, using default parameter values.\n")
} else {
  stop("The number of arguments should be 7.\n")
}

cat(sprintf("# num_procs = %d\n", num_procs))
cat(sprintf("# mean_io_bursts = %g\n", mean_io_bursts))
cat(sprintf("# mean_iat = %g\n", mean_iat))
cat(sprintf("# min_CPU = %g\n", min_CPU))
cat(sprintf("# max_CPU = %g\n", max_CPU))
cat(sprintf("# min_IO = %g\n", min_IO))
cat(sprintf("# max_IO = %g\n", max_IO))

times <- cumsum(rexp(num_procs, rate = 1 / mean_iat))
num_io_bursts <- rpois(num_procs, lambda = mean_io_bursts)

interleave <- function(a, b) {
  return(c(a, b)[order(c(seq_along(a), seq_along(b)))])
}

for(i in seq(num_procs)) {
  cpu_bursts <- runif(num_io_bursts[i]+1, min = min_CPU, max = max_CPU)
  io_bursts <- runif(num_io_bursts[i], min = min_IO, max = max_IO)
  bursts <- interleave(cpu_bursts, io_bursts)
  bursts_s <- paste(bursts, collapse=" ")
  cat(sprintf("%g %s\n", times[i], bursts_s))
}
