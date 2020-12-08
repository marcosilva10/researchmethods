library(reticulate)
library(ggplot2)
library(tidyr)
library(dplyr)

#Produce Workloads, need function to run it changing parameters

info = capture.output(source("gen_workload.R"))
cat(info, file="testScript.txt", sep="\n")


#Run in 4 methods, need function to make it one line per file

system("py simulator.py --cpu-scheduler fcfs --input-file testScript.txt --output-file ResultFiles/testResultFCFS.txt")
system("py simulator.py --cpu-scheduler sjf --input-file testScript.txt --output-file ResultFiles/testResultSJF.txt")
system("py simulator.py --cpu-scheduler rr --quantum 1.0 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
system("py simulator.py --cpu-scheduler srtf --input-file testScript.txt --output-file ResultFiles/testResultSRTF.txt")

#Collect Results from each

tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
tableSJF = read.table("ResultFiles/testResultSJF.txt", header = TRUE, sep = "", dec = ".")
tableRR = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
tableSRTF = read.table("ResultFiles/testResultSRTF.txt", header = TRUE, sep = "", dec = ".")


#Process and make data, maybe plotting

meanTotalTime = c(mean(tableFCFS$tat), 
                     mean(tableSJF$tat), 
                     mean(tableRR$tat), 
                     mean(tableSRTF$tat))

meanWaitingTimes = c(mean(tableFCFS$ready_wait_time), 
          mean(tableSJF$ready_wait_time), 
          mean(tableRR$ready_wait_time), 
          mean(tableSRTF$ready_wait_time))

sdWaitingTimes = c(sd(tableFCFS$ready_wait_time), 
                     sd(tableSJF$ready_wait_time), 
                     sd(tableRR$ready_wait_time), 
                     sd(tableSRTF$ready_wait_time))


df = data.frame(meanTotalTime, meanWaitingTimes, sdWaitingTimes)
ggplot(df, aes(fill=c("fcfs", "sjf", "rr", "srtf"),y=df$meanTotalTime, x=df$meanWaitingTimes)) + 
       geom_bar(position="dodge", stat="identity")


# ============ Quantum time evaluation for RR Scheduling ===============
# The performance of RR is sensitive to the time quantum selected. 
# Most modern systems use time quantum between 10 and 100 milliseconds.
# This needs to be automated.
# ======================================================================
system("py simulator.py --cpu-scheduler rr --quantum 1.0 --input-file testScript.txt --output-file ResultFiles/testResultRRQuantum1.txt")
system("py simulator.py --cpu-scheduler rr --quantum 2.0 --input-file testScript.txt --output-file ResultFiles/testResultRRQuantum2.txt")
system("py simulator.py --cpu-scheduler rr --quantum 4.0 --input-file testScript.txt --output-file ResultFiles/testResultRRQuantum4.txt")
system("py simulator.py --cpu-scheduler rr --quantum 6.0 --input-file testScript.txt --output-file ResultFiles/testResultRRQuantum6.txt")
system("py simulator.py --cpu-scheduler rr --quantum 10.0 --input-file testScript.txt --output-file ResultFiles/testResultRRQuantum10.txt")

#Collect Results from each
tableRRQuantum1 = read.table("ResultFiles/testResultRRQuantum1.txt", header = TRUE, sep = "", dec = ".")
tableRRQuantum2 = read.table("ResultFiles/testResultRRQuantum2.txt", header = TRUE, sep = "", dec = ".")
tableRRQuantum4 = read.table("ResultFiles/testResultRRQuantum4.txt", header = TRUE, sep = "", dec = ".")
tableRRQuantum6 = read.table("ResultFiles/testResultRRQuantum6.txt", header = TRUE, sep = "", dec = ".")
tableRRQuantum10 = read.table("ResultFiles/testResultRRQuantum10.txt", header = TRUE, sep = "", dec = ".")

TurnAroundTimeRR = c(mean(tableRRQuantum1$tat), 
                     mean(tableRRQuantum2$tat), 
                     mean(tableRRQuantum4$tat), 
                     mean(tableRRQuantum6$tat), 
                     mean(tableRRQuantum10$tat))

plot(c(1,2,4,6,10), TurnAroundTimeRR,
     type = "b",
     col = "red",
     xlab = "Time Quantum",
     ylab = "Average turnaround time")

# Some statistical evaluation
# FCFS Results
barplot(tableSRTF$pid, tableSRTF$cpu_bursts_time,
     xlab = "Processes",
     ylab = "CPU Burst",
     main = "Order of submission")

barplot(tableSRTF$arrival_time, tableSRTF$ready_wait_time,
        xlab = "Arrivel Time",
        ylab = "time",
        main = "CPU Waiting Time")

barplot(tableSRTF$arrival_time, tableSRTF$io_wait_time,
        xlab = "Arrivel Time",
        ylab = "time",
        main = "IO Waiting Time")


