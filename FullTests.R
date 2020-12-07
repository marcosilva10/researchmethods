
library(reticulate)
library(ggplot2)
library(tidyr)
library(dplyr)

#Produce Workloads, need function to run it changing parameters

info = capture.output(source("gen_workload.R"))
cat(info, file="testScript.txt", sep="\n")

#Run in 4 methods, need function to make it one line per file

system("py simulator.py --cpu-scheduler fcfs --input-file testScript.txt --output-file testResultFCFS.txt")
system("py simulator.py --cpu-scheduler sjf --input-file testScript.txt --output-file testResultSJF.txt")
system("py simulator.py --cpu-scheduler rr --quantum 1.0 --input-file testScript.txt --output-file testResultRR.txt")
system("py simulator.py --cpu-scheduler srtf --input-file testScript.txt --output-file testResultSRTF.txt")

#Collect Results from each

tableFCFS = read.table("testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
tableSJF = read.table("testResultSJF.txt", header = TRUE, sep = "", dec = ".")
tableRR = read.table("testResultRR.txt", header = TRUE, sep = "", dec = ".")
tableSRTF = read.table("testResultSRTF.txt", header = TRUE, sep = "", dec = ".")


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

#Some statistical evaluation


