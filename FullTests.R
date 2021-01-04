library(reticulate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape)

#Produce Workloads, need function to run it changing parameters
#Defaul and initial values
NumProcs <- 50
MeanIoBursts <- 10
MeanIat <- 25
MinCPU <- 1.0
MaxCPU <- 2.0
MinIO <- 0.3
MaxIO <- 0.5

#Function to generate workload
gen_workload <- function (NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO){
        
        cat("# == Generating Workload == \n")
        cat(sprintf("# num_procs = %d\n", NumProcs))
        cat(sprintf("# mean_io_bursts = %g\n", MeanIoBursts))
        cat(sprintf("# mean_iat = %g\n", MeanIat))
        cat(sprintf("# min_CPU = %g\n", MinCPU))
        cat(sprintf("# max_CPU = %g\n", MaxCPU))
        cat(sprintf("# min_IO = %g\n", MinIO))
        cat(sprintf("# max_IO = %g\n", MaxIO))
        cat("# ===================\n")
        
        createTestStr =  paste("Rscript gen_workload.R", NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO, sep = " ")
        info =  system(createTestStr, intern = TRUE)
        cat(info, file="testScript.txt", sep="\n")  
        invisible(info)
}

#Function to execute simulator.py
exec_simulator <- function(){
        
        cat("# == Execution simulator ==\n")
        
        #Run in 4 methods, need function to make it one line per file
        #C:/Users/marco/anaconda3/envs/rstudio/python.exe -> dont delete pls
        system("py simulator.py --cpu-scheduler fcfs --input-file testScript.txt --output-file ResultFiles/testResultFCFS.txt")
        system("py simulator.py --cpu-scheduler sjf  --input-file testScript.txt --output-file ResultFiles/testResultSJF.txt")
        system("py simulator.py --cpu-scheduler rr   --quantum 1.0 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
        system("py simulator.py --cpu-scheduler srtf --input-file testScript.txt --output-file ResultFiles/testResultSRTF.txt")
        
}

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

barplot(tableSRTF$tat, tableSRTF$ready_wait_time,
        xlab = "total time",
        ylab = "ready WT",
        main = "IO Waiting Time")

barplot(tableSRTF$tat, tableSRTF$io_wait_time,
        xlab = "Arrivel Time",
        ylab = "io WT",
        main = "IO Waiting Time")

barplot(tableSJF$tat, tableSJF$ready_wait_time,
        xlab = "total time",
        ylab = "ready WT",
        main = "IO Waiting Time")

barplot(tableSJF$tat, tableSJF$io_wait_time,
        xlab = "Arrivel Time",
        ylab = "io WT",
        main = "IO Waiting Time")

barplot(tableFCFS$tat, tableFCFS$ready_wait_time,
        xlab = "total time",
        ylab = "ready WT",
        main = "IO Waiting Time")

barplot(tableFCFS$tat, tableFCFS$io_wait_time,
        xlab = "Arrivel Time",
        ylab = "io WT",
        main = "IO Waiting Time")

barplot(tableRR$tat, tableRR$ready_wait_time,
        xlab = "total time",
        ylab = "ready WT",
        main = "IO Waiting Time")

barplot(tableRR$tat, tableRR$io_wait_time,
        xlab = "Arrivel Time",
        ylab = "io WT",
        main = "IO Waiting Time")

#===========================
par(mfrow=c(2,2))

ymax  = 200
xmmax = 100
xleg = "Bursts Time"
yleg = "Total active time"

plotRRWaitingTime <- plot(tableRR$bursts_time, tableRR$tat,
                     xlab = xleg,
                     ylab = yleg,
                     main = "Round Robin",
                     ylim = c(0, ymax),
                     xlim = c(0, xmmax))
plotFCFSWaitingTime <- plot(tableFCFS$bursts_time, tableFCFS$tat,
                     xlab = xleg,
                     ylab = yleg,
                     main = "First come First Served",
                     ylim = c(0, ymax),
                     xlim = c(0, xmmax))
plotSJFWaitingTime <- plot(tableSJF$bursts_time, tableSJF$tat,
                     xlab = xleg,
                     ylab = yleg,
                     main = "S JOB FIRST",
                     ylim = c(0, ymax),
                     xlim = c(0, xmmax))
plotSRTFWaitingTime <- plot(tableSRTF$bursts_time, tableSRTF$tat,
                     xlab = xleg,
                     ylab = yleg,
                     main = "Shortest ready JOB FIRST",
                     ylim = c(0, ymax),
                     xlim = c(0, xmmax))


#==========================================================
# Designed to evaluate the waiting time
# according to the number of processes
#==========================================================

meanWaitingTimes <- c()
num_runs = 10
tmp = c(1,2,3,4)

aux <- 1
for (i in seq(1, by=length(tmp), length=num_runs)){

        eval_parameter <- aux 
    
        gen_workload(eval_parameter, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO)
        exec_simulator()
        
        #Collect Results from each
        cat("# == collecting results ==\n ")
        tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
        tableSJF  = read.table("ResultFiles/testResultSJF.txt", header = TRUE, sep = "", dec = ".")
        tableRR   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
        tableSRTF = read.table("ResultFiles/testResultSRTF.txt", header = TRUE, sep = "", dec = ".")

        tmp = c(mean(tableFCFS$ready_wait_time), 
                mean(tableSJF$ready_wait_time), 
                mean(tableRR$ready_wait_time), 
                mean(tableSRTF$ready_wait_time))
        
        meanWaitingTimes[seq(i, length=length(tmp))] = tmp;
        
        aux <- aux + 1 
}

dim(meanWaitingTimes) = c(length(tmp),num_runs)

colnames(meanWaitingTimes) <- paste0("", 1:num_runs)              # column names
meanWaitingTimes <- cbind(meanWaitingTimes, Mean = rowMeans(meanWaitingTimes[,]))    # column mean
rownames(meanWaitingTimes) <- paste0(c("FCFS", "SJF", "RR", "SRTF")) # row names

#barplot:
barplot(meanWaitingTimes, beside = T,  legend = TRUE, col = c("red","green", "yellow", "blue"),
        xlab = "Processes",
        ylab = "Average Waiting time (ms)",
        main = "Waiting time")

#==========================================================
# Designed to evaluate the average turnaround time
# according to the number of processes
#==========================================================

meanTatTimes <- c()
num_runs = 100
tmp = c(1,2,3,4)

aux <- 1
for (i in seq(1, by=length(tmp), length=num_runs)){
        
        eval_parameter <- aux 
        
        gen_workload(eval_parameter, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO)
        exec_simulator()
        
        #Collect Results from each
        cat("# == collecting results ==\n ")
        tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
        tableSJF  = read.table("ResultFiles/testResultSJF.txt", header = TRUE, sep = "", dec = ".")
        tableRR   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
        tableSRTF = read.table("ResultFiles/testResultSRTF.txt", header = TRUE, sep = "", dec = ".")
        
        tmp = c(mean(tableFCFS$tat), 
                mean(tableSJF$tat), 
                mean(tableRR$tat), 
                mean(tableSRTF$tat))
        
        meanTatTimes[seq(i, length=length(tmp))] = tmp;
        
        aux <- aux + 1 #Change to exponential?
}

dim(meanTatTimes) = c(length(tmp),num_runs)

colnames(meanTatTimes) <- paste0("", 1:num_runs)              # column names
rownames(meanTatTimes) <- paste0(c("FCFS", "SJF", "RR", "SRTF")) # row names

#line plot:
mt = as.data.frame(t(meanTatTimes))
rownames(mt)=c("FCFS", "SJF", "RR", "SRTF")
mt$processes <- rownames(mt)
mt      <- melt(mt, id.vars=c("processes"))

ggplot(mt, aes(x=as.numeric(processes), y=value, color=variable)) + 
    geom_line(size = 1) + geom_point(fill = "white") + 
    scale_shape_manual(values = c(22, 21))  + xlab("Processes") + 
    ylab("Average Turnaround Time") + ggtitle("Average Turnaround Time")

