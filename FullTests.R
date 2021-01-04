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

# ============= Collect Results from each ============
tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
tableSJF  = read.table("ResultFiles/testResultSJF.txt", header = TRUE, sep = "", dec = ".")
tableRR   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
tableSRTF = read.table("ResultFiles/testResultSRTF.txt", header = TRUE, sep = "", dec = ".")

# ========== CPU Burst VS Waiting Time ========
# = compare long jobs with shorter ones
# = Part 1 - COmpare a mix of short and long jobs 
# = Part 2 - fewer processes but longer
# = Should this be done for each algorithm individually?==

# ==== FCFS 

# || PART 1 ||
NumProcs <- 100
MeanIoBursts <- 10
MeanIat <- 25
MinCPU <- 1.0
MaxCPU <- 2.0
MinIO <- 0.3
MaxIO <- 0.5
gen_workload(NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO)
exec_simulator()
tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")

df = data.frame(cpu_bursts_time = tableFCFS$cpu_bursts_time, ready_wait_time = tableFCFS$ready_wait_time)
df$data <- 'FCFS'
# Cpu Burst Distibution
ggplot(df, aes(x = 1:nrow(df), y = cpu_bursts_time))+
    geom_line(aes(colour = data), size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="Process", breaks=seq(0,400,20)) +
    scale_y_continuous(name="CPU Total Burst (sec)", breaks=seq(0,100,5)) + 
    ggtitle("Cpu Burst Distibution")+
    theme_bw()
# Waiting Time
ggplot(df, aes(x = cpu_bursts_time, y = ready_wait_time))+
    geom_line(aes(colour = data), size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="CPU Burst", breaks=seq(0,50,1)) +
    scale_y_continuous(name="Waiting Time (sec)", breaks=seq(0,100,5)) + 
    ggtitle("Waiting Time")+
    theme_bw()

# || Part 2 ||
NumProcs <- 10
MeanIoBursts <- 10
MeanIat <- 25
MinCPU <- 1.0
MaxCPU <- 10.0
MinIO <- 0.3
MaxIO <- 0.5
gen_workload(NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO)
exec_simulator()
tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")

df = data.frame(cpu_bursts_time = tableFCFS$cpu_bursts_time, ready_wait_time = tableFCFS$ready_wait_time)
df$data <- 'FCFS'
# Cpu Burst Distibution
ggplot(df, aes(x = 1:nrow(df), y = cpu_bursts_time))+
    geom_line(aes(colour = data), size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="Process", breaks=seq(0,400,20)) +
    scale_y_continuous(name="CPU Total Burst (sec)", breaks=seq(0,100,5)) + 
    ggtitle("Cpu Burst Distibution")+
    theme_bw()
# Waiting Time
ggplot(df, aes(x = cpu_bursts_time, y = ready_wait_time))+
    geom_line(aes(colour = data), size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="CPU Burst", breaks=seq(0,100,5)) +
    scale_y_continuous(name="Waiting Time (sec)", breaks=seq(0,300,15)) + 
    ggtitle("Waiting Time")+
    theme_bw()
#========= Compare same workload with SJF results =========
# SJF scheduling tends to result in increased waiting times

tableSJF = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")

# turnaround time
df1 = data.frame(cpu_bursts_time = tableFCFS$cpu_bursts_time, tat = tableFCFS$tat)
df2 = data.frame(cpu_bursts_time = tableSJF$cpu_bursts_time, tat = tableSJF$tat)
df1$data <- 'FCFS'
df2$data <- 'SJF'

df <- rbind.data.frame(df1,df2)

ggplot(df, aes(x = cpu_bursts_time, y = tat))+
    geom_line(aes(colour = data), size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="CPU Burst", breaks=seq(0,100,5)) +
    scale_y_continuous(name="Time (sec)", breaks=seq(0,300,15)) + 
    ggtitle("Turnaround Time")+
    theme_bw()

# waiting time

df1 = data.frame(cpu_bursts_time = tableFCFS$cpu_bursts_time, ready_wait_time = tableFCFS$ready_wait_time)
df2 = data.frame(cpu_bursts_time = tableSJF$cpu_bursts_time, ready_wait_time = tableSJF$ready_wait_time)
df1$data <- 'FCFS'
df2$data <- 'SJF'

df <- rbind.data.frame(df1,df2)

ggplot(df, aes(x = cpu_bursts_time, y = ready_wait_time))+
    geom_line(aes(colour = data), size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="CPU Burst", breaks=seq(0,100,5)) +
    scale_y_continuous(name="Time (sec)", breaks=seq(0,300,15)) + 
    ggtitle("Waiting Time")+
    theme_bw()


#=========== Evaluate quantum time ==============
#0.1, 0.2, 0.3, 0.4, 0.5, 0.75
# 1, 2.25, 5, 7.5, 10, 15, 20
# 30, 40, 50, 60
system("py simulator.py --cpu-scheduler rr   --quantum 0.1 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ0.1   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 0.2 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ0.2   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 0.3 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ0.3   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 0.4 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ0.4   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 0.5 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ0.5   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 0.75 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ0.75   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 1 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ1   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 2.25 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ2.25   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 5 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ5   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 7.5 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ7.5   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 10 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ10   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 15 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ15   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 20 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ20   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 30 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ30   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 40 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ40   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
system("py simulator.py --cpu-scheduler rr   --quantum 60 --input-file testScript.txt --output-file ResultFiles/testResultRR.txt")
tableRRQ60   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")

df0.1 = data.frame(time_quantum = 0.1, ready_wait_time = mean(tableRRQ0.1$ready_wait_time))
df0.2 = data.frame(time_quantum = 0.2, ready_wait_time = mean(tableRRQ0.2$ready_wait_time))
df0.3 = data.frame(time_quantum = 0.3, ready_wait_time = mean(tableRRQ0.3$ready_wait_time))
df0.4 = data.frame(time_quantum = 0.4, ready_wait_time = mean(tableRRQ0.4$ready_wait_time))
df0.5 = data.frame(time_quantum = 0.5, ready_wait_time = mean(tableRRQ0.5$ready_wait_time))
df0.75 = data.frame(time_quantum = 0.75,ready_wait_time = mean(tableRRQ0.75$ready_wait_time))
df1 =    data.frame(time_quantum = 1,   ready_wait_time = mean(tableRRQ1$ready_wait_time))
df2.25 = data.frame(time_quantum = 2.5, ready_wait_time = mean(tableRRQ2.25$ready_wait_time))
df5 =    data.frame(time_quantum = 5,   ready_wait_time = mean(tableRRQ5$ready_wait_time))
df7.5 =  data.frame(time_quantum = 7,5, ready_wait_time = mean(tableRRQ7.5$ready_wait_time))
df10 = data.frame(time_quantum = 10, ready_wait_time = mean(tableRRQ10$ready_wait_time))
df15 = data.frame(time_quantum = 15, ready_wait_time = mean(tableRRQ15$ready_wait_time))
df20 = data.frame(time_quantum = 20, ready_wait_time = mean(tableRRQ20$ready_wait_time))
df30 = data.frame(time_quantum = 30, ready_wait_time = mean(tableRRQ30$ready_wait_time))
df40 = data.frame(time_quantum = 40, ready_wait_time = mean(tableRRQ40$ready_wait_time))
df60 = data.frame(time_quantum = 60, ready_wait_time = mean(tableRRQ60$ready_wait_time))

df0.1$quantum <- '0.1'
df0.2$quantum <- '0.2'
df0.3$quantum <- '0.3'
df0.4$quantum <- '0.4'
df0.5$quantum <- '0.5'
df0.75$quantum <- '0.75'
df1$quantum <- '1'
df2.25$quantum <- '2.25'
df5$quantum <- '5'
df7.5$quantum <- '7.5'
df10$quantum <- '10'
df15$quantum <- '15'
df20$quantum <- '20'
df30$quantum <- '30'
df40$quantum <- '40'
df60$quantum <- '60'

df <- bind_rows(df0.1,df0.2,df0.3,df0.4,df0.5,df0.75,df1,df2.25,df5,df7.5,df10,df15,df20,df30,df40,df60)

ggplot(df, aes(x = time_quantum  , y = ready_wait_time))+
    geom_line(size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="Time Quantum (sec)",breaks=seq(1,60,5)) +
    scale_y_continuous(name="Average Waiting Time (sec)") + 
    ggtitle("Quantum Waiting Time")+
    theme_bw()

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
#rownames(mt)=c("FCFS", "SJF", "RR", "SRTF")
mt$processes <- rownames(mt)
mt      <- melt(mt, id.vars=c("processes"))

ggplot(mt, aes(x=as.numeric(processes), y=value, color=variable)) + 
    geom_line(size = 1) + geom_point(fill = "white") + 
    scale_shape_manual(values = c(22, 21))  + xlab("Processes") + 
    ylab("Average Turnaround Time") + ggtitle("Average Turnaround Time")

# ==============================
# ===== Scheduling compare =====
# ==============================

par(mfrow=c(2,2))

ymax  = max(max(tableRR$tat), max(tableRR$tat), max(tableRR$tat), max(tableRR$tat))*1.2
xmmax = max(max(tableRR$bursts_time), max(tableRR$bursts_time), max(tableRR$bursts_time), max(tableRR$bursts_time))*1.2
xleg = "Bursts Time"
yleg = "Turn around time"

plotRRWaitingTime <- plot(tableRR$bursts_time, tableRR$tat,
                          xlab = xleg,
                          ylab = yleg,
                          main = "Round Robin",
                          ylim = c(0, ymax),
                          xlim = c(0, xmmax))

df <- as.data.frame((tableRR))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("Round Robin")

plotFCFSWaitingTime <- plot(tableFCFS$bursts_time, tableFCFS$tat,
                            xlab = xleg,
                            ylab = yleg,
                            main = "First come First Served",
                            ylim = c(0, ymax),
                            xlim = c(0, xmmax))

df <- as.data.frame((tableFCFS))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("First Come First Served")

plotSJFWaitingTime <- plot(tableSJF$bursts_time, tableSJF$tat,
                           xlab = xleg,
                           ylab = yleg,
                           main = "S JOB FIRST",
                           ylim = c(0, ymax),
                           xlim = c(0, xmmax))

df <- as.data.frame((tableSJF))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("Shortest Job First")

plotSRTFWaitingTime <- plot(tableSRTF$bursts_time, tableSRTF$tat,
                            xlab = xleg,
                            ylab = yleg,
                            main = "Shortest ready JOB FIRST",
                            ylim = c(0, ymax),
                            xlim = c(0, xmmax))

df <- as.data.frame((tableSRTF))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("Shortest Remaining Time First")
