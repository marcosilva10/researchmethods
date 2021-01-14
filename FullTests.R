library(reticulate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape)

#Defaul and initial values
NumProcs <- 50
MeanIoBursts <- 10
MeanIat <- 25
MinCPU <- 1.0
MaxCPU <- 2.0
MinIO <- 0.3
MaxIO <- 0.5

#Function to generate workload
gen_workload <- function (NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO, fileId){
    
    #cat("# == Generating Workload == \n")
    createTestStr =  paste("Rscript gen_workload.R", NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO, sep = " ")
    info =  system(createTestStr, intern = TRUE)
    cat(info, file=cat("workload",fileId,".txt"), sep="\n")  
    invisible(info)
}

#Function to execute simulator.py
exec_simulator <- function(fileId){
    
    #cat("# == Execution simulator ==\n")
    
    #Run in 4 methods, need function to make it one line per file
    #C:/Users/marco/anaconda3/envs/rstudio/python.exe -> dont delete pls
    system(paste("C:/Users/marco/anaconda3/envs/rstudio/python.exe simulator.py --cpu-scheduler fcfs --input-file WorkloadFiles/workload",fileId,".txt --output-file ResultFiles/testResultFCFS.txt", sep = ''))
    system(paste("C:/Users/marco/anaconda3/envs/rstudio/python.exe simulator.py --cpu-scheduler sjf  --input-file WorkloadFiles/workload",fileId,".txt --output-file ResultFiles/testResultSJF.txt", sep = ''))
    system(paste("C:/Users/marco/anaconda3/envs/rstudio/python.exe simulator.py --cpu-scheduler rr   --quantum 1 --input-file WorkloadFiles/workload",fileId,".txt --output-file ResultFiles/testResultRR.txt", sep = ''))
    system(paste("C:/Users/marco/anaconda3/envs/rstudio/python.exe simulator.py --cpu-scheduler srtf --input-file WorkloadFiles/workload",fileId,".txt --output-file ResultFiles/testResultSRTF.txt", sep = ''))
    
}

#Function to execute RR algorithm only
exec_rr <- function(quantumValue, fileId){
    
    cat("# == Execution RR simulator ==", quantumValue, "\n")
    
    system(paste("C:/Users/marco/anaconda3/envs/rstudio/python.exe simulator.py --cpu-scheduler rr   --quantum ", quantumValue," --input-file WorkloadFiles/workload",fileId,".txt --output-file ResultFiles/testResultRR.txt", sep = ''))
    
}

#function to plot
plotBurstXTat <- function(tableToUse, schedulername, xmax, ymax) {
    
    xleg = "Bursts Time"
    yleg = "Turn around time"
    
    
    plotRRWaitingTime <- plot(tableToUse$bursts_time, tableToUse$tat,
                              xlab = xleg,
                              ylab = yleg,
                              main = schedulername,
                              ylim = c(0, ymax),
                              xlim = c(0, xmax))
    
    abline(lm(tat ~ bursts_time, data = tableToUse), col = "blue")
}

#=========== Evaluate quantum time ==============
quantum_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 2.25, 5, 7.5, 10, 20)

num_runs = 50
#Create variables
for(i in 1:length(quantum_values)) { 
    nam <- paste("meanWaitingTimestableRRQ", quantum_values[i], sep = "")
    assign(nam, matrix(nrow = num_runs, ncol = 1))
    nam <- paste("meanTatTimestableRRQ", quantum_values[i], sep = "")
    assign(nam, matrix(nrow = num_runs, ncol = 1))
    nam <- paste("sdWaitingTimestableRRQ", quantum_values[i], sep = "")
    assign(nam, matrix(nrow = num_runs, ncol = 1))
    nam <- paste("sdTatTimestableRRQ", quantum_values[i], sep = "")
    assign(nam, matrix(nrow = num_runs, ncol = 1))
}

#Execute simulator
aux <- 1
for (i in seq(1, by=length(11), length=num_runs)){
    
    #fileId = paste("Default", aux, sep = '')
    fileId = paste("Large", aux, sep = '')
    #====================
    cat("# == Execution simulator ==\n")
    exec_rr(0.1,fileId)
    meanWaitingTimestableRRQ0.1[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ0.1[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ0.1[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ0.1[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(0.2,fileId)
    meanWaitingTimestableRRQ0.2[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ0.2[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ0.2[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ0.2[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(0.3,fileId)
    meanWaitingTimestableRRQ0.3[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ0.3[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ0.3[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ0.3[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(0.4,fileId)
    meanWaitingTimestableRRQ0.4[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ0.4[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ0.4[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ0.4[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(0.5,fileId)
    meanWaitingTimestableRRQ0.5[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ0.5[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ0.5[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ0.5[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(0.75,fileId)
    meanWaitingTimestableRRQ0.75[aux,]  = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ0.75[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ0.75[aux,]  = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ0.75[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(1,fileId)
    meanWaitingTimestableRRQ1[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ1[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ1[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ1[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(2.25,fileId)
    meanWaitingTimestableRRQ2.25[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ2.25[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ2.25[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ2.25[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(5,fileId)
    meanWaitingTimestableRRQ5[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ5[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ5[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ5[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(7.5,fileId)
    meanWaitingTimestableRRQ7.5[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ7.5[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ7.5[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ7.5[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(10,fileId)
    meanWaitingTimestableRRQ10[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ10[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ10[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ10[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    exec_rr(20,fileId)
    meanWaitingTimestableRRQ20[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    sdWaitingTimestableRRQ20[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$ready_wait_time)))
    meanTatTimestableRRQ20[aux,]   = c(mean((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    sdTatTimestableRRQ20[aux,]   = c(sd((read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")$tat)))
    
    #===================
    aux <- aux + 1
}

df0.1 = data.frame(time_quantum = 0.1,
                   ready_wait_time = mean(meanWaitingTimestableRRQ0.1), tat = mean(meanTatTimestableRRQ0.1), 
                   sd_ready_wait_time = mean(sdWaitingTimestableRRQ0.1), sd_tat = mean(sdTatTimestableRRQ0.1))

df0.2 = data.frame(time_quantum = 0.2,  ready_wait_time = mean(meanWaitingTimestableRRQ0.2), tat = mean(meanTatTimestableRRQ0.2), 
                   sd_ready_wait_time = mean(sdWaitingTimestableRRQ0.2), sd_tat = mean(sdTatTimestableRRQ0.2))

df0.3 = data.frame(time_quantum = 0.3,  ready_wait_time = mean(meanWaitingTimestableRRQ0.3), tat = mean(meanTatTimestableRRQ0.3),
                   sd_ready_wait_time = mean(sdWaitingTimestableRRQ0.3), sd_tat = mean(sdTatTimestableRRQ0.3))

df0.4 = data.frame(time_quantum = 0.4,  ready_wait_time = mean(meanWaitingTimestableRRQ0.4), tat = mean(meanTatTimestableRRQ0.4),
                   sd_ready_wait_time = mean(sdWaitingTimestableRRQ0.4), sd_tat = mean(sdTatTimestableRRQ0.4))

df0.5 = data.frame(time_quantum = 0.5,  ready_wait_time = mean(meanWaitingTimestableRRQ0.5),
                   tat = mean(meanTatTimestableRRQ0.5),  sd_ready_wait_time = mean(sdWaitingTimestableRRQ0.5), sd_tat = mean(sdTatTimestableRRQ0.5))

df0.75= data.frame(time_quantum = 0.75,ready_wait_time = mean(meanWaitingTimestableRRQ0.75),
                   tat = mean(meanTatTimestableRRQ0.75),  sd_ready_wait_time = mean(sdWaitingTimestableRRQ0.75), sd_tat = mean(sdTatTimestableRRQ0.75))

df1 =   data.frame(time_quantum = 1,   ready_wait_time = mean(meanWaitingTimestableRRQ1),
                   tat = mean(meanTatTimestableRRQ1), sd_ready_wait_time = mean(sdWaitingTimestableRRQ1), sd_tat = mean(sdTatTimestableRRQ1))

df2.25= data.frame(time_quantum = 2.5, ready_wait_time = mean(meanWaitingTimestableRRQ2.25),
                   tat = mean(meanTatTimestableRRQ2.25), sd_ready_wait_time = mean(sdWaitingTimestableRRQ2.25), sd_tat = mean(sdTatTimestableRRQ2.25))

df5 =   data.frame(time_quantum = 5,   ready_wait_time = mean(meanWaitingTimestableRRQ5),
                   tat = mean(meanTatTimestableRRQ5),  sd_ready_wait_time = mean(sdWaitingTimestableRRQ5), sd_tat = mean(sdTatTimestableRRQ5))

df7.5 = data.frame(time_quantum = 7,5, ready_wait_time = mean(meanWaitingTimestableRRQ7.5),
                   tat = mean(meanTatTimestableRRQ7.5),  sd_ready_wait_time = mean(sdWaitingTimestableRRQ7.5), sd_tat = mean(sdTatTimestableRRQ7.5))

df10 =  data.frame(time_quantum = 10,    ready_wait_time = mean(meanWaitingTimestableRRQ10),
                   tat = mean(meanTatTimestableRRQ10),  sd_ready_wait_time = mean(sdWaitingTimestableRRQ10), sd_tat = mean(sdTatTimestableRRQ10))

df20 =  data.frame(time_quantum = 20,    ready_wait_time = mean(meanWaitingTimestableRRQ20),
                   tat = mean(meanTatTimestableRRQ20),  sd_ready_wait_time = mean(sdWaitingTimestableRRQ20), sd_tat = mean(sdTatTimestableRRQ20))

df <- bind_rows(df0.1,df0.2,df0.3,df0.4,df0.5,df0.75,df1,df2.25,df5,df7.5,df10,df20)

ggplot(df, aes(x = time_quantum  , y = ready_wait_time))+
    geom_line(size = 1)+
    geom_point(colour = 'royalblue', size = 2) +
    scale_x_continuous(name="Time Quantum (ms)",breaks=seq(1,max(df$time_quantum),1)) +
    scale_y_continuous(name="Average Waiting Time (ms)") + 
    theme_bw()

# Turnaround time
ggplot(df, aes(x = time_quantum  , y = tat))+
    geom_line(size = 1)+
    geom_point(colour = 'royalblue', size = 2)+
    scale_x_continuous(name="Time Quantum (ms)",breaks=seq(1,max(df$time_quantum),1)) +
    scale_y_continuous(name="Average Turnaround Time (ms)") + 
    theme_bw()


#==========================================================
# Designed to evaluate the average turnaround time and waiting time
# according to the number of processes
#==========================================================

num_runs = 30
tmp = c(1,2,3,4)
meanTatTimes <- matrix(nrow = num_runs, ncol = 4)
meanBurstTimes <- matrix(nrow = num_runs, ncol = 4)
sdTatTimes <- matrix(nrow = num_runs, ncol = 4)
sdBurstTimes <- matrix(nrow = num_runs, ncol = 4)
meanWaitingTimes <- matrix(nrow = num_runs, ncol = 4)

aux <- 1
for (i in seq(1, by=length(tmp), length=num_runs)){
    
    eval_parameter <- aux 
    
    fileId = paste("Default", aux, sep = '')
    #gen_workload(NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO)
    exec_simulator(fileId)
    
    #Collect Results from each
    #cat("# == collecting results ==\n ")
    tableFCFS = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
    tableSJF  = read.table("ResultFiles/testResultSJF.txt", header = TRUE, sep = "", dec = ".")
    tableRR   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
    tableSRTF = read.table("ResultFiles/testResultSRTF.txt", header = TRUE, sep = "", dec = ".")
    
    meanBurstTimes[aux,] = c(mean(tableFCFS$bursts_time), 
                   mean(tableSJF$bursts_time), 
                   mean(tableRR$bursts_time), 
                   mean(tableSRTF$bursts_time))
    
    sdBurstTimes[aux,] = c(sd(tableFCFS$bursts_time), 
                   sd(tableSJF$bursts_time), 
                   sd(tableRR$bursts_time), 
                   sd(tableSRTF$bursts_time))
    
    meanTatTimes[aux,] = c(mean(tableFCFS$tat), 
            mean(tableSJF$tat), 
            mean(tableRR$tat), 
            mean(tableSRTF$tat))
    
    sdTatTimes[aux,] = c(sd(tableFCFS$tat), 
                sd(tableSJF$tat), 
                sd(tableRR$tat), 
                sd(tableSRTF$tat))
    
    meanWaitingTimes[aux,] = c(mean(tableFCFS$ready_wait_time), 
            mean(tableSJF$ready_wait_time), 
            mean(tableRR$ready_wait_time), 
            mean(tableSRTF$ready_wait_time))
    
    aux <- aux + 1 #Change to exponential?
}

#barplot:

dim(meanWaitingTimes) = c(length(tmp),num_runs)

colnames(meanWaitingTimes) <- paste0("", 1:num_runs)              # column names
meanWaitingTimes <- cbind(meanWaitingTimes, Mean = rowMeans(meanWaitingTimes[,]))    # column mean
rownames(meanWaitingTimes) <- paste0(c("FCFS", "SJF", "RR", "SRTF")) # row names

barplot(meanWaitingTimes, beside = T,  legend = TRUE, col = c("red","green", "yellow", "blue"),
        xlab = "Processes",
        ylab = "Average Waiting time (ms)",
        main = "Waiting time")

#========= HISTOGRAM AND ANOVA ===================

par(mfrow=c(2,2))
xLabel = "Turn Around Time"
xEndHist = (max(meanTatTimes[,]))+10
hist(meanTatTimes[,1], xlab = xLabel, main = "FCFS", seq(0, xEndHist, 10))
hist(meanTatTimes[,2], xlab = xLabel, main = "SJF", seq(0, xEndHist, 10))
hist(meanTatTimes[,3], xlab = xLabel, main = "RR", seq(0, xEndHist, 10))
hist(meanTatTimes[,4], xlab = xLabel, main = "SRTF", seq(0, xEndHist, 10))


#print(t.test(meanTatTimes[,2], meanTatTimes[,3], alternative = "l"))
cat("SD comparison for ANOVA", mean(sdTatTimes[,1]), mean(sdTatTimes[,2]), mean(sdTatTimes[,3]), mean(sdTatTimes[,4]))
BurstTimes = meanBurstTimes[,1]
print(summary(manova(cbind(meanTatTimes[,1], meanTatTimes[,2], meanTatTimes[,3], meanTatTimes[,4]) ~ BurstTimes)))


#==========================================================
# Same as the last run, but with larger bursts
#==========================================================

num_runs = 30
meanLargeTests = c(1,2,3,4)
meanLargeTatTimes <- matrix(, nrow = num_runs, ncol = 4)
meanLargeBurstTimes <- matrix(, nrow = num_runs, ncol = 4)
sdLargeTatTimes <- matrix(, nrow = num_runs, ncol = 4)
sdLargeBurstTimes <- matrix(, nrow = num_runs, ncol = 4)

aux <- 1
for (i in seq(1, by=length(tmp), length=num_runs)){
    
    eval_parameter <- aux 
    
    fileId = paste("Large", aux, sep = '')
    #gen_workload(NumProcs, MeanIoBursts, MeanIat, MinCPULarge, MaxCPULarge, MinIOLarge, MaxIOLarge)
    exec_simulator(fileId)
    
    #Collect Results from each
    cat("# == collecting results ==\n ")
    tableFCFSLarge = read.table("ResultFiles/testResultFCFS.txt", header = TRUE, sep = "", dec = ".")
    tableSJFLarge  = read.table("ResultFiles/testResultSJF.txt", header = TRUE, sep = "", dec = ".")
    tableRRLarge   = read.table("ResultFiles/testResultRR.txt", header = TRUE, sep = "", dec = ".")
    tableSRTFLarge = read.table("ResultFiles/testResultSRTF.txt", header = TRUE, sep = "", dec = ".")
    
    meanLargeBurstTimes[aux,] = c(mean(tableFCFSLarge$bursts_time), 
                             mean(tableSJFLarge$bursts_time), 
                             mean(tableRRLarge$bursts_time), 
                             mean(tableSRTFLarge$bursts_time))
    
    sdLargeBurstTimes[aux,] = c(sd(tableFCFSLarge$bursts_time), 
                           sd(tableSJFLarge$bursts_time), 
                           sd(tableRRLarge$bursts_time), 
                           sd(tableSRTFLarge$bursts_time))
    
    meanLargeTatTimes[aux,] = c(mean(tableFCFSLarge$tat), 
                           mean(tableSJFLarge$tat), 
                           mean(tableRRLarge$tat), 
                           mean(tableSRTFLarge$tat))
    
    sdLargeTatTimes[aux,] = c(sd(tableFCFSLarge$tat), 
                         sd(tableSJFLarge$tat), 
                         sd(tableRR$tat), 
                         sd(tableSRTFLarge$tat))
    
    
    aux <- aux + 1 #Change to exponential?
}

print(t.test(meanLargeTatTimes[,2], meanLargeTatTimes[,3], alternative = "l"))
print(t.test(meanLargeTatTimes[,2], meanLargeTatTimes[,1], alternative = "l"))
print(t.test(meanLargeTatTimes[,2], meanLargeTatTimes[,4], alternative = "l"))


par(mfrow=c(2,2))
xLabel = "Turnaround Time"
xEndHist = (max(meanLargeTatTimes[,]))+100
xStartHist = (min(meanLargeTatTimes[,]))-100

hist(meanLargeTatTimes[,1], xlab = xLabel, main = "FCFS", seq(xStartHist, xEndHist, 100))
hist(meanLargeTatTimes[,2], xlab = xLabel, main = "SJF", seq(xStartHist, xEndHist, 100))
hist(meanLargeTatTimes[,3], xlab = xLabel, main = "RR", seq(xStartHist, xEndHist, 100))
hist(meanLargeTatTimes[,4], xlab = xLabel, main = "SRTF", seq(xStartHist, xEndHist, 100))

#==========================================================
# Other Stats
#==========================================================


meanTatTimesCpy <- meanTatTimes
dim(meanTatTimesCpy) = c(length(tmp),num_runs)

colnames(meanTatTimesCpy) <- paste0("", 1:num_runs)              # column names
rownames(meanTatTimesCpy) <- paste0(c("FCFS", "SJF", "RR", "SRTF")) # row names

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

ymax  = max(max(tableRRLargeProccess$tat), max(tableFCFSLargeProccess$tat), max(tableSJFLargeProccess$tat), max(tableSRTFLargeProccess$tat))*1.2
xmax = max(max(tableRRLargeProccess$bursts_time), max(tableFCFSLargeProccess$bursts_time), max(tableSJFLargeProccess$bursts_time), max(tableSRTFLargeProccess$bursts_time))*1.2
xleg = "Bursts Time"
yleg = "Turn around time"

plotBurstXTat(tableRRLargeProccess, "Round Robin", xmax, ymax)
plotBurstXTat(tableFCFSLargeProccess, "First Come First Served", xmax, ymax)
plotBurstXTat(tableSJFLargeProccess, "Shortest Job First", xmax, ymax)
plotBurstXTat(tableSRTFLargeProccess, "Shortest Remaining Time First", xmax, ymax)

mtext("Large 100 proccess", outer = TRUE, cex = 1.5)

df <- as.data.frame((tableRR))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("Round Robin")

df <- as.data.frame((tableFCFS))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("First Come First Served")

df <- as.data.frame((tableSJF))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("Shortest Job First")

df <- as.data.frame((tableSRTF))
ggplot(df, aes(x = bursts_time, y = tat)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    xlab(xleg) + 
    ylab(yleg) + ggtitle("Shortest Remaining Time First")



#=======================
#Small proccess
#=======================

par(mfrow=c(2,2))

ymax  = max(max(tableRRSmallProccess$tat), max(tableFCFSSmallProccess$tat), max(tableSRTFSmallProccess$tat), max(tableSRTFSmallProccess$tat))*1.2
xmax = max(max(tableRRSmallProccess$bursts_time), max(tableFCFSSmallProccess$bursts_time), max(tableSRTFSmallProccess$bursts_time), max(tableSRTFSmallProccess$bursts_time))*1.2

plotBurstXTat(tableRRSmallProccess, "Round Robin", xmax, ymax)
plotBurstXTat(tableFCFSSmallProccess, "First Come First Served", xmax, ymax)
plotBurstXTat(tableSJFSmallProccess, "Shortest Job First", xmax, ymax)
plotBurstXTat(tableSRTFSmallProccess, "Shortest Remaining Time First", xmax, ymax)

mtext("Small 10 proccess", outer = TRUE, cex = 1.5)


#=========================
# Regression Analysis
#=========================

lrFCFS.out = lm(meanBurstTimes~meanWaitingTimes[1,1:30])
lrSJF.out = lm(meanBurstTimes~meanWaitingTimes[2,1:30])
lrRR.out = lm(meanBurstTimes~meanWaitingTimes[3,1:30])
lrSRTF.out = lm(meanBurstTimes~meanWaitingTimes[4,1:30])
summary(lrFCFS.out)
summary(lrSJF.out)
summary(lrRR.out)
summary(lrSRTF.out)
