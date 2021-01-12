#=================================================#
# Generates workloads for the two hyphotesis test #
#=================================================#

#Function to generate workload
gen_workload <- function (NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO, fileId){
  
  createTestStr =  paste("Rscript gen_workload.R", NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO, sep = " ")
  info =  system(createTestStr, intern = TRUE)
  
  filename = paste("WorkloadFiles/workload",fileId,".txt", sep = '')
  cat(info, file=filename, sep="\n")  
  invisible(info)
}

#============ Common parameters ===========
NumProcs <- 50
MeanIoBursts <- 10
MeanIat <- 25
numTests = 50

#============ Default parameters ===========
MinCPU <- 1.0
MaxCPU <- 2.0
MinIO <- 0.3
MaxIO <- 0.5

#============ Enlarged parameters ===========
MinCPULarge <- 2.0
MaxCPULarge <- 10.0
MinIOLarge <- 1.3
MaxIOLarge <- 2.5


for (i in 1:numTests){
  
  fileId = paste("Default",i, sep = '')
  gen_workload(NumProcs, MeanIoBursts, MeanIat, MinCPU, MaxCPU, MinIO, MaxIO, fileId)
  
}

for (i in 1:numTests){
  
  fileId = paste("Large",i, sep = '')
  gen_workload(NumProcs, MeanIoBursts, MeanIat, MinCPULarge, MaxCPULarge, MinIOLarge, MaxIOLarge, fileId)
  
}