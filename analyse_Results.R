
rm(list=ls())
setwd("/home/antonio.calcagni/storage/PCP_eval")
if(!file.exists(paste0(getwd(),"/elaborated_results/"))){dir.create(paste0(getwd(),"/elaborated_results/"))}

ncores <- 10
B <- 100 #number of samples
DD <- 24 #length of the simulation design

type <- "'LDA'"
cmds <- c()
for(d in 2:DD){
  b0 <- ifelse(d==2,71,1)
   for(b in b0:B){
    cmd <- paste0("R CMD BATCH --no-restore --no-save '--args d=",d," b=",b," ",type,"' analyse_Results_exec.R")
    cmds <- c(cmds, cmd)
  }
}

btcs <- split(cmds, ceiling(seq_along(cmds)/ncores))
for(jobs in btcs){ #running batches of jobs
  cat("\n Running:");for(x in jobs){cat("\n\t",strsplit(x,split = "--args")[[1]][2])}

  writeLines(con = "jobList.dat",text = jobs)
  system(paste0("parallel -j ", ncores, " < ", normalizePath("jobList.dat")))
  file.remove("jobList.dat")
}


# (disabled in this version)
# type <- "'CTM'"
# cmds <- c()
# for(d in 1:DD){
#   for(b in 1:B){
#     cmd <- paste0("R CMD BATCH --no-restore --no-save '--args d=",d," b=",b," ",type,"' analyse_Results_exec.R")
#     cmds <- c(cmds, cmd)
#   }
# }
# 
# btcs <- split(cmds, ceiling(seq_along(cmds)/ncores))
# for(jobs in btcs){ #running batches of jobs
#   cat("\n Running:");for(x in jobs){cat("\n\t",strsplit(x,split = "--args")[[1]][2])}
# 
#   writeLines(con = "jobList.dat",text = jobs)
#   system(paste0("parallel -j ", ncores, " < ", normalizePath("jobList.dat")))
#   file.remove("jobList.dat")
# }


