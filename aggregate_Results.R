
rm(list=ls())
setwd("/home/antonio/MEGA/Lavoro_sync/Working_projects/Current/AC_fuzzyTopicModel/mef_continued/PCP_eval/")

type <- "LDA"
#type <- "CTM" #disabled in this version
DD <- 24
B <- 100
res <- vector(mode = "list",length = DD)
for(d in 1:DD){
  cat("\n Design no.: ",d)
  measures <- array(dim=c(5,5,4,B),dimnames = list(NULL,c("cl","rl","stb1","stb2","stb3"),c("LSA","fLSA","LDA","CTM")))
  for(b in 1:B){
    out <- readRDS(paste0("/home/antonio/mount_server/PCP_eval/elaborated_results/design_",d,"_",type,"_",b,"_measures.rds"))
    measures[,,,b] <- out
  }
  res[[d]] <- list(avg=apply(measures,c(1,2,3),mean), std=apply(measures,c(1,2,3),sd), q1=apply(measures,c(1,2,3),function(x)quantile(x,0.25)),q2=apply(measures,c(1,2,3),function(x)quantile(x,0.75)))
}
save(res,file = paste0("elaborated_results/measures_",type,"_aggregated.rds"))

