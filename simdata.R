rm(list=ls()); options(warn = -1)
setwd("/home/antonio.calcagni/storage/PCP_eval/")
library(doParallel)
source("utils/utils2.R")

if(!dir.exists("data")){dir.create("data")}
sts = read.table(file = "settings.dat",header = FALSE,sep = ",",as.is = TRUE)

B = sts[3,2]
ncores = sts[2,2]
chunk.size = floor(B/ncores)
n_resid = rep(0,ncores)
n_resid = rep(0,ncores)
if((ncores*chunk.size)<B){n_resid[ncores] = B - ncores*chunk.size}

cl = parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl,cores=ncores)


##### Set simulation design #####
n = c(1e3,3e3) #number of docs (corpus length)
V = c(1e3,5e3,1e4) #number of unique words (dictionary length)
lambda = c(0.3e3,1e3) #avg number of words per document (document length)
K = c(8,20) #number of topics

Design = expand.grid(n,V,lambda,K)
colnames(Design) = c("n","V","lambda","K")
write.table(x = Design,file = "design.dat",sep = ";",row.names = FALSE)


##### Simulate data #####
set.seed(sts[1,2])

DD = NROW(Design)
for(d in 1:DD){
  cat("Generating data for design:",d, "\n")
  
  # Generating data under LDA true model
  foreach(h=1:ncores, .combine = "c",.export = NULL,.packages = NULL) %dopar% {
    iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
    for(u in iid_core){
      res = LDA_simulate(Design$n[d],K = Design$K[d],lambda = Design$lambda[d],V = Design$V[d])
      saveRDS(res, paste0("data/design_",d,"_LDA_",u,".rds"), compress = "xz")
    }
  }
  
  # Generating data under CTM true model -- disabled in this version
  # foreach(h=1:ncores, .combine = "c",.export = NULL,.packages = NULL) %dopar% {
  #   res = list()
  #   iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
  #   for(u in iid_core){
  #     #res = tryCatch({CTM_simulate(Design$n[d],K = Design$K[d],lambda = Design$lambda[d],V = Design$V[d],alphad = 1e-20)},error=function(e){return(NULL)})
  #     res = tryCatch({CTM_simulate(Design$n[d],K = Design$K[d],lambda = Design$lambda[d],V = Design$V[d])},error=function(e){return(NULL)})
  #     saveRDS(res, paste0("data/design_",d,"_CTM_",u,".rds"), compress = "xz")
  #   }
  # }
  
}

doParallel::stopImplicitCluster(); parallel::stopCluster(cl)
cat('\n Done \n')



