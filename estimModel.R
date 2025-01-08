rm(list=ls())
#setwd("/home/antonio/MEGA/Lavoro_sync/Working_projects/Current/AC_fuzzyTopicModel/mef_continued/PCP_eval/")
setwd("/home/antonio.calcagni/storage/PCP_eval/")
library(doParallel);source("utils/FLSA.R")
if(!dir.exists("results")){dir.create("results")}

##### Atomic functions #####
run_algs = function(X=NULL,K=NULL){ #one-shot call for running the algorithms
  cat("Running LSA..\n");out_lsa = tryCatch({lsa::lsa(X,dims = K)},error=function(e){return(NA)})
  cat("Running fLSA..\n");out_flsa = tryCatch({FLSA_core(X = as.matrix(X),N = NROW(X),M = NCOL(X),K = K,type = "svd")},error=function(e){return(NA)})
  cat("Running LDA..\n");out_lda = tryCatch({topicmodels::LDA(slam::as.simple_triplet_matrix(as.matrix(X)),K,method = "Gibbs", control = list(iter = 2000, burnin = 500))},error=function(e){return(NA)})
  cat("Running CTM..\n");out_ctm = tryCatch({stm::stm(quanteda::as.dfm(X),K = K,verbose = FALSE, init.type="Spectral",emtol = 1e-3,max.em.its = 100)},error=function(e){return(NA)})
  
  lsa_list=NULL
  if(!sum(is.na(out_lsa))){
    lsa_list = list(PW_T = out_lsa$dk%*%solve(diag(apply(out_lsa$dk,2,sum))),PD_T = out_lsa$tk%*%solve(diag(apply(out_lsa$tk,2,sum))))}
  
  flsa_list=NULL
  if(!sum(is.na(out_flsa))){
    flsa_list = list(PW_T = out_flsa$PW_T,PD_T = out_flsa$PT_D)}
  
  lda_list=NULL
  if(!sum(is.na(out_lda))){
    lda_list = list(PW_T = t(exp(out_lda@beta)),PD_T = out_lda@gamma,pepx = topicmodels::perplexity(out_lda,X))}
  
  ctm_list=NULL
  if(!sum(is.na(out_ctm))){
    ctm_list = list(PW_T = t(exp(out_ctm$beta$logbeta[[1]])),PD_T = out_ctm$theta)}
  
  return(list("lsa"=lsa_list,"flsa"=flsa_list,"lda"=lda_list,"ctm"=ctm_list))
}

eval_scenario = function(X,K){
  x=c(sort(seq(K,1,by=-2)),seq(K+2,(K*2),by=2))
  kk = sort(c(min(x),max(x),K,K+c(-2,+2)))
  S = vector(mode = "list",length = length(kk))
  for(k in 1:length(kk)){
    cat(paste0("|> Evaluating scenario..k=",kk[k]),"\n");
    S[[k]] =   run_algs(X,kk[k]) #scenario where k is in a neighborhood of k0 (k0 included)
  }
  names(S) = kk
  S$k0neigh = kk
  return(S)
}


##### Setting the environment #####
sts = read.table(file = "settings.dat",header = FALSE,sep = ",",as.is = TRUE)

B = sts[3,2]
ncores = sts[2,2]
chunk.size = floor(B/ncores)
n_resid = rep(0,ncores)
n_resid = rep(0,ncores)
if((ncores*chunk.size)<B){n_resid[ncores] = B - ncores*chunk.size}

cl = parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl,cores=ncores)

Design = read.table(file = "design.dat",header = TRUE,sep = ";")
DD = NROW(Design)

##### Run algorithms on the generated data
for(d in 11:DD){
  cat("Estimating models for design:",d, "\n")
  
  foreach(h=1:ncores, .combine = "c",.export = NULL,.packages = NULL) %dopar% {
    iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
    for(u in iid_core){
      datain = readRDS(file = paste0("data/design_",d,"_LDA_",u,".rds"))
      res = eval_scenario(datain$Dtm,Design$K[d])
      saveRDS(res, paste0("results/design_",d,"_LDA_",u,".rds"), compress = "xz")
    }
  }
  
  # (disabled in this version)
  # foreach(h=1:ncores, .combine = "c",.export = NULL,.packages = NULL) %dopar% {
  #   iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
  #   for(u in iid_core){
  #     datain = readRDS(file = paste0("data/design_",d,"_CTM_",u,".rds"))
  #     res = eval_scenario(datain$Dtm,Design$K[d])
  #     saveRDS(res, paste0("results/design_",d,"_CTM_",u,".rds"), compress = "xz")
  #   }
  # }
} 

doParallel::stopImplicitCluster(); parallel::stopCluster(cl)
cat('\n Done \n')
