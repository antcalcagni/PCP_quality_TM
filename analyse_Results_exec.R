pcp_Eval <- function(P_DT=NULL){
  rpca_out <- rpca::rpca(M = P_DT,term.delta = 1e-05)
  
  out <- list(out_rpca=NA,out_smooth=NA); cl <- rl <- stb1 <- stb2 <- stb3 <- NA
  if(rpca_out$convergence$converged){
    
    cl <- 1-norm(rpca_out$S,type="F")/norm(rpca_out$L+rpca_out$S,type="F")
    rl <- 1-length(rpca_out$L.svd$d)/NCOL(rpca_out$L)
    
    pp_raw <- apply(rpca_out$L,1,sum)/apply(rpca_out$L+rpca_out$S,1,sum)*100
    out_smooth <- smooth.spline(pp_raw,df = median(pp_raw))
    pp_smooth <- out_smooth$y
    
    out_deriv <- predict(out_smooth, 1:length(pp_raw), deriv = 1)
    stb1 <- mean(abs(out_deriv$y - mean(out_deriv$y)))
    stb2 <- sum(diff(sign(out_deriv$y))!=0)
    stb3 <- mean(diff(sign(out_deriv$y)))
    
    out <- list(out_rpca=rpca_out,out_smooth=out_smooth,measures=c(cl,rl,stb1,stb2,stb3))  
  }
  return(out)
}

get_measures_b <- function(out=NULL){
  S <- vector(mode = "list",length = length(out))
  S[[length(out)]] <- out[[length(out)]]; names(S) <- names(out)
  measures <- array(dim = c(length(out)-1,5,4),dimnames = list(NULL,c("cl","rl","stb1","stb2","stb3"),c("LSA","fLSA","LDA","CTM"))) 
  
  for(l in 1:(length(out)-1)){
    S[[l]]$lsa <- pcp_Eval(out[[l]]$lsa$PD_T)
    S[[l]]$flsa <-pcp_Eval(out[[l]]$flsa$PD_T)
    S[[l]]$lda <- pcp_Eval(out[[l]]$lda$PD_T)
    S[[l]]$ctm <- pcp_Eval(out[[l]]$ctm$PD_T)
    
    measures[l,,1] <- S[[l]]$lsa$measures
    measures[l,,2] <- S[[l]]$flsa$measures
    measures[l,,3] <- S[[l]]$lda$measures
    measures[l,,4] <- S[[l]]$ctm$measures
  }
  return(list(S=S,measures=measures))
}


args <- commandArgs(trailingOnly = TRUE); for (arg in args[-length(args)]) {eval(parse(text = arg))} #to retrieve input args
type <- args[length(args)]

out <- readRDS(paste0("/home/antonio.calcagni/storage/PCP_eval/results/design_",d,"_",type,"_",b,".rds")) #load the data --{d,b,type} are external argument passed via R CMD BATCH
out_b <- get_measures_b(out)

saveRDS(out_b$S, paste0(getwd(),"/elaborated_results/design_",d,"_",type,"_",b,"_pcpEval.rds"), compress = "xz")
saveRDS(out_b$measures, paste0(getwd(),"/elaborated_results/design_",d,"_",type,"_",b,"_measures.rds"), compress = "xz")

