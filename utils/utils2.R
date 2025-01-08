LDA_simulate = function(n,K,lambda,V){
  
  N = rpois(n,lambda) #doc lengths
  terms = unique(sort(mapply(function(j)paste(sample(x = letters,size = runif(1,2,10),replace = TRUE),collapse=""),1:V)))
  topics = paste0("topic", 1:K)
  documents = paste0("doc", 1:n)
  
  alpha=rep(1/K,K) #number-of-topics length vector set to symmetric alpha parameter across all topics
  beta=rep(1/K,length(terms))  #number-of-terms length vector set to symmetric beta paramater across all terms
  
  Theta = MCMCpack::rdirichlet(n, alpha) #Document-Topic distribution matrix
  colnames(Theta) = topics; rownames(Theta) = documents
  
  Phi = MCMCpack::rdirichlet(K, beta) #Topic- Terms distribution matrix
  colnames(Phi) = terms; rownames(Phi) = topics
  
  corpus = vector(mode = "list",length = n)
  for(i in 1:n){
    # Generate each document by sampling words from the conditional model 
    document=rep("",N[i])
    for(j in 1:N[i]){
      z = rmultinom(n = 1, size = 1, prob = Theta[i,]) #For each word in a document, choose a topic from that document's topic distribution
      w = rmultinom(n = 1, size = 1, prob = Phi[which.max(z),]) #Choose a term from that topic's term distribution
      document[j] = colnames(Phi)[which.max(w)] #Create the document vector
    }
    
    # Append the generated document in the final corpus
    corpus[[i]] = document
  }
  
  freq_list = lapply(corpus,table)
  
  # Compute the Document-Term Matrix (DTM)
  Dtm = matrix(data = 0,nrow=n,ncol=length(terms))
  colnames(Dtm) = terms; rownames(Dtm) = documents
  for(i in 1:n){Dtm[i,names(freq_list[[i]])] = freq_list[[i]]}
  
  return(list(Dtm=Dtm,P_DT=Theta,P_WT=t(Phi)))
}

CTM_simulate = function(n,K,lambda,V,alphad=1){
  
  N = rpois(n,lambda) #doc lengths
  terms = unique(sort(mapply(function(j)paste(sample(x = letters,size = runif(1,2,10),replace = TRUE),collapse=""),1:V)))
  topics = paste0("topic", 1:K)
  documents = paste0("doc", 1:n)
  
  alpha=rep(1/K,K) #number-of-topics length vector set to symmetric alpha parameter across all topics
  beta=rep(1/K,length(terms))  #number-of-terms length vector set to symmetric beta paramater across all terms
  
  # Generate a random positive definite covariance matrix for topics
  if(K==8){
    Z <- as.matrix(Matrix::tril(as.matrix(read.csv(file = "utils/K8_corrmat.csv",header = FALSE))))
    Z <- Z + t(as.matrix(Matrix::tril(Z))); diag(Z) <- 1
  }else if(K==20){
    Z <- as.matrix(Matrix::tril(as.matrix(read.csv(file = "utils/K20_corrmat.csv",header = FALSE))))
    Z <- Z + t(as.matrix(Matrix::tril(Z))); diag(Z) <- 1
  }
  sds = sqrt(runif(K,1,10))
  cor_matrix = clusterGeneration::rcorrmatrix(d = K,alphad = alphad)
  cov_matrix = outer(sds, sds)*cor_matrix
  
  # Sample from a multivariate normal distribution for document-topic distributions
  Theta = mvtnorm::rmvnorm(n, mean = rep(0, K), sigma = cov_matrix) 
  Theta = t(mapply(function(i)exp(Theta[i,])/sum(exp(Theta[i,])),1:n)) #soft-max transformation
  colnames(Theta) = topics; rownames(Theta) = documents
  
  Phi = MCMCpack::rdirichlet(K, beta) #Topic- Terms distribution matrix
  colnames(Phi) = terms; rownames(Phi) = topics
  
  corpus = vector(mode = "list",length = n)
  for(i in 1:n){
    # Generate each document by sampling words from the conditional model 
    document=rep("",N[i])
    for(j in 1:N[i]){
      z = rmultinom(n = 1, size = 1, prob = Theta[i,]) #For each word in a document, choose a topic from that document's topic distribution
      w = rmultinom(n = 1, size = 1, prob = Phi[which.max(z),]) #Choose a term from that topic's term distribution
      document[j] = colnames(Phi)[which.max(w)] #Create the document vector
    }
    
    # Append the generated document in the final corpus
    corpus[[i]] = document
  }
  
  freq_list = lapply(corpus,table)
  
  # Compute the Document-Term Matrix (DTM)
  Dtm = matrix(data = 0,nrow=n,ncol=length(terms))
  colnames(Dtm) = terms; rownames(Dtm) = documents
  for(i in 1:n){Dtm[i,names(freq_list[[i]])] = freq_list[[i]]}
  
  return(list(Dtm=Dtm,P_DT=Theta,P_WT=t(Phi)))
}
