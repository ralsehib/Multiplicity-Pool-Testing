# This file contains functions to estimate the individual and multipool testing accuracy; pool testing sensitivity and specificity.
# Last modified date: 9 December 2022
# Authors: Omar De La Cruz Cabrera and Razan Alsehibani


library(Matrix)

# Testing pooling strategy


individual.test = function(x,specificity,sensitivity){
  
  x = as.logical(x) # A batch of people, sick or not
  n = length(x)   # Batch size
  p = ifelse(x,sensitivity,1-specificity)
  positives = rbinom(n,size = 1,prob = p)
  
  as.logical(positives)
  
}



pooled.test = function(x,specificity,sensitivity,threshold){
  
  x = as.logical(x) # A batch of people, sick or not
  n = length(x)   # Batch size
  sqr.n = sqrt(n) # We assume n is a perfect square
  
  
  number.of.positive.pools = foreach(j = 0:(sqr.n-1),.combine = `+`) %do% {### for each pattren
    
    pools = foreach(i = 0:(sqr.n-1),.combine=rbind) %do% (0:(sqr.n-1) - i*j ) %% sqr.n + 1  ## for each pool 
    
    any.sick.in.pool = foreach( k = 1:sqr.n,.combine = c) %do% any(x[pools == k]) ## for each individual
    
    p = ifelse(any.sick.in.pool,sensitivity,1-specificity)
    
    positives = rbinom(sqr.n,size = 1,prob = p)
    
    map.pool.results = positives[pools]
    
    map.pool.results
    
  }
  
  number.of.positive.pools >= threshold
  
}


assess = function(test,batchsize,prevalence,repetitions,...){
  
  result = foreach(r = 1:repetitions,.combine = `+`) %do% {
    
    x = as.logical(rbinom(batchsize,1,p=prevalence))
    
    calls = test(x,...)
    
    result.specificity = sum(!calls[!x])
    result.specificity.denominator = sum(!x)
    result.sensitivity = sum(calls[x])
    result.sensitivity.denominator = sum(x)
    
    
    c(result.specificity,
      result.specificity.denominator,
      result.sensitivity,
      result.sensitivity.denominator)
    
  }
  
  
  c(specificity=result[1]/result[2],
    sensitivity=result[3]/result[4])
  
  
}
