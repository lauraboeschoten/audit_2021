library(tidyverse)
library(plyr)

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")
setwd("F:/CBS_audit_2020-master")

load("datasets_fixedpop_WY.RData")
load("distributions_fixedpop_WY.RData")

results <- readRDS("results_fixedpop_WY.rds")

n_conditions  = 5 #4
n_iterations  = 1000 #1000
n_results     = 9


res_dev      <- vector(mode = "list", length = n_conditions)
res_dev_pre  <- vector(mode = "list", length = n_conditions)
res_dev_post <- vector(mode = "list", length = n_conditions)

res_biasW  <- vector(mode = "list", length = n_conditions)
res_biasXW <- vector(mode = "list", length = n_conditions)

res_bias_signW  <- vector(mode = "list", length = n_conditions)
res_bias_signXW <- vector(mode = "list", length = n_conditions)

res_seW    <- vector(mode = "list", length = n_conditions)
res_seXW   <- vector(mode = "list", length = n_conditions)

res_llW    <- vector(mode = "list", length = n_conditions)
res_llXW   <- vector(mode = "list", length = n_conditions)

res_ulW    <- vector(mode = "list", length = n_conditions)
res_ulXW   <- vector(mode = "list", length = n_conditions)

res_covW   <- vector(mode = "list", length = n_conditions)
res_covXW  <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  
  res_dev[[i]]      <- matrix(NA, ncol = n_iterations, nrow = 1)
  res_dev_pre[[i]]  <- matrix(NA, ncol = n_iterations, nrow = 1)
  res_dev_post[[i]] <- matrix(NA, ncol = n_iterations, nrow = 1)
  
  res_biasW[[i]]      <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_bias_signW[[i]] <- matrix(NA, ncol = n_iterations, nrow = 3)
  
  res_seW[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_llW[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_ulW[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_covW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 3)
  
  res_biasXW[[i]]      <- matrix(NA, ncol = n_iterations, nrow = 9)
  res_bias_signXW[[i]] <- matrix(NA, ncol = n_iterations, nrow = 9)
  
  res_seXW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 9)
  res_llXW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 9)
  res_ulXW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 9)
  res_covXW[[i]]  <- matrix(NA, ncol = n_iterations, nrow = 9)
  
  popW            <- aggregate(N ~ W, data = generated_datasets[[i]], sum)
  popW$N          <- popW$N / sum(generated_datasets[[i]]$N)
  
  popXW           <- aggregate(N ~ X + W, data = generated_datasets[[i]], sum)
  popXW[1:3,"N"]  <- popXW[1:3,"N"]/sum(popXW[1:3,"N"])
  popXW[4:6,"N"]  <- popXW[4:6,"N"]/sum(popXW[4:6,"N"])
  popXW[7:9,"N"]  <- popXW[7:9,"N"]/sum(popXW[7:9,"N"])
  
  for(j in 1:n_iterations){
    
    res_dev[[i]][,j]      <- results[[i]][[j]][[6]]
    res_dev_pre[[i]][,j]  <- results[[i]][[j]][[4]]
    res_dev_post[[i]][,j] <- results[[i]][[j]][[5]]
    
    # absolute bias
    res_biasW[[i]][,j]  <- abs(popW[,"N"] - results[[i]][[j]][[7]][,"prop"])
    res_biasXW[[i]][,j] <- abs(popXW[,"N"] - results[[i]][[j]][[8]][,"prop"])
    
    # bias (including sign)
    res_bias_signW[[i]][,j]  <- results[[i]][[j]][[7]][,"prop"] - popW[,"N"]
    res_bias_signXW[[i]][,j] <- results[[i]][[j]][[8]][,"prop"] - popXW[,"N"]
    
    # coverage: sample size nog uitrekenen! 
    ssize <- sum(results[[i]][[j]][[9]][results[[i]][[j]][[9]]$Z==1,"freqplus"])
    
    res_seW[[i]][,j]    <- sqrt(results[[i]][[j]][[7]][,"var"])
    res_llW[[i]][,j]    <- results[[i]][[j]][[7]][,"prop"] - qt(.975, ssize-1) * res_seW[[i]][,j]
    res_ulW[[i]][,j]    <- results[[i]][[j]][[7]][,"prop"] + qt(.975, ssize-1) * res_seW[[i]][,j]
    res_covW[[i]][,j]   <- res_llW[[i]][,j] < popW[,"N"] & popW[,"N"] < res_ulW[[i]][,j]
    
    res_seXW[[i]][,j]    <- sqrt(results[[i]][[j]][[8]][,"var"])
    res_llXW[[i]][,j]    <- results[[i]][[j]][[8]][,"prop"] - qt(.975, ssize-1) * res_seXW[[i]][,j]
    res_ulXW[[i]][,j]    <- results[[i]][[j]][[8]][,"prop"] + qt(.975, ssize-1) * res_seXW[[i]][,j]
    res_covXW[[i]][,j]   <- res_llXW[[i]][,j] < popXW[,"N"] & popXW[,"N"] < res_ulXW[[i]][,j]
    
  }
  
}

deviance      = matrix(NA, nrow = n_conditions)
deviance_pre  = matrix(NA, nrow = n_conditions)
deviance_post = matrix(NA, nrow = n_conditions)
biasW         = matrix(NA, nrow = n_conditions, ncol = 3)
bias_signW    = matrix(NA, nrow = n_conditions, ncol = 3)
sdW           = matrix(NA, nrow = n_conditions, ncol = 3)
sesd_ratioW   = matrix(NA, nrow = n_conditions, ncol = 3)
covW          = matrix(NA, nrow = n_conditions, ncol = 3)
biasXW        = matrix(NA, nrow = n_conditions, ncol = 9)
bias_signXW   = matrix(NA, nrow = n_conditions, ncol = 9)
sdXW          = matrix(NA, nrow = n_conditions, ncol = 9)
sesd_ratioXW  = matrix(NA, nrow = n_conditions, ncol = 9)
covXW         = matrix(NA, nrow = n_conditions, ncol = 9)

for(i in 1:n_conditions){
  
  deviance[i,1]      = mean(res_dev[[i]])
  deviance_pre[i,1]  = mean(res_dev_pre[[i]])
  deviance_post[i,1] = mean(res_dev_post[[i]])
  
  for(j in 1:3){
    
    biasW[i,j]       = mean(res_biasW[[i]][j,])
    bias_signW[i,j]  = mean(res_bias_signW[[i]][j,])
    sdW[i,j]         = sd(res_bias_signW[[i]][j,])
    sesd_ratioW[i,j] = mean(res_seW[[i]][j,]) / sdW[i,j]
    covW[i,j]        = sum(res_covW[[i]][j,]) / 1000*100
    
  }
  
  for(k in 1:9){
    
    biasXW[i,k]       = mean(res_biasXW[[i]][k,])
    bias_signXW[i,k]  = mean(res_bias_signXW[[i]][k,])
    sdXW[i,k]         = sd(res_bias_signXW[[i]][k,])
    sesd_ratioXW[i,k] = mean(res_seXW[[i]][k,]) / sdXW[i,k]
    covXW[i,k]        = sum(res_covXW[[i]][k,]) / 1000*100
  }

}


save.image("simresults_fixedpop_WY.RData")


