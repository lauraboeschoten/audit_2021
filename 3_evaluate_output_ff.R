library(tidyverse)
library(plyr)

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")

load("datasets_ff.RData")
load("distributions_ff.RData")

results <- readRDS("results_ff.rds")

n_conditions  = 8 #4
n_iterations  = 1000 #1000
n_results     = 9


res_dev    <- vector(mode = "list", length = n_conditions)

res_biasW  <- vector(mode = "list", length = n_conditions)
res_biasXW <- vector(mode = "list", length = n_conditions)

res_llW    <- vector(mode = "list", length = n_conditions)
res_llXW   <- vector(mode = "list", length = n_conditions)

res_ulW    <- vector(mode = "list", length = n_conditions)
res_ulXW   <- vector(mode = "list", length = n_conditions)

res_covW   <- vector(mode = "list", length = n_conditions)
res_covXW  <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  
  res_dev[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 1)
  
  res_biasW[[i]]  <- matrix(NA, ncol = n_iterations, nrow = 3)
  
  res_llW[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_ulW[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_covW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 3)
  
  res_biasXW[[i]] <- matrix(NA, ncol = n_iterations, nrow = 9)
  
  res_llXW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 9)
  res_ulXW[[i]]   <- matrix(NA, ncol = n_iterations, nrow = 9)
  res_covXW[[i]]  <- matrix(NA, ncol = n_iterations, nrow = 9)
  
  popW  <- aggregate(prob ~ W, data = generated_distributions[[i]], sum)
  popXW <- aggregate(prob ~ X + W, data = generated_distributions[[i]], sum) # hier conditionals van maken
  popXW[1:3,"prob"] <- popXW[1:3,"prob"]/sum(popXW[1:3,"prob"])
  popXW[4:6,"prob"] <- popXW[4:6,"prob"]/sum(popXW[4:6,"prob"])
  popXW[7:9,"prob"] <- popXW[7:9,"prob"]/sum(popXW[7:9,"prob"])
  

  
  for(j in 1:n_iterations){
    
    res_dev[[i]][,j]    <- results[[i]][[j]][[6]]
    res_biasW[[i]][,j]  <- abs(popW[,"prob"] - results[[i]][[j]][[7]][,"prop"])  #/popW[,"prob"]
    res_biasXW[[i]][,j] <- abs(popXW[,"prob"] - results[[i]][[j]][[8]][,"prop"]) #/popXW[,"prob"]
    
    # coverage: sample size nog uitrekenen! 
    ssize <- sum(results[[i]][[j]][[9]][results[[i]][[j]][[9]]$Z==1,"freqplus"])
    
    res_llW[[i]][,j]    <-  results[[i]][[j]][[7]][,"prop"] - qt(.975, ssize-1) * sqrt(results[[i]][[j]][[7]][,"var"])
    res_ulW[[i]][,j]    <-  results[[i]][[j]][[7]][,"prop"] + qt(.975, ssize-1) * sqrt(results[[i]][[j]][[7]][,"var"])
    res_covW[[i]][,j]   <-  res_llW[[i]][,j] < popW[,"prob"] & popW[,"prob"] < res_ulW[[i]][,j]
    
    res_llXW[[i]][,j]    <-  results[[i]][[j]][[8]][,"prop"] - qt(.975, ssize-1) * sqrt(results[[i]][[j]][[8]][,"var"])
    res_ulXW[[i]][,j]    <-  results[[i]][[j]][[8]][,"prop"] + qt(.975, ssize-1) * sqrt(results[[i]][[j]][[8]][,"var"])
    res_covXW[[i]][,j]   <-  res_llXW[[i]][,j] < popXW[,"prob"] & popXW[,"prob"] < res_ulXW[[i]][,j]
    
  }
}

deviance = matrix(NA, nrow = n_conditions)
biasW    = matrix(NA, nrow = n_conditions, ncol = 3)
covW     = matrix(NA, nrow = n_conditions, ncol = 3)
biasXW   = matrix(NA, nrow = n_conditions, ncol = 9)
covXW    = matrix(NA, nrow = n_conditions, ncol = 9)

for(i in 1:n_conditions){
  
  deviance[i,1] = mean(res_dev[[i]])
  
  for(j in 1:3){
    
    biasW[i,j] = mean(res_biasW[[i]][j,])
    covW[i,j]  = sum(res_covW[[i]][j,]) / 1000*100
    
  }
  
  for(k in 1:9){
    
    biasXW[i,k]= mean(res_biasXW[[i]][k,])
    covXW[i,k] = sum(res_covXW[[i]][k,]) / 1000*100
  }


  }


save.image("simresults.RData")


# generate output 