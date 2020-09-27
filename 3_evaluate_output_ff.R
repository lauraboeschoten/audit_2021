library(tidyverse)
library(plyr)

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")

load("datasets_ff.RData")
load("distributions_ff.RData")

results <- readRDS("results_ff.rds")

n_conditions  = 6 #4
n_iterations  = 2 #1000
n_results     = 9


res_dev    <- vector(mode = "list", length = n_conditions)
res_biasW  <- vector(mode = "list", length = n_conditions)
res_covW   <- vector(mode = "list", length = n_conditions)
res_biasXW <- vector(mode = "list", length = n_conditions)
res_covXW  <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  res_dev[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 1)
  res_biasW[[i]]  <- matrix(NA, ncol = n_iterations, nrow = 3)
  res_biasXW[[i]] <- matrix(NA, ncol = n_iterations, nrow = 9)
  
  popW  <- aggregate(prob ~ W, data = generated_distributions[[i]], sum)
  popXW <- aggregate(prob ~ X + W, data = generated_distributions[[i]], sum) # hier conditionals van maken
  popXW[1:3,"prob"]/sum(popXW[1:3,"prob"])
  

  
  for(j in 1:n_iterations){
    
    res_dev[[i]][,j]    <- results[[i]][[j]][[6]]
    res_biasW[[i]][,j]  <- abs(popW[,"prob"] - results[[i]][[j]][[7]][,"prop"])/popW[,"prob"]
    res_biasXW[[i]][,j] <- abs(popXW[,"prob"] - results[[i]][[j]][[8]][,"prop"])/popXW[,"prob"]
    
  }
}


# generate output 