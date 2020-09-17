library(tidyverse)
library(plyr)

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")

load("datasets_ff.RData")
load("distributions_ff.RData")

results <- readRDS("results_ff.rds")

n_conditions  = 4 #36
n_iterations  = 10 #1000
n_results     = 6


simres <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  simres[[i]] <- vector(mode = "list", length = n_iterations)
}


# generate output 