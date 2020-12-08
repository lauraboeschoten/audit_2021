
library(tidyverse)
library(dplyr)

set.seed(123)

setwd("F:/CBS_audit_2020-master")

# number of conditions
x_ncon = c(1)
y_ncon = c(1)
z_ncon = c(1)

conditions = list(x_ncon = x_ncon, y_ncon = y_ncon, z_ncon = z_ncon)
xyzw_con = expand.grid(conditions)

n_conditions  = nrow(xyzw_con)
n_iterations  = 1000

WX = list()
WX[[1]] = matrix(c(.9/3, .05/3, .05/3, 
                   .1/3, .8/3, .1/3,
                   .15/3, .15/3, .7/3), nrow = 3, ncol = 3, byrow = TRUE)


WY = list()
WY[[1]] = matrix(c(.8/3, .1/3, .1/3, 
                   .1/3, .8/3, .1/3,
                   .1/3, .1/3, .8/3), nrow = 3, ncol = 3, byrow = TRUE)

XZ = list()
XZ[[1]] = matrix(c(.01, .97/3, 
                   .01, .97/3,
                   .01, .97/3), nrow = 3, ncol = 2, byrow = TRUE)


X = c(1,2,3)
Y = c(1,2,3)
W = c(1,2,3)
Z = c(1,2)

variables = list(X = X, Y = Y, W = W, Z = Z)

generated_datasets = list()
generated_distributions = list()

# loop over all conditions (combinations of different relations between X Y W Z )
for (i in 1:n_conditions){
  
  XYWZ = expand.grid(variables)
  XYW = expand.grid(variables[1:3])
  
  for (j in 1:nrow(XYW)){
    # if X=... and W=... find the corresponding probability in the matrix WX
    if(XYW[j,'W'] == 1 & XYW[j,'X'] == 1){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,1]}
    if(XYW[j,'W'] == 1 & XYW[j,'X'] == 2){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,2]}
    if(XYW[j,'W'] == 1 & XYW[j,'X'] == 3){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,3]}
    if(XYW[j,'W'] == 2 & XYW[j,'X'] == 1){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,1]}
    if(XYW[j,'W'] == 2 & XYW[j,'X'] == 2){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,2]}
    if(XYW[j,'W'] == 2 & XYW[j,'X'] == 3){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,3]}
    if(XYW[j,'W'] == 3 & XYW[j,'X'] == 1){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,1]}
    if(XYW[j,'W'] == 3 & XYW[j,'X'] == 2){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,2]}
    if(XYW[j,'W'] == 3 & XYW[j,'X'] == 3){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,3]}
    # if X=... and Y=... find the corresponding probability in the matrix WY
    if(XYW[j,'W'] == 1 & XYW[j,'Y'] == 1){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,1]}
    if(XYW[j,'W'] == 1 & XYW[j,'Y'] == 2){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,2]}
    if(XYW[j,'W'] == 1 & XYW[j,'Y'] == 3){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,3]}
    if(XYW[j,'W'] == 2 & XYW[j,'Y'] == 1){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,1]}
    if(XYW[j,'W'] == 2 & XYW[j,'Y'] == 2){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,2]}
    if(XYW[j,'W'] == 2 & XYW[j,'Y'] == 3){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,3]}
    if(XYW[j,'W'] == 3 & XYW[j,'Y'] == 1){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,1]}
    if(XYW[j,'W'] == 3 & XYW[j,'Y'] == 2){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,2]}
    if(XYW[j,'W'] == 3 & XYW[j,'Y'] == 3){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,3]}
    
  }
  
  # Assuming independence (relation between XW is independent of relation XY, we multiply the two probabilities
  # multiply the number by 9 to make all final probabilities sum to one)
  XYW[,'prob']                = (XYW[,'WX']*XYW[,'WY'])*3
  
  generated_distributions[[i]] = XYW
  
  # Generate one fixed population of 10000 with the chosen distribution of XYW.
  XYW = cbind(XYW[,1:3], N = rmultinom(1, size = 10000, prob = XYW[,'prob']))
  
  # Now generate 1000 datasets XYWZ corresponding to audit samples drawn from this population
  # according to the probabilities in the matrix XZ (again assuming independence w.r.t. XW and XY)
  XYW <- cbind(XYW, matrix(NA_integer_, nrow = nrow(XYW), ncol = n_iterations))
  
  XZ_pop <- XZ[[as.numeric(paste0(xyzw_con[i,3]))]]
  XZ_pop <- XZ_pop / rowSums(XZ_pop)
  
  for (j in 1:nrow(XYW)) {
    XYW[j, -(1:4)] <- rbinom(n_iterations, size = XYW[j, 'N'], prob = XZ_pop[XYW[j,'X'], 1])
  }
  
  XYWZ <- merge(XYWZ, XYW, by = c('X','Y','W'))
  XYWZ[XYWZ$Z == 2, -(1:5)] <- XYWZ$N[XYWZ$Z == 2] - XYWZ[XYWZ$Z == 2, -(1:5)]
  
  XYWZ <- XYWZ[order(XYWZ$Z, XYWZ$W, XYWZ$Y, XYWZ$X), ]
  row.names(XYWZ) <- NULL
  generated_datasets[[i]] = XYWZ
  
}


### Compute deviance, estimate proportions and estimate variance

source("2a_function_generate_tabs.R")
source("2f_function_loglinear_model.R")
source("2h_function_proportionW_and_WX.R")

results <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  results[[i]] <- vector(mode = "list", length = n_iterations)
}

# loop over conditions
for(i in 1:n_conditions){
  
  # loop over iterations
  for(j in 1:n_iterations){
    cat(sprintf('Condition %d iteration %d', i, j))
    # select dataset j from condition i
    data <- generated_datasets[[i]][,c(1:4,j+5)]
    
    # Generate frequencytab from data
    tab  <- preparetables(data)
    
    tab$X = as.factor(tab$X)
    tab$Y = as.factor(tab$Y)
    
    # deviance independence model
    model_before <- fitmodel(model = 'freq ~ X*Y + Y*Z',
                             tab = tab)
    
    tab[ , 'freqplus'] <- tab[ , 'freq']
    
    tab_audit <- data[data$Z == 1, c('X','Y','W',paste0(j))]
    names(tab_audit)[4] <- 'freq'
    
    prop_list <- results_W_and_WX(tab_audit, tab)
    propW <- prop_list[[1]]
    propWX <- prop_list[[2]]
    
    
    # store results
    reslist = list(model_before$G2, 
                   propW,
                   propWX,
                   tab)
    
    results[[i]][[j]] = reslist
    
  }
  
}


## Evaluate results

res_dev_pre  <- vector(mode = "list", length = n_conditions)

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
  
  res_dev_pre[[i]]  <- matrix(NA, ncol = n_iterations, nrow = 1)
  
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
    
    res_dev_pre[[i]][,j]  <- results[[i]][[j]][[1]]
    
    # absolute bias
    res_biasW[[i]][,j]  <- abs(popW[,"N"] - results[[i]][[j]][[2]][,"prop"])
    res_biasXW[[i]][,j] <- abs(popXW[,"N"] - results[[i]][[j]][[3]][,"prop"])
    
    # bias (including sign)
    res_bias_signW[[i]][,j]  <- results[[i]][[j]][[2]][,"prop"] - popW[,"N"]
    res_bias_signXW[[i]][,j] <- results[[i]][[j]][[3]][,"prop"] - popXW[,"N"]
    
    # coverage: sample size nog uitrekenen! 
    ssize <- sum(results[[i]][[j]][[4]][results[[i]][[j]][[4]]$Z==1,"freqplus"])
    
    res_seW[[i]][,j]    <- sqrt(results[[i]][[j]][[2]][,"var"])
    res_llW[[i]][,j]    <- results[[i]][[j]][[2]][,"prop"] - qt(.975, ssize-1) * res_seW[[i]][,j]
    res_ulW[[i]][,j]    <- results[[i]][[j]][[2]][,"prop"] + qt(.975, ssize-1) * res_seW[[i]][,j]
    res_covW[[i]][,j]   <- res_llW[[i]][,j] < popW[,"N"] & popW[,"N"] < res_ulW[[i]][,j]
    
    res_seXW[[i]][,j]    <- sqrt(results[[i]][[j]][[3]][,"var"])
    res_llXW[[i]][,j]    <- results[[i]][[j]][[3]][,"prop"] - qt(.975, ssize-1) * res_seXW[[i]][,j]
    res_ulXW[[i]][,j]    <- results[[i]][[j]][[3]][,"prop"] + qt(.975, ssize-1) * res_seXW[[i]][,j]
    res_covXW[[i]][,j]   <- res_llXW[[i]][,j] < popXW[,"N"] & popXW[,"N"] < res_ulXW[[i]][,j]
    
  }
  
}

deviance_pre  = matrix(NA, nrow = n_conditions)
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
  
  deviance_pre[i,1]  = mean(res_dev_pre[[i]])
  
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

save.image("simresults_test_varianties_fixedpop.RData")


## Plot the se-sd ratio

data_plot_sesd = as.data.frame(matrix(NA, 12*n_conditions, 4))
colnames(data_plot_sesd) = c("table","condition", "category", "ratio")

data_plot_sesd$table = rep(c(rep("W", 3), rep("XW", 9)), n_conditions)
data_plot_sesd$category = rep(c(c(1:3), c(1:9)), n_conditions)
data_plot_sesd$condition = rep(1:n_conditions, each = 12)
data_plot_sesd$ratio = c(sesd_ratioW[1,], sesd_ratioXW[1,])

plot_sesd = ggplot(data_plot_sesd, 
                   aes(x     = factor(category), 
                       y     = ratio, 
                       group = factor(condition))) +
  geom_point(aes(shape = factor(condition))) +
  geom_hline(yintercept = 1, color = "red") + 
  theme_classic() + ylim(c(0,NA)) +
  labs(x="Category", y="Ratio of standard error to standard deviation")  + 
  facet_wrap(~table, scales = "free_x") 

ggsave("0_test_plot_sesdratio_fixedpop_WY.pdf")
