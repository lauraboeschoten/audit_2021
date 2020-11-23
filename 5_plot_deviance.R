library(tidyverse)
library(plyr)

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")

load("datasets_WX.RData")
load("distributions_WX.RData")
load("simresults_WX.RData")

results_WX <- readRDS("results_WX.rds")
results_WY <- readRDS("results_WY.rds")
results_XZ <- readRDS("results_XZ.rds")

results_dev_WX <- readRDS("results_WX_dev.rds")
results_dev_WY <- readRDS("results_WY_dev.rds")
results_dev_XZ <- readRDS("results_XZ_dev.rds")

res_dev_WX    <- vector(mode = "list", length = n_conditions)
res_dev_WY    <- vector(mode = "list", length = n_conditions)
res_dev_XZ    <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  
  res_dev_WX[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 1)
  res_dev_WY[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 1)
  res_dev_XZ[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 1)
  
  for(j in 1:n_iterations){
    
    res_dev_WX[[i]][,j]    <- (results_dev_WX[[i]][[j]][[1]] - results_WX[[i]][[j]][[5]]) / results_dev_WX[[i]][[j]][[1]]
    res_dev_WY[[i]][,j]    <- (results_dev_WY[[i]][[j]][[1]] - results_WY[[i]][[j]][[5]]) / results_dev_WY[[i]][[j]][[1]]
    res_dev_XZ[[i]][,j]    <- (results_dev_XZ[[i]][[j]][[1]] - results_XZ[[i]][[j]][[5]]) / results_dev_XZ[[i]][[j]][[1]]
  }
}

n_conditions  = 4 #4
n_iterations  = 1000 #1000
n_results     = 9

## Deviance plot

data_plot_deviance = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 3))
colnames(data_plot_deviance) = c("study","condition", "deviance")

data_plot_deviance$study     = rep(c("condition WX", "condition WY", "condition XZ"), each = n_conditions*n_iterations)
data_plot_deviance$condition = rep(rep(1:4, each = n_iterations), 3)
data_plot_deviance$deviance  = c(res_dev_WX[[1]], res_dev_WX[[2]], res_dev_WX[[3]], res_dev_WX[[4]],
                                 res_dev_WY[[1]], res_dev_WY[[2]], res_dev_WY[[3]], res_dev_WY[[4]],
                                 res_dev_XZ[[1]], res_dev_XZ[[2]], res_dev_XZ[[3]], res_dev_XZ[[4]])

plot_deviance = ggplot(data_plot_deviance, aes(x = factor(condition), y = (1-deviance))) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Relative deviance")  +
  facet_grid(~study)

ggsave("plot_deviance.pdf")


# ------------------------------------------------------------------------------

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")

load("datasets_ff.RData")
load("distributions_ff.RData")
load("simresults_ff.RData")

results_ff     <- readRDS("results_ff.rds")
results_dev_ff <- readRDS("results_ff_dev.rds")

n_conditions  = 8 #4
n_iterations  = 1000 #1000
n_results     = 9

res_dev_ff    <- vector(mode = "list", length = n_conditions)


for(i in 1:n_conditions){
  
  res_dev_ff[[i]]    <- matrix(NA, ncol = n_iterations, nrow = 1)
  
  for(j in 1:n_iterations){
    
    res_dev_ff[[i]][,j]    <- (results_dev_ff[[i]][[j]][[1]] - results_ff[[i]][[j]][[5]]) / results_dev_ff[[i]][[j]][[1]]
    }
}


## Deviance plot

data_plot_deviance = as.data.frame(matrix(NA, n_conditions*n_iterations, 2))
colnames(data_plot_deviance) = c("condition", "deviance")

conds = c("WX1, WY1, XZ1",
          "WX4, WY1, XZ1",
          "WX1, WY4, XZ1",
          "WX4, WY4, XZ1",
          "WX1, WY1, XZ4",
          "WX4, WY1, XZ4",
          "WX1, WY4, XZ4",
          "WX4, WY4, XZ4")

data_plot_deviance$condition = rep(conds, each = n_iterations)




data_plot_deviance$deviance  = c(res_dev_ff[[1]], res_dev_ff[[2]], res_dev_ff[[3]], res_dev_ff[[4]],
                                 res_dev_ff[[5]], res_dev_ff[[6]], res_dev_ff[[7]], res_dev_ff[[8]])

plot_deviance = ggplot(data_plot_deviance, aes(x = factor(condition), y = (1-deviance))) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Relative deviance") +
  ylim(0, 0.8) + 
  theme(axis.text.x = element_text(angle=45))

ggsave("plot_deviance_ff.pdf")


