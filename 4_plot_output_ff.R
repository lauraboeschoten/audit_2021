library(tidyverse)
library(plyr)

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")

load("datasets_ff.RData")
load("distributions_ff.RData")
load("simresults_ff.RData")

results <- readRDS("results_ff.rds")

n_conditions  = 8 #4
n_iterations  = 1000 #1000
n_results     = 9

## Deviance plot

data_plot_deviance = as.data.frame(matrix(NA, n_conditions*n_iterations, 2))
colnames(data_plot_deviance) = c("condition", "deviance")

data_plot_deviance$condition = rep(1:8, each = n_iterations)
data_plot_deviance$deviance  = c(res_dev[[1]], res_dev[[2]], res_dev[[3]], res_dev[[4]],
                                 res_dev[[5]], res_dev[[6]], res_dev[[7]], res_dev[[8]])

plot_deviance = ggplot(data_plot_deviance, aes(x = factor(condition), y = (1-deviance))) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Relative deviance") 

ggsave("plot_deviance_ff.pdf")


## Bias W plot

data_plot_biasW = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 3))
colnames(data_plot_biasW) = c("condition", "categoryW", "bias")

data_plot_biasW$condition = rep(1:8, each=n_iterations*3)
data_plot_biasW$categoryW = rep(rep(1:3, each=n_iterations), 4)
data_plot_biasW$bias      = c(res_biasW[[1]][1,], res_biasW[[1]][2,], res_biasW[[1]][3,],
                              res_biasW[[2]][1,], res_biasW[[2]][2,], res_biasW[[2]][3,],
                              res_biasW[[3]][1,], res_biasW[[3]][2,], res_biasW[[3]][3,],
                              res_biasW[[4]][1,], res_biasW[[4]][2,], res_biasW[[4]][3,],
                              res_biasW[[5]][1,], res_biasW[[5]][2,], res_biasW[[5]][3,],
                              res_biasW[[6]][1,], res_biasW[[6]][2,], res_biasW[[6]][3,],
                              res_biasW[[7]][1,], res_biasW[[7]][2,], res_biasW[[7]][3,],
                              res_biasW[[8]][1,], res_biasW[[8]][2,], res_biasW[[8]][3,])

plot_biasW = ggplot(data_plot_biasW, aes(x = factor(categoryW), y = bias)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Absolute bias")  + 
  facet_grid(~condition)

ggsave("plot_biasW_ff.pdf")

## Bias XW plot

data_plot_biasXW = as.data.frame(matrix(NA, n_conditions*n_iterations*9, 3))
colnames(data_plot_biasXW) = c("condition", "categoryXW", "bias")

data_plot_biasXW$condition = rep(1:8, each=n_iterations*9)
data_plot_biasXW$categoryXW = rep(rep(1:9, each=n_iterations), 8)
data_plot_biasXW$bias      = c(res_biasXW[[1]][1,], res_biasXW[[1]][2,], res_biasXW[[1]][3,],
                               res_biasXW[[1]][4,], res_biasXW[[1]][5,], res_biasXW[[1]][6,],
                               res_biasXW[[1]][7,], res_biasXW[[1]][8,], res_biasXW[[1]][9,],
                               
                               res_biasXW[[2]][1,], res_biasXW[[2]][2,], res_biasXW[[2]][3,],
                               res_biasXW[[2]][4,], res_biasXW[[2]][5,], res_biasXW[[2]][6,],
                               res_biasXW[[2]][7,], res_biasXW[[2]][8,], res_biasXW[[2]][9,],
                               
                               res_biasXW[[3]][1,], res_biasXW[[3]][2,], res_biasXW[[3]][3,],
                               res_biasXW[[3]][4,], res_biasXW[[3]][5,], res_biasXW[[3]][6,],
                               res_biasXW[[3]][7,], res_biasXW[[3]][8,], res_biasXW[[3]][9,],
                               
                               res_biasXW[[4]][1,], res_biasXW[[4]][2,], res_biasXW[[4]][3,],
                               res_biasXW[[4]][4,], res_biasXW[[4]][5,], res_biasXW[[4]][6,],
                               res_biasXW[[4]][7,], res_biasXW[[4]][8,], res_biasXW[[4]][9,],
                               
                               res_biasXW[[5]][1,], res_biasXW[[5]][2,], res_biasXW[[5]][3,],
                               res_biasXW[[5]][4,], res_biasXW[[5]][5,], res_biasXW[[5]][6,],
                               res_biasXW[[5]][7,], res_biasXW[[5]][8,], res_biasXW[[5]][9,],
                               
                               res_biasXW[[6]][1,], res_biasXW[[6]][2,], res_biasXW[[6]][3,],
                               res_biasXW[[6]][4,], res_biasXW[[6]][5,], res_biasXW[[6]][6,],
                               res_biasXW[[6]][7,], res_biasXW[[6]][8,], res_biasXW[[6]][9,],
                               
                               res_biasXW[[7]][1,], res_biasXW[[7]][2,], res_biasXW[[7]][3,],
                               res_biasXW[[7]][4,], res_biasXW[[7]][5,], res_biasXW[[7]][6,],
                               res_biasXW[[7]][7,], res_biasXW[[7]][8,], res_biasXW[[7]][9,],
                               
                               res_biasXW[[8]][1,], res_biasXW[[8]][2,], res_biasXW[[8]][3,],
                               res_biasXW[[8]][4,], res_biasXW[[8]][5,], res_biasXW[[8]][6,],
                               res_biasXW[[8]][7,], res_biasXW[[8]][8,], res_biasXW[[8]][9,])

plot_biasXW = ggplot(data_plot_biasXW, aes(x = factor(categoryXW), y = bias)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Absolute bias")  + 
  facet_grid(~condition)

ggsave("plot_biasXW_ff.pdf")

## coverage plots

data_plot_coverage = as.data.frame(matrix(NA, 12*8, 4))
colnames(data_plot_coverage) = c("table","condition", "category", "coverage")

data_plot_coverage$table = rep(c(rep("W", 3), rep("XW", 9)), 8)
data_plot_coverage$category = rep(c(c(1:3), c(1:9)), 8)
data_plot_coverage$condition = rep(1:8, each = 12)
data_plot_coverage$coverage = c(covW[1,], covXW[1,],
                                covW[2,], covXW[2,],
                                covW[3,], covXW[3,],
                                covW[4,], covXW[4,],
                                covW[5,], covXW[5,],
                                covW[6,], covXW[6,],
                                covW[7,], covXW[7,],
                                covW[8,], covXW[8,])

data_plot_coverage[(data_plot_coverage) == 0] <- NA
data_plot_coverage[(data_plot_coverage) == 100] <- NA

plot_coverage = ggplot(data_plot_coverage, 
                       aes(x     = factor(category), 
                           y     = coverage, 
                           group = factor(condition))) +
  geom_point(aes(shape = factor(condition))) +
  scale_shape_manual(values = c(0:7)) +
  geom_hline(yintercept = 95) + 
  theme_classic() + 
  labs(x="Category", y="Coverage of the 95 CI")  + 
  facet_wrap(~table, scales = "free_x") 

ggsave("plot_coverage_ff.pdf")
