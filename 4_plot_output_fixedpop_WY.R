library(tidyverse)
library(plyr)

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")
setwd("F:/CBS_audit_2020-master")

load("datasets_fixedpop_WY.RData")
load("distributions_fixedpop_WY.RData")
load("simresults_fixedpop_WY.RData")

results <- readRDS("results_fixedpop_WY.rds")

n_conditions  = 5 #4
n_iterations  = 1000 #1000
n_results     = 9

## Deviance plots

# reduction in deviance
data_plot_deviance = as.data.frame(matrix(NA, n_conditions*n_iterations, 2))
colnames(data_plot_deviance) = c("condition", "deviance")

data_plot_deviance$condition = rep(1:n_conditions, each = n_iterations)
data_plot_deviance$deviance  = c(res_dev[[1]], res_dev[[2]], res_dev[[3]], res_dev[[4]], res_dev[[5]])

plot_deviance = ggplot(data_plot_deviance, aes(x = factor(condition), y = (1-deviance))) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Relative deviance") 

ggsave("plot_deviance_fixedpop_WY.pdf")


# deviance before and after optimization
data_plot_deviance2 = as.data.frame(matrix(NA, n_conditions*n_iterations, 3))
colnames(data_plot_deviance2) = c("condition", "deviance_pre", "deviance_post")

data_plot_deviance2$condition = rep(1:n_conditions, each = n_iterations)
data_plot_deviance2$deviance_pre  = c(res_dev_pre[[1]], res_dev_pre[[2]], res_dev_pre[[3]], res_dev_pre[[4]], res_dev_pre[[5]])
data_plot_deviance2$deviance_post = c(res_dev_post[[1]], res_dev_post[[2]], res_dev_post[[3]], res_dev_post[[4]], res_dev_post[[5]])

plot_deviance_pre = ggplot(data_plot_deviance2, aes(x = factor(condition), y = (deviance_pre))) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Deviance (before optimization)") 

ggsave("plot_deviance_pre_fixedpop_WY.pdf")

plot_deviance_post = ggplot(data_plot_deviance2, aes(x = factor(condition), y = (deviance_post))) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Deviance (after optimization)") 

ggsave("plot_deviance_post_fixedpop_WY.pdf")



## Bias W plot

data_plot_biasW = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 3))
colnames(data_plot_biasW) = c("condition", "categoryW", "bias")

data_plot_biasW$condition = rep(1:n_conditions, each=n_iterations*3)
data_plot_biasW$categoryW = rep(rep(1:3, each=n_iterations), n_conditions)
data_plot_biasW$bias      = c(res_biasW[[1]][1,], res_biasW[[1]][2,], res_biasW[[1]][3,],
                              res_biasW[[2]][1,], res_biasW[[2]][2,], res_biasW[[2]][3,],
                              res_biasW[[3]][1,], res_biasW[[3]][2,], res_biasW[[3]][3,],
                              res_biasW[[4]][1,], res_biasW[[4]][2,], res_biasW[[4]][3,],
                              res_biasW[[5]][1,], res_biasW[[5]][2,], res_biasW[[5]][3,])

plot_biasW = ggplot(data_plot_biasW, aes(x = factor(categoryW), y = bias)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Absolute bias")  + 
  facet_grid(~condition)

ggsave("plot_biasW_fixedpop_WY.pdf")

# including sign
data_plot_bias_signW = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 3))
colnames(data_plot_bias_signW) = c("condition", "categoryW", "bias")

data_plot_bias_signW$condition = rep(1:n_conditions, each=n_iterations*3)
data_plot_bias_signW$categoryW = rep(rep(1:3, each=n_iterations), n_conditions)
data_plot_bias_signW$bias      = c(res_bias_signW[[1]][1,], res_bias_signW[[1]][2,], res_bias_signW[[1]][3,],
                                   res_bias_signW[[2]][1,], res_bias_signW[[2]][2,], res_bias_signW[[2]][3,],
                                   res_bias_signW[[3]][1,], res_bias_signW[[3]][2,], res_bias_signW[[3]][3,],
                                   res_bias_signW[[4]][1,], res_bias_signW[[4]][2,], res_bias_signW[[4]][3,],
                                   res_bias_signW[[5]][1,], res_bias_signW[[5]][2,], res_bias_signW[[5]][3,])

plot_bias_signW = ggplot(data_plot_bias_signW, aes(x = factor(categoryW), y = bias)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  geom_hline(yintercept = 0, color = "red") +
  labs(x="Condition", y="Estimation error")  + 
  facet_grid(~condition)

ggsave("plot_bias_signW_fixedpop_WY.pdf")



## Bias XW plot

data_plot_biasXW = as.data.frame(matrix(NA, n_conditions*n_iterations*9, 3))
colnames(data_plot_biasXW) = c("condition", "categoryXW", "bias")

data_plot_biasXW$condition = rep(1:n_conditions, each=n_iterations*9)
data_plot_biasXW$categoryXW = rep(rep(1:9, each=n_iterations), n_conditions)
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
                               res_biasXW[[5]][7,], res_biasXW[[5]][8,], res_biasXW[[5]][9,]
)

plot_biasXW = ggplot(data_plot_biasXW, aes(x = factor(categoryXW), y = bias)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Absolute bias")  + 
  facet_grid(~condition)

ggsave("plot_biasXW_fixedpop_WY.pdf")


# including sign

data_plot_bias_signXW = as.data.frame(matrix(NA, n_conditions*n_iterations*9, 3))
colnames(data_plot_bias_signXW) = c("condition", "categoryXW", "bias")

data_plot_bias_signXW$condition = rep(1:n_conditions, each=n_iterations*9)
data_plot_bias_signXW$categoryXW = rep(rep(1:9, each=n_iterations), n_conditions)
data_plot_bias_signXW$bias      = c(res_bias_signXW[[1]][1,], res_bias_signXW[[1]][2,], res_bias_signXW[[1]][3,],
                               res_bias_signXW[[1]][4,], res_bias_signXW[[1]][5,], res_bias_signXW[[1]][6,],
                               res_bias_signXW[[1]][7,], res_bias_signXW[[1]][8,], res_bias_signXW[[1]][9,],
                               
                               res_bias_signXW[[2]][1,], res_bias_signXW[[2]][2,], res_bias_signXW[[2]][3,],
                               res_bias_signXW[[2]][4,], res_bias_signXW[[2]][5,], res_bias_signXW[[2]][6,],
                               res_bias_signXW[[2]][7,], res_bias_signXW[[2]][8,], res_bias_signXW[[2]][9,],
                               
                               res_bias_signXW[[3]][1,], res_bias_signXW[[3]][2,], res_bias_signXW[[3]][3,],
                               res_bias_signXW[[3]][4,], res_bias_signXW[[3]][5,], res_bias_signXW[[3]][6,],
                               res_bias_signXW[[3]][7,], res_bias_signXW[[3]][8,], res_bias_signXW[[3]][9,],
                               
                               res_bias_signXW[[4]][1,], res_bias_signXW[[4]][2,], res_bias_signXW[[4]][3,],
                               res_bias_signXW[[4]][4,], res_bias_signXW[[4]][5,], res_bias_signXW[[4]][6,],
                               res_bias_signXW[[4]][7,], res_bias_signXW[[4]][8,], res_bias_signXW[[4]][9,],
                               
                               res_bias_signXW[[5]][1,], res_bias_signXW[[5]][2,], res_bias_signXW[[5]][3,],
                               res_bias_signXW[[5]][4,], res_bias_signXW[[5]][5,], res_bias_signXW[[5]][6,],
                               res_bias_signXW[[5]][7,], res_bias_signXW[[5]][8,], res_bias_signXW[[5]][9,]
)

plot_bias_signXW = ggplot(data_plot_bias_signXW, aes(x = factor(categoryXW), y = bias)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  geom_hline(yintercept = 0, color = "red") +
  labs(x="Condition", y="Estimation error")  + 
  facet_grid(~condition)

ggsave("plot_bias_signXW_fixedpop_WY.pdf")


## coverage plots

data_plot_coverage = as.data.frame(matrix(NA, 12*n_conditions, 4))
colnames(data_plot_coverage) = c("table","condition", "category", "coverage")

data_plot_coverage$table = rep(c(rep("W", 3), rep("XW", 9)), n_conditions)
data_plot_coverage$category = rep(c(c(1:3), c(1:9)), n_conditions)
data_plot_coverage$condition = rep(1:n_conditions, each = 12)
data_plot_coverage$coverage = c(covW[1,], covXW[1,],
                                covW[2,], covXW[2,],
                                covW[3,], covXW[3,],
                                covW[4,], covXW[4,],
                                covW[5,], covXW[5,])

data_plot_coverage[(data_plot_coverage) == 0] <- NA
data_plot_coverage[(data_plot_coverage) == 100] <- NA

plot_coverage = ggplot(data_plot_coverage, 
                       aes(x     = factor(category), 
                           y     = coverage, 
                           group = factor(condition))) +
  geom_point(aes(shape = factor(condition))) +
  geom_hline(yintercept = 95, color = "red") + 
  theme_classic() + 
  labs(x="Category", y="Coverage of the 95 CI")  + 
  facet_wrap(~table, scales = "free_x") 

ggsave("plot_coverage_fixedpop_WY.pdf")


## se-sd ratio plots

data_plot_sesd = as.data.frame(matrix(NA, 12*n_conditions, 4))
colnames(data_plot_sesd) = c("table","condition", "category", "ratio")

data_plot_sesd$table = rep(c(rep("W", 3), rep("XW", 9)), n_conditions)
data_plot_sesd$category = rep(c(c(1:3), c(1:9)), n_conditions)
data_plot_sesd$condition = rep(1:n_conditions, each = 12)
data_plot_sesd$ratio = c(sesd_ratioW[1,], sesd_ratioXW[1,],
                         sesd_ratioW[2,], sesd_ratioXW[2,],
                         sesd_ratioW[3,], sesd_ratioXW[3,],
                         sesd_ratioW[4,], sesd_ratioXW[4,],
                         sesd_ratioW[5,], sesd_ratioXW[5,])

plot_sesd = ggplot(data_plot_sesd, 
                   aes(x     = factor(category), 
                       y     = ratio, 
                       group = factor(condition))) +
  geom_point(aes(shape = factor(condition))) +
  geom_hline(yintercept = 1, color = "red") + 
  theme_classic() + ylim(c(0,NA)) +
  labs(x="Category", y="Ratio of standard error to standard deviation")  + 
  facet_wrap(~table, scales = "free_x") 

ggsave("plot_sesdratio_fixedpop_WY.pdf")
