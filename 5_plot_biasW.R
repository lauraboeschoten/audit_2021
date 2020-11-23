library(tidyverse)
library(plyr)

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")

load("distributions_WX.RData")
dist_WX = generated_distributions
load("distributions_WY.RData")
dist_WY = generated_distributions
load("distributions_XZ.RData")
dist_XZ = generated_distributions

results_WX <- readRDS("results_WX.rds")
results_WY <- readRDS("results_WY.rds")
results_XZ <- readRDS("results_XZ.rds")


n_conditions  = 4 #4
n_iterations  = 1000 #1000
n_results     = 9

pop_WX  <- c(aggregate(prob ~ W, data = dist_WX[[1]], sum)[1,2],
             aggregate(prob ~ W, data = dist_WX[[2]], sum)[1,2],
             aggregate(prob ~ W, data = dist_WX[[3]], sum)[1,2],
             aggregate(prob ~ W, data = dist_WX[[4]], sum)[1,2])
pop_WY  <- c(aggregate(prob ~ W, data = dist_WY[[1]], sum)[1,2],
             aggregate(prob ~ W, data = dist_WY[[2]], sum)[1,2],
             aggregate(prob ~ W, data = dist_WY[[3]], sum)[1,2],
             aggregate(prob ~ W, data = dist_WY[[4]], sum)[1,2])
pop_XZ  <- c(aggregate(prob ~ W, data = dist_XZ[[1]], sum)[1,2],
             aggregate(prob ~ W, data = dist_XZ[[2]], sum)[1,2],
             aggregate(prob ~ W, data = dist_XZ[[3]], sum)[1,2],
             aggregate(prob ~ W, data = dist_XZ[[4]], sum)[1,2])


res_WX_biasW <- matrix(NA, 1000, 4)
res_WY_biasW <- matrix(NA, 1000, 4)
res_XZ_biasW <- matrix(NA, 1000, 4)

for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    res_WX_biasW[j,i] <- abs(results_WX[[i]][[j]][[7]][1,1] - pop_WX[i])
    res_WY_biasW[j,i] <- abs(results_WY[[i]][[j]][[7]][1,1] - pop_WY[i])
    res_XZ_biasW[j,i] <- abs(results_XZ[[i]][[j]][[7]][1,1] - pop_XZ[i])
  }
}



data_plot_biasW = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 3))
colnames(data_plot_biasW) = c("study","condition", "biasW")

data_plot_biasW$study     = rep(c("condition WX", "condition WY", "condition XZ"), each = n_conditions*n_iterations)
data_plot_biasW$condition = rep(rep(1:4, each = n_iterations), 3)
data_plot_biasW$biasW  = c(res_WX_biasW[,1:4], 
                           res_WY_biasW[,1:4],
                           res_XZ_biasW[,1:4])

plot_biasW = ggplot(data_plot_biasW, aes(x = factor(condition), y = biasW)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Absolute Bias")  +
  ylim(0, 0.06) +
  facet_grid(~study)

ggsave("plot_biasW.pdf")

# ------------------------------------------------------------------------------

setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")

load("simresults_ff.RData")
dist_ff = generated_distributions


results_ff <- readRDS("results_ff.rds")

n_conditions  = 8 #4
n_iterations  = 1000 #1000
n_results     = 9

pop_ff  <- c(aggregate(prob ~ W, data = dist_ff[[1]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[2]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[3]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[4]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[5]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[6]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[7]], sum)[1,2],
             aggregate(prob ~ W, data = dist_ff[[8]], sum)[1,2])


res_ff_biasW <- matrix(NA, n_iterations, n_conditions)


for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    res_ff_biasW[j,i] <- abs(results_ff[[i]][[j]][[7]][1,1] - pop_ff[i])

  }
}



data_plot_biasW_ff = as.data.frame(matrix(NA, n_conditions*n_iterations, 2))
colnames(data_plot_biasW_ff) = c("condition", "biasW")

conds = c("WX1, WY1, XZ1",
          "WX4, WY1, XZ1",
          "WX1, WY4, XZ1",
          "WX4, WY4, XZ1",
          "WX1, WY1, XZ4",
          "WX4, WY1, XZ4",
          "WX1, WY4, XZ4",
          "WX4, WY4, XZ4")

data_plot_biasW_ff$condition = rep(conds, each = n_iterations)
data_plot_biasW_ff$biasW  = c(res_ff_biasW[,1:8])

plot_biasW_ff = ggplot(data_plot_biasW_ff, aes(x = factor(condition), y = biasW)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + 
  labs(x="Condition", y="Absolute Bias") + 
  ylim(0, 0.06) + 
  theme(axis.text.x = element_text(angle=45))

ggsave("plot_biasW_ff.pdf")
