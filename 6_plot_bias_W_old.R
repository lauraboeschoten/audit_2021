library(tidyverse)
library(plyr)

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")
setwd("//cbsp.nl/Productie/Secundair/MPOnderzoek/Werk/Combineren/Projecten/MeetfoutenEnergiebestanden/Artikel/audit_2021-master_nieuw")

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

res_WX_biasW_old <- matrix(NA, 1000, 4)
res_WY_biasW_old <- matrix(NA, 1000, 4)
res_XZ_biasW_old <- matrix(NA, 1000, 4)

for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    
    res_WX_biasW_old[j,i] <- aggregate(freq ~ X, data=results_WX[[i]][[j]][[9]][1:9,], sum)[1,2] /
      sum(aggregate(freq ~ X, data=results_WX[[i]][[j]][[9]][1:9,], sum)) - pop_WX[i]
    res_WY_biasW_old[j,i] <- aggregate(freq ~ X, data=results_WY[[i]][[j]][[9]][1:9,], sum)[1,2] /
      sum(aggregate(freq ~ X, data=results_WY[[i]][[j]][[9]][1:9,], sum)) - pop_WY[i]
    res_XZ_biasW_old[j,i] <- aggregate(freq ~ X, data=results_XZ[[i]][[j]][[9]][1:9,], sum)[1,2] /
      sum(aggregate(freq ~ X, data=results_XZ[[i]][[j]][[9]][1:9,], sum)) - pop_XZ[i]
    
    res_WX_biasW[j,i] <- results_WX[[i]][[j]][[7]][1,1] - pop_WX[i]
    res_WY_biasW[j,i] <- results_WY[[i]][[j]][[7]][1,1] - pop_WY[i]
    res_XZ_biasW[j,i] <- results_XZ[[i]][[j]][[7]][1,1] - pop_XZ[i]
  }
}



data_plot_biasW_old = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 4))
colnames(data_plot_biasW_old) = c("moment","study","condition", "biasW")

data_plot_biasW_old$moment    = rep("1.Without procedure", nrow(data_plot_biasW_old))
data_plot_biasW_old$study     = rep(c("condition WX", "condition WY", "condition XZ"), each = n_conditions*n_iterations)
data_plot_biasW_old$condition = rep(rep(1:4, each = n_iterations), 3)
data_plot_biasW_old$biasW  = c(res_WX_biasW_old[,1:4], 
                               res_WY_biasW_old[,1:4],
                               res_XZ_biasW_old[,1:4])

data_plot_biasW = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 4))
colnames(data_plot_biasW) = c("moment","study","condition", "biasW")

data_plot_biasW$moment    = rep("2. With procedure", nrow(data_plot_biasW))
data_plot_biasW$study     = rep(c("condition WX", "condition WY", "condition XZ"), each = n_conditions*n_iterations)
data_plot_biasW$condition = rep(rep(1:4, each = n_iterations), 3)
data_plot_biasW$biasW  = c(res_WX_biasW[,1:4], 
                           res_WY_biasW[,1:4],
                           res_XZ_biasW[,1:4])

data_plot_biasW_total = rbind(data_plot_biasW_old, data_plot_biasW)


plot_biasW = ggplot(data_plot_biasW_total, aes(x = factor(condition), y = biasW, fill=factor(moment))) +
  geom_boxplot() +
  theme_classic() + 
  labs(x="Condition", y="Bias", fill=" ")  +
  scale_fill_grey(start=0.8, end=0.2) +
  facet_grid(~study)

ggsave("plot_biasW_incl_before.pdf", width = 26.5, height = 11, units = 'cm')

# ------------------------------------------------------------------------------

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")

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
res_ff_biasW_old <-  matrix(NA, n_iterations, n_conditions)


for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    res_ff_biasW[j,i]     <- results_ff[[i]][[j]][[7]][1,1] - pop_ff[i]
    res_ff_biasW_old[j,i] <- aggregate(freq ~ X, data=results_ff[[i]][[j]][[9]][1:9,], sum)[1,2] /
      sum(aggregate(freq ~ X, data=results_ff[[i]][[j]][[9]][1:9,], sum)) - pop_ff[i]
    
  }
}



data_plot_biasW_ff_old = as.data.frame(matrix(NA, n_conditions*n_iterations, 3))
colnames(data_plot_biasW_ff_old) = c("moment","condition", "biasW")

conds = c("WX1, WY1, XZ1",
          "WX4, WY1, XZ1",
          "WX1, WY4, XZ1",
          "WX4, WY4, XZ1",
          "WX1, WY1, XZ4",
          "WX4, WY1, XZ4",
          "WX1, WY4, XZ4",
          "WX4, WY4, XZ4")

data_plot_biasW_ff_old$moment = rep("1.Without procedure", nrow(data_plot_biasW_ff_old))
data_plot_biasW_ff_old$condition = rep(conds, each = n_iterations)
data_plot_biasW_ff_old$biasW  = c(res_ff_biasW_old[,1:8])


data_plot_biasW_ff = as.data.frame(matrix(NA, n_conditions*n_iterations, 3))
colnames(data_plot_biasW_ff) = c("moment","condition", "biasW")

conds = c("WX1, WY1, XZ1",
          "WX4, WY1, XZ1",
          "WX1, WY4, XZ1",
          "WX4, WY4, XZ1",
          "WX1, WY1, XZ4",
          "WX4, WY1, XZ4",
          "WX1, WY4, XZ4",
          "WX4, WY4, XZ4")

data_plot_biasW_ff$moment = rep("2. After procedure", nrow(data_plot_biasW_ff))
data_plot_biasW_ff$condition = rep(conds, each = n_iterations)
data_plot_biasW_ff$biasW  = c(res_ff_biasW[,1:8])

data_plot_biasW_ff_total <- rbind(data_plot_biasW_ff, data_plot_biasW_ff_old)

plot_biasW_ff = ggplot(data_plot_biasW_ff_total, aes(x = factor(condition), y = biasW, fill=factor(moment))) +
  geom_boxplot() +
  theme_classic() + 
  labs(x="Condition", y="Bias", fill=" ")  +
  scale_fill_grey(start=0.8, end=0.2) 

ggsave("plot_biasW_ff_incl_before.pdf", width = 26.5, height = 11, units = 'cm')

