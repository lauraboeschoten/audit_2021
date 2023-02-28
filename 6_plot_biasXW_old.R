library(tidyverse)
library(plyr)

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")
setwd("//cbsp.nl/Productie/Secundair/MPOnderzoek/Werk/Combineren/Projecten/MeetfoutenEnergiebestanden/Artikel/audit_2021-master_nieuw")

load("distributions_WX.RData")
dist_WX = generated_distributions
load("datasets_WX.RData")
data_WX = generated_datasets

load("distributions_WY.RData")
dist_WY = generated_distributions
load("datasets_WY.RData")
data_WY = generated_datasets

load("distributions_XZ.RData")
dist_XZ = generated_distributions
load("datasets_XZ.RData")
data_XZ = generated_datasets


results_WX <- readRDS("results_WX.rds")
results_WY <- readRDS("results_WY.rds")
results_XZ <- readRDS("results_XZ.rds")


n_conditions  = 4 #4
n_iterations  = 1000 #1000
n_results     = 9


pop_WX  <- c(aggregate(prob ~ X + W, data = dist_WX[[1]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WX[[1]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_WX[[2]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WX[[2]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_WX[[3]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WX[[3]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_WX[[4]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WX[[4]], sum)[1:3,3]))
pop_WY  <- c(aggregate(prob ~ X + W, data = dist_WY[[1]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WY[[1]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_WY[[2]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WY[[2]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_WY[[3]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WY[[3]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_WY[[4]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_WY[[4]], sum)[1:3,3]))
pop_XZ  <- c(aggregate(prob ~ X + W, data = dist_XZ[[1]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_XZ[[1]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_XZ[[2]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_XZ[[2]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_XZ[[3]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_XZ[[3]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_XZ[[4]], sum)[1,3] / sum(aggregate(prob ~ X + W, data = dist_XZ[[4]], sum)[1:3,3]))


res_WX_biasXW <- matrix(NA, 1000, 4)
res_WY_biasXW <- matrix(NA, 1000, 4)
res_XZ_biasXW <- matrix(NA, 1000, 4)

res_WX_biasXW_old <- matrix(NA, 1000, 4)
res_WY_biasXW_old <- matrix(NA, 1000, 4)
res_XZ_biasXW_old <- matrix(NA, 1000, 4)

for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    res_WX_biasXW[j,i] <- results_WX[[i]][[j]][[8]][1,1] - pop_WX[i]
    res_WY_biasXW[j,i] <- results_WY[[i]][[j]][[8]][1,1] - pop_WY[i]
    res_XZ_biasXW[j,i] <- results_XZ[[i]][[j]][[8]][1,1] - pop_XZ[i]
  }
}

##

for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    datWX = data_WX[[i]][,c(1:4,j+4)]
    colnames(datWX) <- c("X", "Y", "W",  "Z", "freq")
    
    datWY = data_WY[[i]][,c(1:4,j+4)]
    colnames(datWY) <- c("X", "Y", "W",  "Z", "freq")
    
    datXZ = data_XZ[[i]][,c(1:4,j+4)]
    colnames(datXZ) <- c("X", "Y", "W",  "Z", "freq")
    
    res_WX_biasXW_old[j,i]  <- aggregate(freq ~ X + W + Z , data = datWX, sum)[1,4] /sum(aggregate(freq ~ X + W + Z, data = datWX, sum)[1:3,4]) - pop_WX[i]
    res_WY_biasXW_old[j,i]  <- aggregate(freq ~ X + W + Z , data = datWY, sum)[1,4] /sum(aggregate(freq ~ X + W + Z, data = datWY, sum)[1:3,4]) - pop_WY[i]
    res_XZ_biasXW_old[j,i]  <- aggregate(freq ~ X + W + Z , data = datXZ, sum)[1,4] /sum(aggregate(freq ~ X + W + Z, data = datXZ, sum)[1:3,4]) - pop_XZ[i]
  }
}



data_plot_biasXW_old = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 4))
colnames(data_plot_biasXW_old) = c("moment","study","condition", "biasXW")

data_plot_biasXW_old$moment    = rep("1.Without procedure", nrow(data_plot_biasXW_old))
data_plot_biasXW_old$study     = rep(c("condition WX", "condition WY", "condition XZ"), each = n_conditions*n_iterations)
data_plot_biasXW_old$condition = rep(rep(1:4, each = n_iterations), 3)
data_plot_biasXW_old$biasXW  = c(res_WX_biasXW_old[,1:4], 
                               res_WY_biasXW_old[,1:4],
                               res_XZ_biasXW_old[,1:4])

data_plot_biasXW = as.data.frame(matrix(NA, n_conditions*n_iterations*3, 4))
colnames(data_plot_biasXW) = c("moment","study","condition", "biasXW")

data_plot_biasXW$moment    = rep("2. With procedure", nrow(data_plot_biasXW))
data_plot_biasXW$study     = rep(c("condition WX", "condition WY", "condition XZ"), each = n_conditions*n_iterations)
data_plot_biasXW$condition = rep(rep(1:4, each = n_iterations), 3)
data_plot_biasXW$biasXW  = c(res_WX_biasXW[,1:4], 
                           res_WY_biasXW[,1:4],
                           res_XZ_biasXW[,1:4])

data_plot_biasXW_total = rbind(data_plot_biasXW_old, data_plot_biasXW)


plot_biasXW = ggplot(data_plot_biasXW_total, aes(x = factor(condition), y = biasXW, fill=factor(moment))) +
  geom_boxplot() +
  theme_classic() + 
  labs(x="Condition", y="Bias", fill=" ")  +
  scale_fill_grey(start=0.8, end=0.2) +
  facet_grid(~study)

ggsave("plot_biasXW_incl_before.pdf", width = 26.5, height = 11, units = 'cm')

# ------------------------------------------------------------------------------

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie_v1")

load("simresults_ff.RData")
dist_ff = generated_distributions
load("datasets_ff.RData")
data_ff = generated_datasets


results_ff <- readRDS("results_ff.rds")

n_conditions  = 8 #4
n_iterations  = 1000 #1000
n_results     = 9

pop_ff  <- c(aggregate(prob ~ X + W, data = dist_ff[[1]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[1]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[2]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[2]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[3]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[3]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[4]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[4]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[5]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[5]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[6]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[6]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[7]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[7]], sum)[1:3,3]),
             aggregate(prob ~ X + W, data = dist_ff[[8]], sum)[1,3]/ sum(aggregate(prob ~ X + W, data = dist_ff[[8]], sum)[1:3,3]))


res_ff_biasXW <- matrix(NA, n_iterations, n_conditions)
res_ff_biasXW_old <-  matrix(NA, n_iterations, n_conditions)


for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    res_ff_biasXW[j,i] <- abs(results_ff[[i]][[j]][[8]][1,1] - pop_ff[i])
    
  }
}

for(i in 1:n_conditions){
  for(j in 1:n_iterations){
    datff = data_ff[[i]][,c(1:4,j+4)]
    colnames(datff) <- c("X", "Y", "W",  "Z", "freq")
    
    res_ff_biasXW_old[j,i]  <- aggregate(freq ~ X + W + Z , data = datff, sum)[1,4] /sum(aggregate(freq ~ X + W + Z, data = datff, sum)[1:3,4]) - pop_ff[i]

  }
}

##

data_plot_biasXW_ff_old = as.data.frame(matrix(NA, n_conditions*n_iterations, 3))
colnames(data_plot_biasXW_ff_old) = c("moment","condition", "biasXW")

conds = c("WX1, WY1, XZ1",
          "WX4, WY1, XZ1",
          "WX1, WY4, XZ1",
          "WX4, WY4, XZ1",
          "WX1, WY1, XZ4",
          "WX4, WY1, XZ4",
          "WX1, WY4, XZ4",
          "WX4, WY4, XZ4")

data_plot_biasXW_ff_old$moment = rep("1.Without procedure", nrow(data_plot_biasXW_ff_old))
data_plot_biasXW_ff_old$condition = rep(conds, each = n_iterations)
data_plot_biasXW_ff_old$biasXW  = c(res_ff_biasXW_old[,1:8])


data_plot_biasXW_ff = as.data.frame(matrix(NA, n_conditions*n_iterations, 3))
colnames(data_plot_biasXW_ff) = c("moment","condition", "biasXW")

conds = c("WX1, WY1, XZ1",
          "WX4, WY1, XZ1",
          "WX1, WY4, XZ1",
          "WX4, WY4, XZ1",
          "WX1, WY1, XZ4",
          "WX4, WY1, XZ4",
          "WX1, WY4, XZ4",
          "WX4, WY4, XZ4")

data_plot_biasXW_ff$moment = rep("2. After procedure", nrow(data_plot_biasXW_ff))
data_plot_biasXW_ff$condition = rep(conds, each = n_iterations)
data_plot_biasXW_ff$biasXW  = c(res_ff_biasXW[,1:8])

data_plot_biasXW_ff_total <- rbind(data_plot_biasXW_ff, data_plot_biasXW_ff_old)

plot_biasXW_ff = ggplot(data_plot_biasXW_ff_total, aes(x = factor(condition), y = biasXW, fill=factor(moment))) +
  geom_boxplot() +
  theme_classic() + 
  labs(x="Condition", y="Bias", fill=" ")  +
  scale_fill_grey(start=0.8, end=0.2) 
  #theme(axis.text.x = element_text(angle=45)) 

ggsave("plot_biasXW_ff_incl_before.pdf", width = 26.5, height = 11, units = 'cm')

##


