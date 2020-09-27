results_W <- function(data, tab_extra){
  
  #library(plyr)
  library(dplyr)
  
  # get results for W 
  XY_extra <- cbind(tab_extra[tab_extra$Z == 1, c("X","Y")], 
                    tab_extra[tab_extra$Z == 2, "freqplus"] - tab_extra[tab_extra$Z == 1, "freq"])
  colnames(XY_extra) <- c("X","Y","extra")
  
  WYX_new <- matrix(NA, nrow(XY_extra)*3, 4)
  colnames(WYX_new) <- c("X","Y","W","freq")
  
  # als negatieve waarde, dan op 0 zetten? 
  XY_extra[,"extra"][XY_extra[,"extra"] < 0 ] <-0
  
  for(k in 1:nrow(XY_extra)){
    
    X_select = paste0(XY_extra[k, "X"])
    Y_select = paste0(XY_extra[k, "Y"])
    N_select = paste0(XY_extra[k, "extra"])
    
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "X"] <- rep(X_select, 3)
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "Y"] <- rep(Y_select, 3)
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "W"] <- c(1,2,3)
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "freq"] <- data[data$X == X_select & data$Y == Y_select & data$Z == 1, 5] +
      rmultinom(1, 
                N_select, # number of extra cases in audit
                data[data$X == X_select & data$Y == Y_select & data$Z == 2, 5]/ # W available in Z=2
                  sum(data[data$X == X_select & data$Y == Y_select & data$Z == 2, 5]))
    
  }
  
  WYX_new      <- as.data.frame(WYX_new)
  WYX_new$freq <- as.numeric(as.character(WYX_new$freq))
  
  WY_new       <- aggregate(freq ~ Y + W, data = WYX_new, sum)
  
  W_probs           <- matrix(NA, 3, 2 )
  colnames(W_probs) <- c("prop", "var")
  rownames(W_probs) <- c(1,2,3)
  
  for (k in 1:nrow(W_probs)){
    
    # proportions
    W_probs[k,1] <- sum(WY_new[k,   "freq"] / sum(WY_new$freq),
                        WY_new[k+3, "freq"] / sum(WY_new$freq),
                        WY_new[k+6, "freq"] / sum(WY_new$freq))
    
    # variances
    W_probs[k,2] <-  sum((sum(data[data$Y==1,],5) / sum(data[,5]))^2 / 
                           sum(WY_new$freq) * 
                           WY_new[k,"freq"]       / sum(WY_new[1:3,"freq"]) * 
                           (1 - WY_new[k,"freq"]  / sum(WY_new[1:3,"freq"])),
                         (sum(data[data$Y==2,],5) / sum(data[,5]))^2 / 
                           sum(WY_new$freq) * 
                           WY_new[k+3,"freq"]      / sum(WY_new[4:6,"freq"]) * 
                           (1 - WY_new[k+3,"freq"] / sum(WY_new[4:6,"freq"])),
                         (sum(data[data$Y==3,],5)  / sum(data[,5]))^2 / 
                           sum(WY_new$freq) * 
                           WY_new[k+6,"freq"]      / sum(WY_new[7:9,"freq"]) * 
                           (1 - WY_new[k+6,"freq"] / sum(WY_new[7:9,"freq"])))
    
  }
  
  return(W_probs)
}
