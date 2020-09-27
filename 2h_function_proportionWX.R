results_WX <- function(data, tab_extra, W_probs){
  
  library(plyr)
  
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
  
  #WY_new       <- ddply(WYX_new, .(Y,W), summarize, freq=sum(freq))
  
  XW_probs <- matrix(NA, 9, 2)
  rownames(XW_probs) <- c("X1W1","X2W1","X3W1","X1W2","X2W2","X3W2","X1W3","X2W3","X3W3")
  colnames(XW_probs) <- c("prop","var")
  
  for (k in 1:3){
    
    # proportions
    XW_probs[(1+(k-1)*3),1] <- sum(WYX_new[k,   "freq"]/sum(WYX_new[WYX_new$W==k,"freq"]),
                                   WYX_new[k+9, "freq"]/sum(WYX_new[WYX_new$W==k,"freq"]),
                                   WYX_new[k+18,"freq"]/sum(WYX_new[WYX_new$W==k,"freq"]))
    
    XW_probs[(2+(k-1)*3),1] <- sum(WYX_new[k+3, "freq"]/sum(WYX_new[WYX_new$W==k,"freq"]),
                                   WYX_new[k+12,"freq"]/sum(WYX_new[WYX_new$W==k,"freq"]),
                                   WYX_new[k+21,"freq"]/sum(WYX_new[WYX_new$W==k,"freq"]))
    
    XW_probs[(3+(k-1)*3),1] <- sum(WYX_new[k+6, "freq"]/sum(WYX_new[WYX_new$W==k,"freq"]),
                                   WYX_new[k+15,"freq"]/sum(WYX_new[WYX_new$W==k,"freq"]),
                                   WYX_new[k+24,"freq"]/sum(WYX_new[WYX_new$W==k,"freq"]))
    
    
    XW_probs[(1+(k-1)*3),2] <-  (1/(W_probs[k,1]^2)) *                            # 1 / (phatW^2) * 
      sum(c((sum(data[data$Y==1,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *       # P(Y1)^2 / N check
           (WYX_new[k,"freq"] / sum(WYX_new[c(1:9),"freq"]) *                     # P(W1X1|Y1)
              (1 - (WYX_new[k,"freq"] / sum(WYX_new[c(1:9),"freq"]))) +           # 1 - P(W1X1|Y1)
              XW_probs[(1+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
              (sum(WYX_new[c(k,k+3,k+6),"freq"]) / sum(WYX_new[c(1:9),"freq"]) *  # P(W1|Y1)
              (1 - (sum(WYX_new[c(k,k+3,k+6),"freq"]) / sum(WYX_new[c(1:9),"freq"])))) - # 1 - P(W1|Y1) -
              2 * XW_probs[(1+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
              (WYX_new[k,"freq"] / sum(WYX_new[c(1:9),"freq"])) *                 # P(X1W1|Y1) *
              (1 - (WYX_new[k,"freq"] / sum(WYX_new[c(1:9),"freq"])))),           # 1 - P(X1W1|Y1)
          (sum(data[data$Y==2,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *         # P(Y2)^2 / N check
            (WYX_new[k+9,"freq"] / sum(WYX_new[c(10:18),"freq"]) *                 # P(W1X1|Y2)
              (1 - (WYX_new[k+9,"freq"] / sum(WYX_new[c(10:18),"freq"]))) +        # 1 - P(W1X1|Y2)
              XW_probs[(1+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
              (sum(WYX_new[c(k+9,k+12,k+15),"freq"]) / sum(WYX_new[c(10:18),"freq"]) * # P(W1|Y2)
              (1 - (sum(WYX_new[c(k+9,k+12,k+15),"freq"]) / sum(WYX_new[c(10:18),"freq"])))) - # 1 - P(W1|Y2) -
              2 * XW_probs[(1+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
              (WYX_new[k+9,"freq"] / sum(WYX_new[c(10:18),"freq"])) *              # P(X1W1|Y2) *
              (1 - (WYX_new[k+9,"freq"] / sum(WYX_new[c(10:18),"freq"])))),        # 1 - P(X1W1|Y2)
          (sum(data[data$Y==3,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *         # P(Y3)^2 / N check
            (WYX_new[k+18,"freq"] / sum(WYX_new[c(19:27),"freq"]) *                 # P(W1X1|Y3)
              (1 - (WYX_new[k+18,"freq"] / sum(WYX_new[c(19:27),"freq"]))) +        # 1 - P(W1X1|Y3)
              XW_probs[(1+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
              (sum(WYX_new[c(k+18,k+21,k+24),"freq"]) / sum(WYX_new[c(19:27),"freq"]) * # P(W1|Y3)
              (1 - (sum(WYX_new[c(k+18,k+21,k+24),"freq"]) / sum(WYX_new[c(19:27),"freq"])))) - # 1 - P(W1|Y3) -
              2 * XW_probs[(1+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
              (WYX_new[k+18,"freq"] / sum(WYX_new[c(19:27),"freq"])) *              # P(X1W1|Y3) *
              (1 - (WYX_new[k+18,"freq"] / sum(WYX_new[c(19:27),"freq"]))))))       # 1 - P(X1W1|Y3)
    
    XW_probs[(2+(k-1)*3),2] <-  (1/(W_probs[k,1]^2)) *                            # 1 / (phatW^2) * 
      sum(c((sum(data[data$Y==1,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *       # P(Y1)^2 / N check
              (WYX_new[k+3,"freq"] / sum(WYX_new[c(1:9),"freq"]) *                     # P(W1X1|Y1)
                 (1 - (WYX_new[k+3,"freq"] / sum(WYX_new[c(1:9),"freq"]))) +           # 1 - P(W1X1|Y1)
                 XW_probs[(2+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
                 (sum(WYX_new[c(k,k+3,k+6),"freq"]) / sum(WYX_new[c(1:9),"freq"]) *  # P(W1|Y1)
                    (1 - (sum(WYX_new[c(k,k+3,k+6),"freq"]) / sum(WYX_new[c(1:9),"freq"])))) - # 1 - P(W1|Y1) -
                 2 * XW_probs[(2+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
                 (WYX_new[k+3,"freq"] / sum(WYX_new[c(1:9),"freq"])) *                 # P(X1W1|Y1) *
                 (1 - (WYX_new[k+3,"freq"] / sum(WYX_new[c(1:9),"freq"])))),           # 1 - P(X1W1|Y1)
            (sum(data[data$Y==2,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *         # P(Y2)^2 / N check
              (WYX_new[k+12,"freq"] / sum(WYX_new[c(10:18),"freq"]) *                 # P(W1X1|Y2)
                 (1 - (WYX_new[k+12,"freq"] / sum(WYX_new[c(10:18),"freq"]))) +        # 1 - P(W1X1|Y2)
                 XW_probs[(2+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
                 (sum(WYX_new[c(k+9,k+12,k+15),"freq"]) / sum(WYX_new[c(10:18),"freq"]) * # P(W1|Y2)
                    (1 - (sum(WYX_new[c(k+9,k+12,k+15),"freq"]) / sum(WYX_new[c(10:18),"freq"])))) - # 1 - P(W1|Y2) -
                 2 * XW_probs[(2+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
                 (WYX_new[k+12,"freq"] / sum(WYX_new[c(10:18),"freq"])) *              # P(X1W1|Y2) *
                 (1 - (WYX_new[k+12,"freq"] / sum(WYX_new[c(10:18),"freq"])))),        # 1 - P(X1W1|Y2)
            (sum(data[data$Y==3,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *         # P(Y3)^2 / N check
              (WYX_new[k+21,"freq"] / sum(WYX_new[c(19:27),"freq"]) *                 # P(W1X1|Y3)
                 (1 - (WYX_new[k+21,"freq"] / sum(WYX_new[c(19:27),"freq"]))) +        # 1 - P(W1X1|Y3)
                 XW_probs[(2+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
                 (sum(WYX_new[c(k+18,k+21,k+24),"freq"]) / sum(WYX_new[c(19:27),"freq"]) * # P(W1|Y3)
                    (1 - (sum(WYX_new[c(k+18,k+21,k+24),"freq"]) / sum(WYX_new[c(19:27),"freq"])))) - # 1 - P(W1|Y3) -
                 2 * XW_probs[(2+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
                 (WYX_new[k+21,"freq"] / sum(WYX_new[c(19:27),"freq"])) *              # P(X1W1|Y3) *
                 (1 - (WYX_new[k+21,"freq"] / sum(WYX_new[c(19:27),"freq"]))))))       # 1 - P(X1W1|Y3)
    
    XW_probs[(3+(k-1)*3),2] <-  (1/(W_probs[k,1]^2)) *                            # 1 / (phatW^2) * 
      sum(c((sum(data[data$Y==1,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *       # P(Y1)^2 / N check
              (WYX_new[k+6,"freq"] / sum(WYX_new[c(1:9),"freq"]) *                     # P(W1X1|Y1)
                 (1 - (WYX_new[k+6,"freq"] / sum(WYX_new[c(1:9),"freq"]))) +           # 1 - P(W1X1|Y1)
                 XW_probs[(3+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
                 (sum(WYX_new[c(k,k+3,k+6),"freq"]) / sum(WYX_new[c(1:9),"freq"]) *  # P(W1|Y1)
                    (1 - (sum(WYX_new[c(k,k+3,k+6),"freq"]) / sum(WYX_new[c(1:9),"freq"])))) - # 1 - P(W1|Y1) -
                 2 * XW_probs[(3+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
                 (WYX_new[k+6,"freq"] / sum(WYX_new[c(1:9),"freq"])) *                 # P(X1W1|Y1) *
                 (1 - (WYX_new[k+6,"freq"] / sum(WYX_new[c(1:9),"freq"])))),           # 1 - P(X1W1|Y1)
            (sum(data[data$Y==2,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *         # P(Y2)^2 / N check
              (WYX_new[k+15,"freq"] / sum(WYX_new[c(10:18),"freq"]) *                 # P(W1X1|Y2)
                 (1 - (WYX_new[k+15,"freq"] / sum(WYX_new[c(10:18),"freq"]))) +        # 1 - P(W1X1|Y2)
                 XW_probs[(3+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
                 (sum(WYX_new[c(k+9,k+12,k+15),"freq"]) / sum(WYX_new[c(10:18),"freq"]) * # P(W1|Y2)
                    (1 - (sum(WYX_new[c(k+9,k+12,k+15),"freq"]) / sum(WYX_new[c(10:18),"freq"])))) - # 1 - P(W1|Y2) -
                 2 * XW_probs[(3+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
                 (WYX_new[k+15,"freq"] / sum(WYX_new[c(10:18),"freq"])) *              # P(X1W1|Y2) *
                 (1 - (WYX_new[k+15,"freq"] / sum(WYX_new[c(10:18),"freq"])))),        # 1 - P(X1W1|Y2)
            (sum(data[data$Y==3,],5)/sum(data[,5])^2 / sum(WYX_new$freq)) *         # P(Y3)^2 / N check
              (WYX_new[k+24,"freq"] / sum(WYX_new[c(19:27),"freq"]) *                 # P(W1X1|Y3)
                 (1 - (WYX_new[k+24,"freq"] / sum(WYX_new[c(19:27),"freq"]))) +        # 1 - P(W1X1|Y3)
                 XW_probs[(3+(k-1)*3),"prop"]^2 *                                              # P(X1|W1)^2
                 (sum(WYX_new[c(k+18,k+21,k+24),"freq"]) / sum(WYX_new[c(19:27),"freq"]) * # P(W1|Y3)
                    (1 - (sum(WYX_new[c(k+18,k+21,k+24),"freq"]) / sum(WYX_new[c(19:27),"freq"])))) - # 1 - P(W1|Y3) -
                 2 * XW_probs[(3+(k-1)*3),"prop"] *                                            # 2 * P(X1|W1) * 
                 (WYX_new[k+24,"freq"] / sum(WYX_new[c(19:27),"freq"])) *              # P(X1W1|Y3) *
                 (1 - (WYX_new[k+24,"freq"] / sum(WYX_new[c(19:27),"freq"]))))))       # 1 - P(X1W1|Y3)
          
    
  }
  
  return(XW_probs)
}
