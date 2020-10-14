
generate_W_for_audit <- function(data, tab_extra) {
  
  # get results for W 
  XY_extra <- cbind(tab_extra[tab_extra$Z == 1, c("X","Y")], 
                    tab_extra[tab_extra$Z == 1, "freqplus"] - tab_extra[tab_extra$Z == 1, "freq"])
  colnames(XY_extra) <- c("X","Y","extra")
  
  WYX_new <- matrix(NA, nrow(XY_extra)*3, 4)
  colnames(WYX_new) <- c("X","Y","W","freq")
  
  
  for(k in 1:nrow(XY_extra)){
    
    X_select = paste0(XY_extra[k, "X"])
    Y_select = paste0(XY_extra[k, "Y"])
    N_select = XY_extra[k, "extra"]
    
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "X"] <- rep(X_select, 3)
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "Y"] <- rep(Y_select, 3)
    WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "W"] <- c(1,2,3)
    
    if (N_select > 0) {
      WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "freq"] <- data[data$X == X_select & data$Y == Y_select & data$Z == 1, 5] +
        rmultinom(1, 
                  N_select, # number of extra cases in audit
                  data[data$X == X_select & data$Y == Y_select & data$Z == 2, 5]/ # W available in Z=2
                    sum(data[data$X == X_select & data$Y == Y_select & data$Z == 2, 5]))
    } else if (N_select < 0) {
      WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "freq"] <- data[data$X == X_select & data$Y == Y_select & data$Z == 1, 5] -
        rmultinom(1, 
                  abs(N_select), # number of cases to remove from audit
                  data[data$X == X_select & data$Y == Y_select & data$Z == 1, 5]/ # W available in Z=1
                    sum(data[data$X == X_select & data$Y == Y_select & data$Z == 1, 5]))
    } else { # N_select == 0
      WYX_new[(1+((k-1)*3)):(3+((k-1)*3)), "freq"] <- data[data$X == X_select & data$Y == Y_select & data$Z == 1, 5]
    }
    
  }
  
  WYX_new      <- as.data.frame(WYX_new)
  WYX_new$freq <- as.numeric(as.character(WYX_new$freq))
  
  return(WYX_new)
  
}

