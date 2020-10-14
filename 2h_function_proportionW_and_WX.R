
results_W_and_WX <- function(WYX_new, tab_extra){
  
  ## 1) Estimate distribution of W
  
  # conditional distribution of W given Y in sample
  tab_WY_new       <- aggregate(freq ~ Y + W, data = WYX_new, sum)
  tab_Y_new        <- aggregate(freq ~ Y, data = WYX_new, sum)
  tab_WY_new       <- merge(tab_WY_new, tab_Y_new, by = 'Y', all = TRUE,
                            suffixes = c('.wy','.y'))
  tab_WY_new$p.wcy <- tab_WY_new$freq.wy / tab_WY_new$freq.y
  
  # population distribution of Y
  Y_pop            <- aggregate(freq ~ Y, data = tab_extra, sum)
  Y_pop$P.y        <- Y_pop$freq/sum(tab_extra$freq)
  
  tab_WY_new       <- merge(tab_WY_new, Y_pop[, c('Y','P.y')], by = 'Y', all = TRUE)
  
  tab_WY_new$term.prop <- tab_WY_new$P.y * tab_WY_new$p.wcy
  tab_WY_new$term.var  <- (tab_WY_new$P.y^2 / tab_WY_new$freq.y) * tab_WY_new$p.wcy * (1 - tab_WY_new$p.wcy)
  
  W_probs           <- matrix(NA, length(unique(WYX_new$W)), 2)
  colnames(W_probs) <- c("prop", "var")
  rownames(W_probs) <- 1:nrow(W_probs)
  
  for (w in 1:nrow(W_probs)){
    
    # proportions
    W_probs[w,1] <- sum(tab_WY_new$term.prop[tab_WY_new$W == w])
    
    # variances
    W_probs[w,2] <-  sum(tab_WY_new$term.var[tab_WY_new$W == w])
    
  }
  
  
  ## 2) Estimate distribution of X given W
  
  tab_WXY_new        <- aggregate(freq ~ X + Y + W, data = WYX_new, sum)
  tab_WXY_new        <- merge(tab_WXY_new, tab_WY_new[ , !(names(tab_WY_new) %in% c('term.prop','term.var'))],
                              by = c('W','Y'), all = TRUE)
  tab_WXY_new$p.wxcy <- tab_WXY_new$freq / tab_WXY_new$freq.y # conditional distribution of (W,X) given Y in sample
  
  tab_WXY_new$term.prop <- tab_WXY_new$P.y * tab_WXY_new$p.wxcy
  
  tab_WXY_new$term1.var <- (tab_WXY_new$P.y^2 / tab_WXY_new$freq.y) * tab_WXY_new$p.wxcy * (1 - tab_WXY_new$p.wxcy)
  tab_WXY_new$term2.var <- (tab_WXY_new$P.y^2 / tab_WXY_new$freq.y) * tab_WXY_new$p.wcy * (1 - tab_WXY_new$p.wcy)
  tab_WXY_new$term3.var <- (tab_WXY_new$P.y^2 / tab_WXY_new$freq.y) * tab_WXY_new$p.wxcy * (1 - tab_WXY_new$p.wcy)
  
  XW_probs <- matrix(NA, 9, 2)
  rownames(XW_probs) <- c("X1W1","X2W1","X3W1","X1W2","X2W2","X3W2","X1W3","X2W3","X3W3")
  colnames(XW_probs) <- c("prop","var")
  
  for (k in 1:nrow(XW_probs)){
    
    x <- as.integer(substr(rownames(XW_probs)[k],2,2))
    w <- as.integer(substr(rownames(XW_probs)[k],4,4))
    
    # proportions
    XW_probs[k,1] <- sum(tab_WXY_new$term.prop[tab_WXY_new$X == x & tab_WXY_new$W == w]) / W_probs[w,1]
    
    # variances
    XW_probs[k,2] <- ( sum(tab_WXY_new$term1.var[tab_WXY_new$X == x & tab_WXY_new$W == w]) +
      (XW_probs[k,1])^2 * sum(tab_WXY_new$term2.var[tab_WXY_new$X == x & tab_WXY_new$W == w]) +
      (-2 * XW_probs[k,1]) * sum(tab_WXY_new$term3.var[tab_WXY_new$X == x & tab_WXY_new$W == w]) ) / (W_probs[w,1])^2
    
  }
  
  return(list(W_probs, XW_probs))
}
