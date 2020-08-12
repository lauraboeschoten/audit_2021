# Function to prepare required tables

preparetables <- function(data) {
  
  # prepare tab xyz
  tab_xyz = aggregate(data$freq, by = list(X = data$X, Y = data$Y, Z = data$Z), 
                      FUN  = sum, drop = TRUE)
  colnames(tab_xyz) = c("X","Y","Z","freq")
  
  # Determine number of cases available to be included in audit 
  tab_xyz$Z     = as.factor(tab_xyz$Z)
  tab_available = tab_xyz[which(tab_xyz$Z=='2'),]
  
  # prepare tab xy (RTOT)
  tab_xy = aggregate(data$freq, by = list(X = data$X, Y = data$Y), 
                      FUN  = sum, drop = TRUE)
  colnames(tab_xy) = c("X","Y","rtot")
  
  # prepare tab yz (KTOT)
  tab_yz = aggregate(data$freq, by = list(Y = data$Y, Z = data$Z), 
                      FUN  = sum, drop = TRUE)
  colnames(tab_yz) = c("Y","Z","ktot")
  
  # prepare tab y (TOT)
  tab_y = aggregate(data$freq, by = list(Y = data$Y), 
                     FUN  = sum, drop = TRUE)
  colnames(tab_y) = c("Y","tot")
  
  # merge tabs
  tab <- merge(x = tab_xyz, y = tab_xy, by = c('X', 'Y'), all.x = TRUE)
  tab <- merge(x = tab,     y = tab_yz, by = c('Y', 'Z'), all.x = TRUE)
  tab <- merge(x = tab,     y = tab_y,  by = 'Y',         all.x = TRUE)
  tab <- tab[order(tab$X, tab$Y, tab$Z), ]
  
  tab$Freq.pred <- tab$rtot * (tab$ktot / tab$tot)
  
  # include number available for Z
  tab <- merge(x = tab, y = tab_available[ , c('X', 'Y', 'freq')],
               by = c('X', 'Y'),
               all.x = TRUE)
  tab <- tab[order(tab$Z, tab$Y, tab$X), ]
  
  colnames(tab) <- c("X","Y","Z","freq","rtot","ktot","tot","freq.pred","freqZ")
  
  return(tab)
}