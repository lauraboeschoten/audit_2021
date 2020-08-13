## Functie om gradiknt van G2 uit te rekenen binnen constrOptim (type 1)
berekenG2.grad.type1 <- function(theta0, startaantallen, beschikbaar, rijtotalen, tab, cst) {
  
  idxvast             <- which(beschikbaar == 0)
  aantallen           <- startaantallen
  aantallen[-idxvast] <- theta0
  aantallen1          <- c(rijtotalen - aantallen, aantallen)
  tab$Freq            <- pmax(0, round(aantallen1, digits = 8))
  ktot                <- aggregate(tab$Freq, by = tab[ , c('Y','Z')], FUN = sum)
  idx                 <- which(aantallen != 0)
  temp1               <- rep(ktot$x[ktot$Z == 'TRUE'], each = length(unique(tab$X)))
  temp2               <- rep(ktot$x[ktot$Z != 'TRUE'], each = length(unique(tab$X)))
  grad                <- 2 * (log(aantallen) - log(rijtotalen - aantallen)) -
                         2 * (log(temp1)     - log(temp2))
  
  return(grad[-idxvast])
}


## Functie om gradiknt van G2 uit te rekenen binnen constrOptim (type 2)
berekenG2.grad.type2 <- function(theta0, startaantallen, beschikbaar, rijtotalen, tab, cst) {
  
  K                   <- length(startaantallen)
  idxvast             <- which(beschikbaar == 0)
  idx0                <- which(startaantallen == 0)
  aantallen           <- startaantallen
  
  if (length(idxvast) > 0) {
    aantallen[-idxvast] <- aantallen[-idxvast] + theta0[1:(K - length(idxvast))]
  } else {
    aantallen           <- aantallen + theta0[1:K]
  }
  if (length(idx0) > 0) {
    aantallen[-idx0]    <- aantallen[-idx0] - theta0[-(1:(K - length(idxvast)))]
  } else {
    aantallen           <- aantallen - theta0[-(1:K)]
  }
  
  aantallen1          <- c(rijtotalen - aantallen, aantallen)
  tab$Freq            <- pmax(0, round(aantallen1, digits = 8))
  ktot                <- aggregate(tab$Freq, by = tab[ , c('Y','Z')], FUN = sum)
  temp1               <- rep(ktot$x[ktot$Z == 2], each = length(unique(tab$X)))
  temp2               <- rep(ktot$x[ktot$Z != 2], each = length(unique(tab$X)))
  grad                <- 2 * (log(aantallen) - log(rijtotalen - aantallen)) -
                         2 * (log(temp1) - log(temp2))
  
  #if (idxvast > 0) {
    
  #  return(c(grad[-idxvast],-grad[-idx0]))
    
  #} else {
    
    return(c(grad,-grad))
    
  #}
  
  
  #return(c(grad[-idxvast],-grad[-idx0]))
}
