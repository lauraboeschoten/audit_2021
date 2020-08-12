## Functie om G2 uit te rekenen binnen constrOptim (type 1)
berekenG2.type1  <- function(theta0, startaantallen, beschikbaar, rijtotalen, tab, cst) {
  
  idxvast             <- which(beschikbaar == 0)
  aantallen           <- startaantallen
  aantallen[-idxvast] <- theta0
  aantallen           <- c(rijtotalen - aantallen, aantallen)
  tab$Freq            <- pmax(0, round(aantallen, digits = 8))
  ktot                <- aggregate(tab$Freq, by = tab[ , c('Y','Z')], FUN = sum)
  idx                 <- which(tab$Freq != 0)
  G2                  <- cst + 2 * sum(tab$Freq[idx] * log(tab$Freq[idx])) - 
                               2 * sum(ktot$x * log(ktot$x))
  
  return(G2)
}

## Functie om G2 uit te rekenen binnen constrOptim (type 2)
berekenG2.type2 <- function(theta0, startaantallen, beschikbaar, rijtotalen, tab, cst) {
  
  K                   <- length(startaantallen)
  idxvast             <- which(beschikbaar == 0)
  idx0                <- which(startaantallen == 0)
  aantallen           <- startaantallen
  aantallen[-idxvast] <- aantallen[-idxvast] + theta0[1:(K - length(idxvast))]
  aantallen[-idx0]    <- aantallen[-idx0] - theta0[-(1:(K - length(idxvast)))]
  aantallen           <- c(rijtotalen - aantallen, aantallen)
  tab$Freq            <- pmax(0, round(aantallen, digits = 8))
  ktot                <- aggregate(tab$Freq, by = tab[ , c('Y','Z')], FUN = sum)
  idx                 <- which(tab$Freq != 0)
  G2                  <- cst + 2 * sum(tab$Freq[idx] * log(tab$Freq[idx])) -
                               2 * sum(ktot$x * log(ktot$x))
  return(G2)
}
