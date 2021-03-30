## Functie om restricties voor minimalisatie te maken
prepareerRestricties <- function(extra, nweg, start, beschikbaar, type) {
  
  # type = 1: alleen (maximaal extra) eenheden toevoegen
  # type = 2: (maximaal extra) eenheden toevoegen en (maximaal extra) eenheden weglaten
  
  K <- length(beschikbaar)
  
  ## restricties (alleen ongelijkheden)
  
  if (type == 1) { # alleen eenheden toevoegen
    
    idxvast <- which(beschikbaar == 0)
    aantalvast <- length(idxvast)
    
    # ondergrens en bovengrens per cel (alleen als beschikbaar > 0)
    A1 <- diag(K)[-idxvast, -idxvast]
    b1 <- start[-idxvast]
    A2 <- -diag(K)[-idxvast, -idxvast]
    b2 <- -(start + beschikbaar)[-idxvast]
    # bovengrens voor aantal extra bekeken
    A3 <- matrix(-1, nrow = 1, ncol = K - aantalvast)
    b3 <- -(sum(start[-idxvast]) + extra)
    # samenvoegen
    A <- rbind(A1, A2, A3)
    b <- c(b1, b2, b3)
    
  } else if (type == 2) { # eenheden toevoegen en weglaten
    
    idxvast    <- which(beschikbaar == 0)
    aantalvast <- length(idxvast)
    idx0       <- which(start == 0)
    aantal0    <- length(idx0)
    
    # ondergrens en bovengrens toegevoegde waarden per cel (alleen als beschikbaar > 0)
    if (aantalvast > 0) {
      A1 <- cbind(diag(K)[-idxvast, -idxvast], matrix(0, nrow = K - aantalvast, ncol = K - aantal0))
      b1 <- rep(0, K - aantalvast)
      A2 <- cbind(-diag(K)[-idxvast, -idxvast], matrix(0, nrow = K - aantalvast, ncol = K - aantal0))
      b2 <- -beschikbaar[-idxvast]
    } else {
      A1 <- cbind(diag(K), matrix(0, nrow = K - aantalvast, ncol = K - aantal0))
      b1 <- rep(0, K - aantalvast)
      A2 <- cbind(-diag(K), matrix(0, nrow = K - aantalvast, ncol = K - aantal0))
      b2 <- -beschikbaar
    }
    
    # ondergrens en bovengrens weggelaten waarden per cel (alleen als start > 0)
    if (aantal0 > 0) {
      A3 <- cbind(matrix(0, nrow = K - aantal0, ncol = K - aantalvast), diag(K)[-idx0, -idx0])
      b3 <- rep(0, K - aantal0)
      A4 <- cbind(matrix(0, nrow = K - aantal0, ncol = K - aantalvast), -diag(K)[-idx0, -idx0])
      b4 <- -start[-idx0]
    } else {
      A3 <- cbind(matrix(0, nrow = K - aantal0, ncol = K - aantalvast), diag(K))
      b3 <- rep(0, K - aantal0)
      A4 <- cbind(matrix(0, nrow = K - aantal0, ncol = K - aantalvast), -diag(K))
      b4 <- -start
    }
    
    # bovengrens voor totaal aantal extra bekeken
    A5 <- cbind(matrix(-1, nrow = 1, ncol = K - aantalvast), matrix(0, nrow = 1, ncol = K - aantal0))
    b5 <- -extra
    # bovengrens voor totaal aantal weggelaten
    A6 <- cbind(matrix(0, nrow = 1, ncol = K - aantalvast), matrix(-1, nrow = 1, ncol = K - aantal0))
    b6 <- -(extra*nweg)
    # samenvoegen
    A <- rbind(A1, A2, A3, A4, A5, A6)
    b <- c(b1, b2, b3, b4, b5, b6)
    
  }
  
  return(list(A = A, b = b))
}
