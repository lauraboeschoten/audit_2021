## Functie om willekeurige startoplossing voor minimalisatie te maken (type 1)
prepareerStartwaarden.type1 <- function(extra, start, beschikbaar, L = 0.9, U = 0.99) {
  
  ### Startoplossing bepalen
  # Lastig: startoplossing voor constrOptim moet voldoen aan restricties
  # en mag niet precies op een ondergrens of bovengrens liggen
  
  # Aanpak:
  # - trek R uit uniforme verdeling op (L, U) (aanname: U < 1)
  # - trek FR uit uniforme verdeling op (0.2, 1)
  # - definieer het aantal eenheden dat per keer wordt toegevoegd als FR * R * extra eenheden,
  #   mits dit niet groter dan 10 is (anders 10)
  # - trek per rij een willekeurig aantal (maximaal perkeer) eenheden om toe te voegen
  #   zonder dat hierdoor exact voldaan wordt aan een bovengrens
  # - ga net zo lang door totdat R * extra eenheden zijn toegevoegd
  
  bovengrens   <- start + beschikbaar
  idxvast      <- which(beschikbaar == 0)
  theta0       <- start[-idxvast]
  
  klaar        <- FALSE
  toewijzen    <- runif(n = 1, min = L, max = U) * extra
  FR           <- runif(n = 1, min = 0.2, max = 1)
  perkeer      <- min(10, ceiling(toewijzen / (FR * length(bovengrens[-idxvast]))))
  while (!klaar) {
    #print(toewijzen)
    rest       <- bovengrens[-idxvast] - theta0
    idx        <- which(rest >= 1)
    x          <- sample(idx, 1)
    erbij      <- min(runif(n = 1, min = 0, max = min(perkeer, rest[x])), toewijzen)
    theta0[x]  <- theta0[x] + erbij
    toewijzen  <- toewijzen - erbij
    klaar      <- (length(idx) == 0) | (toewijzen <= 0)
  }
  
  # cellen die nog op hun oorspronkelijke startwaarden liggen iets ophogen (met veel minder dan 1)
  idx0         <- which(theta0 == start[-idxvast])
  theta0[idx0] <- theta0[idx0] + 0.01
  return(theta0)
}

## Functie om willekeurige startoplossing voor minimalisatie te maken (type 2)
prepareerStartwaarden.type2 <- function(extra, nweg, start, beschikbaar,
                                        Lextra = 0.9, Uextra = 0.99, Lweg = 0.10, Uweg = 0.99) {
  
  ### Startoplossing bepalen
  # Lastig: startoplossing voor constrOptim moet voldoen aan restricties
  # en mag niet precies op een ondergrens of bovengrens liggen
  
  # Aanpak:
  # - trek R uit uniforme verdeling op (L, U) (aanname: U < 1)
  # - trek FR uit uniforme verdeling op (0.2, 1)
  # - definieer het aantal eenheden dat per keer wordt toegevoegd als FR * R * extra eenheden,
  #   mits dit niet groter dan 10 is (anders 10)
  # - trek per rij een willekeurig aantal (maximaal perkeer) eenheden om toe te voegen
  #   zonder dat hierdoor exact voldaan wordt aan een bovengrens
  # - ga net zo lang door totdat R * extra eenheden zijn toegevoegd
  
  bovengrens  <- beschikbaar
  K           <- length(bovengrens)
  idxvast     <- which(beschikbaar == 0)     # integer(0)
  aantalvast  <- length(idxvast)             # 0 
  idx0        <- which(start == 0)           # integer(0)
  aantal0     <- length(idx0)                # 0 
  theta0      <- rep(0, 2*K - aantalvast - aantal0)
  
  klaar       <- FALSE
  toewijzen   <- runif(n = 1, min = Lextra, max = Uextra) * extra
  FR          <- runif(n = 1, min = 0.2, max = 1)
  perkeer     <- min(10, ceiling(toewijzen / (FR * (K - aantalvast))))
  
  while (!klaar) {
    
    #print(toewijzen)
    if (aantalvast > 0) {
      
      rest      <- bovengrens[-idxvast] - theta0[1:(K - aantalvast)]
      idx       <- which(rest >= 1)
      x         <- sample(idx, 1)
      erbij     <- min(runif(n = 1, min = 0, max = min(perkeer, rest[x])), toewijzen)
      theta0[x] <- theta0[x] + erbij
      toewijzen <- toewijzen - erbij
      klaar     <- (length(idx) == 0) | (toewijzen <= 0)
      
    } else {
      
      rest      <- bovengrens
      idx       <- which(rest >= 1)
      x         <- sample(idx, 1)
      erbij     <- min(runif(n = 1, min = 0, max = min(perkeer, rest[x])), toewijzen)
      theta0[x] <- theta0[x] + erbij
      toewijzen <- toewijzen - erbij
      klaar     <- (length(idx) == 0) | (toewijzen <= 0)
      
    }
    
  }
  
  klaar       <- FALSE
  toewijzen   <- runif(n = 1, min = Lweg, max = Uweg) * extra * nweg
  FR          <- runif(n = 1, min = 0.2, max = 1)
  perkeer     <- min(10, ceiling(toewijzen / (FR * (K - aantalvast))))
  while (!klaar) {
    
    #print(toewijzen)
    if (aantalvast > 0) {
      
      rest      <- start[-idx0] - theta0[-(1:(K - aantalvast))]
      idx       <- which(rest >= 1)
      x         <- sample(idx, 1)
      eraf      <- min(runif(n = 1, min = 0, max = min(perkeer, rest[x])), toewijzen)
      theta0[K - aantalvast + x] <- theta0[K - aantalvast + x] + eraf
      toewijzen <- toewijzen - eraf
      klaar     <- (length(idx) == 0) | (toewijzen <= 0)

    } else {
      
      rest      <- start
      idx       <- which(rest >= 1)
      x         <- sample(idx, 1)
      eraf      <- min(runif(n = 1, min = 0, max = min(perkeer, rest[x])), toewijzen)
      theta0[K - aantalvast + x]  <- theta0[K - aantalvast + x] + eraf
      toewijzen <- toewijzen - eraf
      klaar     <- (length(idx) == 0) | (toewijzen <= 0)
      
    }
  }
  
    
  
  # cellen die nog op hun oorspronkelijke startwaarden liggen iets ophogen (met veel minder dan 1)
  idx00         <- which(theta0 == 0)
  theta0[idx00] <- theta0[idx00] + 0.01
  
  return(theta0)
  
}

