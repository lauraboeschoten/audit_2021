library(tidyverse)
library(dplyr)

#setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")
setwd("F:/CBS_audit_2020-master")

load("datasets_fixedpop_WY.RData")
load("distributions_fixedpop_WY.RData")

source("2a_function_generate_tabs.R")
source("2b_function_restrictions_minimalisation.R")
source("2c_function_prepare_startingvalues.R")
source("2d_function_calculate_deviance.R")
source("2e_function_calculate_gradient.R")
source("2f_function_loglinear_model.R")
source("2g_function_generate_W_for_audit.R")
source("2h_function_proportionW_and_WX.R")

n_conditions  = 4
n_iterations  = 1000
n_results     = 9

results <- vector(mode = "list", length = n_conditions)

for(i in 1:n_conditions){
  results[[i]] <- vector(mode = "list", length = n_iterations)
}

# loop over conditions
for(i in 1:n_conditions){
  
  # loop over iterations
  for(j in 1:n_iterations){
    cat(sprintf('Condition %d iteration %d', i, j))
    # select dataset j from condition i
    data <- generated_datasets[[i]][,c(1:4,j+5)]
    
    # Generate frequencytab from data
    tab  <- preparetables(data)
    
    tab$X = as.factor(tab$X)
    tab$Y = as.factor(tab$Y)
    
    # Compute constant term of deviance function
    tot  <- aggregate(tab$freq, by = tab[ , 'Y', drop = FALSE], FUN = sum)
    rtot <- aggregate(tab$freq, by = tab[ , c('X','Y')], FUN = sum)
    cst  <- 2 * sum(tot$x * log(tot$x), na.rm=TRUE) - 2 * sum(rtot$x * log(rtot$x), na.rm=TRUE)
    
    # deviance independence model
    
    model_before <- fitmodel(model = 'freq ~ X*Y + Y*Z',
                       tab = tab)

    # Prepare restrictions for minimalization procedure
    #extra, nweg, start, beschikbaar, typ
    
    extra     <- c(300L)
    nweg      <- c(0.05)
    ex_nweg   <- expand.grid(extra=extra,nweg=nweg)
    maxpoging <- 200L
    
    conv             <- rep(NA_integer_, nrow(ex_nweg))
    G2sol            <- rep(NA_real_, nrow(ex_nweg))
    extra_toegewezen <- rep(NA_real_, nrow(ex_nweg))
    extra_weggelaten <- rep(NA_real_, nrow(ex_nweg))
    tab_extra        <- tab
    
    start       <- tab$freq[tab$Z  == '1']
    beschikbaar <- tab$freqZ[tab$Z == '1']
    rijtotalen  <- tab$rtot[tab$Z  == '1']
    
    seed = sum(i,j)
    set.seed(seed)
    
    for (s in 1:nrow(ex_nweg)) {
      cat(sprintf('Bepaal optimale toewijzing van %d extra eenheden en %d weglaten\n', 
                  ex_nweg[s,1], ex_nweg[s,1]*ex_nweg[s,2]))
      flush.console()
      
      prepAb <- prepareerRestricties(extra       = ex_nweg[s,1],
                                     nweg        = ex_nweg[s,2],
                                     start       = start,
                                     beschikbaar = beschikbaar,
                                     type        = 2)
      
      beste <- Inf
      
      for (poging in 1:maxpoging) {
        cat(sprintf('---Poging %d van %d\n', poging, maxpoging))
        flush.console()
        
        theta0 <- prepareerStartwaarden.type2(extra       = ex_nweg[s,1],
                                              nweg        = ex_nweg[s,2],
                                              start       = start,
                                              beschikbaar = beschikbaar,
                                              Lextra      = 0.10, 
                                              Uextra      = 0.99,
                                              Lweg        = 0.10, 
                                              Uweg        = 0.99)
        
        oplossingP <- constrOptim(theta          = theta0, 
                                  f              = berekenG2.type2,
                                  grad           = berekenG2.grad.type2,
                                  ui             = prepAb$A,
                                  ci             = prepAb$b, 
                                  control        = list(maxit = 1000, abstol = 1e-3),
                                  startaantallen = start,
                                  beschikbaar    = beschikbaar,
                                  rijtotalen     = rijtotalen,
                                  tab            = tab,
                                  cst            = cst)
        
        if (oplossingP$convergence == 0 & oplossingP$value < beste) {
          oplossing <- oplossingP
          beste <- oplossing$value
          cat(sprintf('-----Betere oplossing gevonden met G^2 = %10.8f\n', beste))
          flush.console()
          
        } else if (poging == 1) {
          
          oplossing <- oplossingP
        }
      }
    }
    
    ## Resultaat
    conv[s]  <- oplossing$convergence        # oplossing gevonden? (0 = ja)
    G2sol[s] <- oplossing$value              # waarde doelfunctie in minimum
    
    # Vertaal oplossing naar frequentietabel
    Freqsol                         <- start
    Freqsol[which(beschikbaar > 0)] <- Freqsol[which(beschikbaar > 0)] + oplossing$par[1:(sum(beschikbaar > 0))]
    Freqsol[which(start > 0)]       <- Freqsol[which(start > 0)] - oplossing$par[-(1:(sum(beschikbaar > 0)))]
    Freqsol                         <- c(Freqsol, rijtotalen - Freqsol)
    extra_toegewezen[s]             <- sum(oplossing$par[1:(sum(beschikbaar > 0))])
    extra_weggelaten[s]             <- sum(oplossing$par[-(1:(sum(beschikbaar > 0)))])
    
    tab_extra[,"freqplus"] <- round(Freqsol, digits = 0)
    
    tab_audit <- generate_W_for_audit(data, tab_extra)
    
    prop_list <- results_W_and_WX(tab_audit, tab_extra)
    propW <- prop_list[[1]]
    propWX <- prop_list[[2]]
    
    
    # store results
    reslist = list(conv, 
                   extra_toegewezen, 
                   extra_weggelaten, 
                   model_before$G2, 
                   G2sol, 
                   ((model_before$G2-G2sol)/model_before$G2), 
                   propW,
                   propWX,
                   tab_extra)
    
    results[[i]][[j]] = reslist
    
    saveRDS(results, "results_fixedpop_WY.RDS")
    
  }
  
}
