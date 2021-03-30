
# load functions
source("2a_function_generate_tabs.R")
source("2b_function_restrictions_minimalisation.R")
source("2c_function_prepare_startingvalues.R")
source("2d_function_calculate_deviance.R")
source("2e_function_calculate_gradient.R")
source("2f_function_loglinear_model.R")


# Input: aggregated data-set with variables X,Y,Z
# X: Target variable that is under inspection of audit
# Y: Background categories, audit sample will be selected that is representative with respect to Y
# Z: Indication in initial audit sample or not 0-1
data <- read.table("dataset.txt", header=TRUE)

# Generate frequencytabs from data
tab  <- preparetables(data)

# Compute constant term of deviance function
tot  <- aggregate(tab$freq, by = tab[ , 'Y', drop = FALSE], FUN = sum)
rtot <- aggregate(tab$freq, by = tab[ , c('X','Y')], FUN = sum)
cst  <- 2 * sum(tot$x * log(tot$x), na.rm=TRUE) - 2 * sum(rtot$x * log(rtot$x), na.rm=TRUE)

# deviance independence model

model_before <- fitmodel(model = 'freq ~ X*Y + Y*Z',
                         tab = tab)

# Prepare restrictions for minimalization procedure
#extra, nweg, start, beschikbaar, typ

# Specify the number of additional cases you want to include in your audit sample
extra     <- c(300L)

# Specify the number of cases you allow to be excluded from  the initial audit sample as a 
# fraction of the number you include
nweg      <- c(0.05L)

ex_nweg   <- expand.grid(extra=extra,nweg=nweg)

# Specify the number of iterations you allow the search algorithm to run
maxpoging <- 200L

conv             <- rep(NA_integer_, nrow(ex_nweg))
G2sol            <- rep(NA_real_, nrow(ex_nweg))
extra_toegewezen <- rep(NA_real_, nrow(ex_nweg))
extra_weggelaten <- rep(NA_real_, nrow(ex_nweg))
tab_extra        <- tab

start       <- tab$freq[tab$Z  == '1']
beschikbaar <- tab$freqZ[tab$Z == '1']
rijtotalen  <- tab$rtot[tab$Z  == '1']

#seed = sum(i,j)
#set.seed(seed)
set.seed(20200810 * 2)
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