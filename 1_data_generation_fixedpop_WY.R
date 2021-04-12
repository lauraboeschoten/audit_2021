
set.seed(123)

setwd("F:/CBS_audit_2020-master")

# number of conditions
x_ncon = c(1)
y_ncon = c(1, 2, 3, 4)
z_ncon = c(1)

conditions = list(x_ncon = x_ncon, y_ncon = y_ncon, z_ncon = z_ncon)
xyzw_con = expand.grid(conditions)

WX = list()
WX[[1]] = matrix(c(.9/3, .05/3, .05/3, 
                   .1/3, .8/3, .1/3,
                   .15/3, .15/3, .7/3), nrow = 3, ncol = 3, byrow = TRUE)


WY = list()
WY[[1]] = matrix(c(.8/3, .1/3, .1/3, 
                   .1/3, .8/3, .1/3,
                   .1/3, .1/3, .8/3), nrow = 3, ncol = 3, byrow = TRUE)
WY[[2]] = matrix(c(.6/3, .2/3, .2/3, 
                   .2/3, .6/3, .2/3,
                   .2/3, .2/3, .6/3), nrow = 3, ncol = 3, byrow = TRUE)
WY[[3]] = matrix(c(.9/3, .05/3, .05/3, 
                   .1/3, .8/3, .1/3,
                   .15/3, .15/3, .7/3), nrow = 3, ncol = 3, byrow = TRUE)
WY[[4]] = matrix(c(.8/3, .1/3, .1/3, 
                   .2/3, .6/3, .2/3,
                   .3/3, .3/3, .4/3), nrow = 3, ncol = 3, byrow = TRUE)

XZ = list()
XZ[[1]] = matrix(c(.018, .97/3, 
                   .010, .97/3,
                   .002, .97/3), nrow = 3, ncol = 2, byrow = TRUE)


X = c(1,2,3)
Y = c(1,2,3)
W = c(1,2,3)
Z = c(1,2)

variables = list(X = X, Y = Y, W = W, Z = Z)

generated_datasets = list()
generated_distributions = list()

# loop over all conditions (combinations of different relations between X Y W Z )
for (i in 1:nrow(xyzw_con)){
  
  XYWZ = expand.grid(variables)
  XYW = expand.grid(variables[1:3])
  
  for (j in 1:nrow(XYW)){
    # if X=... and W=... find the corresponding probability in the matrix WX
    if(XYW[j,'W'] == 1 & XYW[j,'X'] == 1){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,1]}
    if(XYW[j,'W'] == 1 & XYW[j,'X'] == 2){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,2]}
    if(XYW[j,'W'] == 1 & XYW[j,'X'] == 3){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,3]}
    if(XYW[j,'W'] == 2 & XYW[j,'X'] == 1){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,1]}
    if(XYW[j,'W'] == 2 & XYW[j,'X'] == 2){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,2]}
    if(XYW[j,'W'] == 2 & XYW[j,'X'] == 3){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,3]}
    if(XYW[j,'W'] == 3 & XYW[j,'X'] == 1){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,1]}
    if(XYW[j,'W'] == 3 & XYW[j,'X'] == 2){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,2]}
    if(XYW[j,'W'] == 3 & XYW[j,'X'] == 3){XYW[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,3]}
    # if X=... and Y=... find the corresponding probability in the matrix WY
    if(XYW[j,'W'] == 1 & XYW[j,'Y'] == 1){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,1]}
    if(XYW[j,'W'] == 1 & XYW[j,'Y'] == 2){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,2]}
    if(XYW[j,'W'] == 1 & XYW[j,'Y'] == 3){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,3]}
    if(XYW[j,'W'] == 2 & XYW[j,'Y'] == 1){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,1]}
    if(XYW[j,'W'] == 2 & XYW[j,'Y'] == 2){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,2]}
    if(XYW[j,'W'] == 2 & XYW[j,'Y'] == 3){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,3]}
    if(XYW[j,'W'] == 3 & XYW[j,'Y'] == 1){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,1]}
    if(XYW[j,'W'] == 3 & XYW[j,'Y'] == 2){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,2]}
    if(XYW[j,'W'] == 3 & XYW[j,'Y'] == 3){XYW[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,3]}
    
  }
  
  # Assuming independence (relation between XW is independent of relation XY, we multiply the two probabilities
  # multiply the number by 9 to make all final probabilities sum to one)
  XYW[,'prob']                = (XYW[,'WX']*XYW[,'WY'])*3
  
  generated_distributions[[i]] = XYW
  
  # Generate one fixed population of 10000 with the chosen distribution of XYW.
  XYW = cbind(XYW[,1:3], N = rmultinom(1, size = 10000, prob = XYW[,'prob']))
  
  # Now generate 1000 datasets XYWZ corresponding to audit samples drawn from this population
  # according to the probabilities in the matrix XZ (again assuming independence w.r.t. XW and XY)
  XYW <- cbind(XYW, matrix(NA_integer_, nrow = nrow(XYW), ncol = 1000))
  
  XZ_pop <- XZ[[as.numeric(paste0(xyzw_con[i,3]))]]
  XZ_pop <- XZ_pop / rowSums(XZ_pop)
  
  for (j in 1:nrow(XYW)) {
    XYW[j, -(1:4)] <- rbinom(1000, size = XYW[j, 'N'], prob = XZ_pop[XYW[j,'X'], 1])
  }
  
  XYWZ <- merge(XYWZ, XYW, by = c('X','Y','W'))
  XYWZ[XYWZ$Z == 2, -(1:5)] <- XYWZ$N[XYWZ$Z == 2] - XYWZ[XYWZ$Z == 2, -(1:5)]
  
  XYWZ <- XYWZ[order(XYWZ$Z, XYWZ$W, XYWZ$Y, XYWZ$X), ]
  row.names(XYWZ) <- NULL
  generated_datasets[[i]] = XYWZ
  
}

save(generated_distributions, file = "distributions_fixedpop_WY.RData")
save(generated_datasets,      file = "datasets_fixedpop_WY.RData")
