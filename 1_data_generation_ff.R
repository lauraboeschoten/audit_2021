
setwd("C:/Users/F112974/surfdrive/Onderzoek/CBS/audit_paper/simulatie")
set.seed(123)

# number of conditions
x_ncon = c(1, 2)
y_ncon = c(1, 2)
z_ncon = c(1, 2)

conditions = list(x_ncon = x_ncon, y_ncon = y_ncon, z_ncon = z_ncon)
xyzw_con = expand.grid(conditions)

WX = list()
WX[[1]] = matrix(c(1/3, 0, 0, 
                   0, 1/3, 0,
                   0, 0, 1/3), nrow = 3, ncol = 3, byrow = TRUE)
WX[[2]] = matrix(c(.9/3, .05/3, .05/3, 
                   .1/3, .8/3, .1/3,
                   .15/3, .15/3, .7/3), nrow = 3, ncol = 3, byrow = TRUE)

WY = list()
WY[[1]] = matrix(c(.8/3, .1/3, .1/3, 
                   .1/3, .8/3, .1/3,
                   .1/3, .1/3, .8/3), nrow = 3, ncol = 3, byrow = TRUE)
WY[[2]] = matrix(c(.8/3, .1/3, .1/3, 
                   .2/3, .6/3, .2/3,
                   .3/3, .3/3, .4/3), nrow = 3, ncol = 3, byrow = TRUE)

XZ = list()
XZ[[1]] = matrix(c(.01, .97/3, 
                   .01, .97/3,
                   .01, .97/3), nrow = 3, ncol = 2, byrow = TRUE)
XZ[[2]] = matrix(c(.018, .97/3, 
                   .010, .97/3,
                   .002, .97/3), nrow = 3, ncol = 2, byrow = TRUE)

X = c(1,2,3)
Y = c(1,2,3)
W = c(1,2,3)
Z = c(1,2)

variables = list(X = X, Y = Y, W = W, Z=Z)
XYWZ = expand.grid(variables)

generated_datasets = list()
generated_distributions = list()

# loop over all conditions (combinations of different relations between X Y W Z )
for (i in 1:nrow(xyzw_con)){
  
  for (j in 1:nrow(XYWZ)){
    # if X=... and W=... find the corresponding probability in the matrix WX
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'X'] == 1){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,1]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'X'] == 2){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,2]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'X'] == 3){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][1,3]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'X'] == 1){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,1]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'X'] == 2){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,2]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'X'] == 3){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][2,3]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'X'] == 1){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,1]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'X'] == 2){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,2]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'X'] == 3){XYWZ[j,'WX'] = WX[[as.numeric(paste0(xyzw_con[i,1]))]][3,3]}
    # if X=... and Y=... find the corresponding probability in the matrix WY
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'Y'] == 1){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,1]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'Y'] == 2){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,2]}
    if(XYWZ[j,'W'] == 1 & XYWZ[j,'Y'] == 3){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][1,3]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'Y'] == 1){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,1]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'Y'] == 2){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,2]}
    if(XYWZ[j,'W'] == 2 & XYWZ[j,'Y'] == 3){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][2,3]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'Y'] == 1){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,1]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'Y'] == 2){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,2]}
    if(XYWZ[j,'W'] == 3 & XYWZ[j,'Y'] == 3){XYWZ[j,'WY'] = WY[[as.numeric(paste0(xyzw_con[i,2]))]][3,3]}
    # If Y=.. and Z=... find the corresponding probability in the matrix WZ
    if(XYWZ[j,'X'] == 1 & XYWZ[j,'Z'] == 1){XYWZ[j,'XZ'] = XZ[[as.numeric(paste0(xyzw_con[i,3]))]][1,1]}
    if(XYWZ[j,'X'] == 1 & XYWZ[j,'Z'] == 2){XYWZ[j,'XZ'] = XZ[[as.numeric(paste0(xyzw_con[i,3]))]][1,2]}
    if(XYWZ[j,'X'] == 2 & XYWZ[j,'Z'] == 1){XYWZ[j,'XZ'] = XZ[[as.numeric(paste0(xyzw_con[i,3]))]][2,1]}
    if(XYWZ[j,'X'] == 2 & XYWZ[j,'Z'] == 2){XYWZ[j,'XZ'] = XZ[[as.numeric(paste0(xyzw_con[i,3]))]][2,2]}
    if(XYWZ[j,'X'] == 3 & XYWZ[j,'Z'] == 1){XYWZ[j,'XZ'] = XZ[[as.numeric(paste0(xyzw_con[i,3]))]][3,1]}
    if(XYWZ[j,'X'] == 3 & XYWZ[j,'Z'] == 2){XYWZ[j,'XZ'] = XZ[[as.numeric(paste0(xyzw_con[i,3]))]][3,2]}
    
  }
  
  # Assuming independence (relation between XW is independent of relation XY and YZ, we multiply the 3 probabilities
  # multiply the number by 9 to make all final probabilities sum to one)
  XYWZ[,'prob']                = (XYWZ[,'WX']*XYWZ[,'WY']*XYWZ[,'XZ'])*9
  generated_distributions[[i]] = XYWZ
  
  # now generating 1 dataset per simulation condition
  generated_datasets[[i]] = cbind(XYWZ[,1:4], rmultinom(1000, size = 10000, prob = XYWZ[,'prob']))
  
}

save(generated_distributions, file = "distributions_ff.RData")
save(generated_datasets,      file = "datasets_ff.RData")
