
#README:

#Name: MAWT_OptiRollSize 

#Published in : MAWTROLL

#Description : 'Compute the optimal rolling windth using our new approach'

#Keywords : quantile regression,dimension reduction, tail risk, optimal rolling window method

#See Also : myFUN_roll_lasso, roll_ws_lasso, roll_lasso, MAWT.mean, vare,Netden
#Author : Xingmin Zhang

#Datafile : BANK RETURN AND MACRO CAPM.csv, macro-state.csv, idio_part.csv


  # 2.1. compute CSI300 volatility for macro state variables
  dejijr = read.csv("BANK RETURN AND MACRO CAPM.csv", header = TRUE);
  data_mac  = as.matrix(dejijr[, c(28:30)])
  spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                   distribution.model = "std") # without fixed parameters here
  fit  = ugarchfit(spec, data = data_mac[,1],fit.control=list(scale=1), solver.control=list(trace=1)) # fit
  sig. = sigma(fit) # fitted hat{sigma}_t
  csi300vari = as.numeric(sig.);
  data_macro = cbind(csi300vari,data_mac[,2:3]);
  Z = as.matrix(data_macro);
  str(Z);
  write.csv(Z,file = "macro-state.csv");# macro state variables

 #2.2. initial data
  idiosyn = read.csv(file = paste("idio_part.csv"))[,-1];# idiosyncratic part
  str(idiosyn);
  X = idiosyn;
  nfirm = ncol(X);# the number of banks
  n = nrow(X);# the full sample size 
  nz = ncol(Z);# the number of macro economic variables
  k = 100;# the repeated times in the bootstrap process.
  tau = 0.1;# the tail quantile

 #2.3. solve the optimal rolling window size
  weidj = seq(as.integer (max(1.5*(n)^(2/3),30)),as.integer(min(3*(n)^(2/3),n-1)),by = 1)
  widths = data.frame (weidj);
  ww =length(weidj);
  s = 1;# the roll forward width each time 
  NP = nfirm + nz - 1;# the number of all variables
  lambda = c(rep(0.01,nfirm-1),rep(0,nz));# the penalty value
  
  OptimalRoll = MAWT.mean(widths = widths,X = X,Z = Z,k = k,s = s,ww = ww,tau = tau,lambda = lambda,nfirm = nfirm,NP = NP)
  OptimalRoll;