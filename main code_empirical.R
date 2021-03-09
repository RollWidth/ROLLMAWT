
#README:

#Name: MAWT_MainCode 

#Published in : MAWTROLL

#Description : 'The main code for the empirical section. In this part, we run 
#                a network model,high-dimension quantile regression based on
#                the LASSO technic(hereafter, HQR-LASSO),for tail risk using
#                optimal rolling window method with MAWT algorithm'

# 
#Keywords : quantile regression,dimension reduction, tail risk, optimal rolling window method

#See Also : myFUN_roll_lasso, roll_ws_lasso, roll_lasso, MAWT.mean, vare,Netden
#Author : Xingmin Zhang

#Datafile : BANK RETURN AND MACRO CAPM.csv


  # Install packages and source our own code
  rm(list=ls())
  libraries = c("bootstrap","quantreg","psych","rugarch","miscTools","psych","GGally","zoo")
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
  })
  lapply(libraries, library, quietly = TRUE, character.only = TRUE)
  library(network)
  library(sna)
  library(tibble)
  library(psych)

  source("myFUN_rq_lasso.r");
  source("myFUN_roll_lasso.r");
  source("rq_ws_lasso.r");
  source("rq_lasso.r");
  source("roll_ws_lasso.r");
  source("roll_lasso.r");
  source("MAWT.mean.r");
  source("vare.r");
  source("Netden.r");

# Step.1. Decompose return into common and idiosyncratic parts
  # 1.1. read data
   data   = read.csv("BANK RETURN AND MACRO CAPM.csv", header = TRUE);
   data_y = as.matrix(data[, c(2:21)]);#log returns of banks and insurances
   data_capm = as.matrix(data[, c(22:27)]);#fama-french five factor
   data_mac = as.matrix(data[, c(28:30)]);# macro variables

   str(data);
   str(data_y);
   str(data_mac);
   str(data_capm);

   # set the quantile level
   tau = 0.1
   cpd = ncol(data_y);# the number of firms
   n = nrow(data_y)# the number of observations

  # run fama-french model to get the common part and idiosyncratic part
  ywnew = matrix(0,n,cpd);
  idi = matrix(0,n,cpd);
  R = vector();# R square
  for(k in 1:cpd){
     data_yyy = data_y[,k];
     bw = lm(data_yyy ~ data_capm);#decomposition equation
     ywnew[,k] = as.vector(fitted.values(bw));
     idi[,k]   =  as.numeric(bw$residuals);
     R[k]     =  (summary(bw))$r.squared;
  }
  nam = colnames(data)[2:21];# name 
  R2 = cbind(nam, R);
  write.csv(idi,file = paste("idio_part.csv"));
  write.csv(ywnew,file = paste("common_part.csv"));
  write.csv(R2,file = paste("R_square.csv"));




  # Step.2. Compute the optimal rolling windth using our new approach
  # 2.1. compute CSI300 volatility for macro state variables
  spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                   distribution.model = "std") # without fixed parameters here
  fit  = ugarchfit(spec, data = data_mac[,1],fit.control=list(scale=1), solver.control=list(trace=1)) # fit
  sig. = sigma(fit); # fitted hat{sigma}_t
  csi300vari = as.numeric(sig.);
  data_macro = cbind(csi300vari,data_mac[,2:3]);
  Z = as.matrix(data_macro);
  str(Z);
  write.csv(Z,file = "macro-state.csv");# macro state variables

 #2.2. initial data
  idiosyn = read.csv(file = paste("idio_part.csv"))[,-1];# idiosyncratic part
  Z = read.csv("macro-state.csv")[,-1];# macro state variables
  str(idiosyn);
  str(Z);
  X = idiosyn;
  cpd = ncol(X);# the number of banks
  n = nrow(X);# the full sample size 
  nz = ncol(Z);# the number of macro economic variables
  k = 100;# the repeated times in the bootstrap process.
  tau = 0.1;# the tail quantile

 #2.3. solve the optimal rolling window size
  weidj = seq(as.integer (max(1.5*(n)^(2/3),20)),as.integer(min(3*(n)^(2/3),n-1)),by = 1)
  widths = data.frame (weidj);
  ww =length(weidj);
  s = 1;# the roll forward width each time 
  NP = cpd + nz - 1;# the number of all variables
  lambda = c(rep(0.01,cpd-1),rep(0,nz));# the penalty value
  
  OptimalRoll = MAWT.mean(widths = widths,X = X,Z = Z,k = k,s = s,ww = ww,tau = tau,lambda = lambda,nfirm = cpd,NP = NP)
  OptimalRoll;



  #Step.3. Run network risk model using the optimal rolling window size, 
  #        also doing network contagion analysis
  tau1 = 0.1;# lower tail
  tau2 = 0.5;# mediean state
  lambda = c(rep(0.01,cpd - 1),rep(0,nz));
  ws = 424;

  NetModel = Netden(ws=ws,X=X,Z=Z,nfirm = cpd,tau1=tau1,tau2=tau2,lambda=lambda,n=n,nz=nz)
  # the outputs of function "NetModel" include: the estimated coefficients, total connectedness,
  # total density, in-degree, out-degree.
  









