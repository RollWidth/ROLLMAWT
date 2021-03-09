#README:

#Name: MAWT_Compare

#Published in : MAWTROLL

#Description : 'Compare network density among different rolling window sizes,
#                113, half-year
#                230, one-year
#                424, optimal rolling window size
#                460, two-year '

#Keywords : network density, compartion

#See Also : myFUN_roll_lasso, rq_ws_lasso, rq_lasso, myFUN_rq_lasso, vare
#Author : Xingmin Zhang

#Datafile : idio_part.csv, macro-state.csv

  rm(list=ls())
  libraries = c("bootstrap","quantreg","psych","rugarch","miscTools","psych","GGally")
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
  })
  lapply(libraries, library, quietly = TRUE, character.only = TRUE)
  library(network)
  library(sna)
  library(tibble)
  library(psych)

  source("myFUN_roll_lasso.r");
  source("rq_ws_lasso.r");
  source("rq_lasso.r");
  source("myFUN_rq_lasso.r");
  source("vare.r");
  source("Netden.r");

  # 1. input data
  idiosyn = read.csv(file = paste("idio_part.csv"))[,-1];# idiosyncratic part
  X = as.matrix(idiosyn);
  Z = read.csv("macro-state.csv")[,-1];
  str (X);
  str(idiosyn);
  str (Z);

  n =  nrow(X);# full size
  cpd = ncol(X);# number of firms
  nz = ncol(Z);#the number of macro state variables
  tau1 = 0.1;# lower tail
  tau2 = 0.5;# mediean state
  lambda = c(rep(0.01,cpd - 1),rep(0,nz));

  # 2. estimate betas apllying network model
  WsSet = c(115,230,460);# the set of all rolling window sizes
  Comp = apply(WsSet, FUN = Netden, X=X,Z=Z,nfirm = cpd,tau1=tau1,tau2=tau2,,lambda=lambda,n=n,nz=nz)
  
 