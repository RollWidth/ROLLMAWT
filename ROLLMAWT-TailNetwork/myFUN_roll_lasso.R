myFUN_roll_lasso <- function (ws_seq,data,tau,k,s,lambda,np,nfirm=nfirm,nz=nz) {      
  re = apply(ws_seq,1,FUN = roll_ws_lasso,data = data,tau = tau, 
               k = k,ST = s,lambda = lambda, np = np,nfirm=nfirm,nz=nz)    
  mse_boot = unlist(sapply(re,"[",1))
  TV_heter = unlist(sapply(re,"[",2))
  beta     = sapply(re,"[",3)
  names(beta)= paste0("beta_roll",c(1:length(mse_boot)))
  out = list(mse_boot = mse_boot, TV_heter = TV_heter, beta = beta)  
  return(out)
}

