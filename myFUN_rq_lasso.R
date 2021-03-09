myFUN_rq_lasso <- function (ws_seq,data,tau,s,lambda,np,r) {      
  re = apply(ws_seq,1,FUN = rq_ws_lasso,data = data,tau = tau, 
               ST = s,lambda = lambda, np = np, r = r)    
  beta     = sapply(re,"[",q)
  names(beta)= paste0("beta_roll",c(1:r))
  out = list(beta = beta)  
  return(out)
}

