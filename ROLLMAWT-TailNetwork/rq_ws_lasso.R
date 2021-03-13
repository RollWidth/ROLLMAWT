rq_ws_lasso = function(ws,data,tau,lambda,T,NP,nfirm,nz){
   beta =  matrix(0,(T - ws + 1),NP)
   for (l in 1: (T - ws + 1)){
     dataw = data [(l:(l+ws-1)),];
     rewp = rq_lasso(data = dataw,tau,lambda,nfirm,nz)
     beta[l,] =  rewp$beta_roll;
   }
   out = list(beta_roll = beta)
   return(out)
  }