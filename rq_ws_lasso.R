rq_ws_lasso = function(ws,data,tau,lambda,T){
   beta =  matrix(0,(T - ws + 1),22)
   for (l in 1: (T - ws + 1)){
     dataw = data [(l:(l+ws-1)),];
     rewp = rq_lasso(data = dataw,tau,lambda)
     beta[l,] =  rewp$beta_roll;
   }
   out = list(beta_roll = beta)
   return(out)
  }