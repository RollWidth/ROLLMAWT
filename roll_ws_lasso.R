roll_ws_lasso = function(ws,data,tau,k,ST,np,lambda){
   r2   = rollapply(data, width = ws,by = ST,
             FUN = roll_lasso,tau=tau,k=k, lambda = lambda,
            nfirm=nfirm,nz=nz, by.column = FALSE, align = "right")
   mse.boot = (unlist(r2[,1]));
   r        = length(mse.boot);
   beta_roll = matrix(unlist(r2[,2]),nrow = r,ncol = np,byrow = T);
   distance  = (1/sqrt(2))*as.matrix(dist(beta_roll));
   upper.tri(distance,diag = FALSE);
   distance[upper.tri(distance)] <- 0;
   distance_lower = distance[-1,-r];
   TV_m           = as.matrix(abs(distance_lower));
   TV_m_l         = apply(TV_m,2,sum);
   TV_heter       = -sum(TV_m_l)/(r-1); #average time variation
   mse_boot       = mean (mse.boot);
   out = list(mse_boot = mse_boot,TV_heter = TV_heter,beta_roll = beta_roll)
   return(out)
  }
