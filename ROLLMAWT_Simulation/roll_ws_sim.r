roll_ws_sim = function(ws,data,k,ST,nx){
   r2   = rollapply(data, width = ws,by = ST,
             FUN = roll_sim,k=k,by.column = FALSE, align = "right")
   mse.true = (unlist(r2[,1]));
   mse.boot = (unlist(r2[,2]));
   r        = length(mse.boot);
   beta_roll = matrix(unlist(r2[,3]),nrow = r,ncol = nx,byrow = T);
   distance  = (1/sqrt(2))*as.matrix(dist(beta_roll));
   upper.tri(distance,diag = FALSE);
   distance[upper.tri(distance)] <- 0;
   distance_lower = distance[-1,-r];
   TV_m           = as.matrix(abs(distance_lower));
   TV_m_l         = apply(TV_m,2,sum);
   TV_heter       = -sum(TV_m_l)/(r-1); #average time variation
   mse_boot       = mean (mse.boot);
   mse_true       = mean (mse.true);
   out = list( mse_true =  mse_true, mse_boot = mse_boot,TV_heter = TV_heter)
   return(out)
  }