myFUN.roll.sim <- function (ws_seq,data,k,s,nx) {        
  re = apply(ws_seq,1,FUN = roll_ws_sim,data = data, 
               k = k,ST = s,nx = nx)    
  mse_true = unlist(sapply(re,"[",1))
  mse_boot = unlist(sapply(re,"[",2))
  TV_heter = unlist(sapply(re,"[",3))
  out = list(mse_true = mse_true,mse_boot = mse_boot, TV_heter = TV_heter)  
  return(out)
}



