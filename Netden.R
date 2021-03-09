#Description : 'This function is the main function for our network analysis'
#               first, we run the high-dimensional quantile regression with lasso;'
#               second, we do network analysis, like network density, in-degree, out-degree;'

Netden = function(ws,X,Z,nfirm,tau1,tau2,lambda,n,nz){
  betat = list();
  betam = list();
  vartail10 = matrix(0,nfirm,n)
  varmed50  = matrix(0,nfirm,n)
  for (g in 1:nfirm) {
    cat("Firm:", g);
    Yg   = X[,g];# the return of g-th firm
    xxg  = X[,-g];# return except g-th firm
    dew = cbind (Yg,xxg,Z);
    Alnu = ncol(dew);#
    ZIndex = c((Alnu-nz+1):Alnu)
    # VaR
    varw = vare(data2 = dew,q1 = tau1,q2 = tau2,nzw = ZIndex);
    vartail10[g,] = varw$var10;
    varmed50[g,] = varw$var50;
    bfunt  = rq_ws_lasso(ws = ws,data = dew,tau = tau1,lambda = lambda,T = n);
    bfuntm = rq_ws_lasso(ws = ws,data = dew,tau = tau2,lambda = lambda,T = n);
    betat[[g]] = bfunt$beta_roll;
    write.csv(betat[[g]],paste0("beta_roll_tau01_",ws,"_",g,".csv"))
    betam[[g]] = bfuntm$beta_roll;
    write.csv(betam[[g]],paste0("beta_roll_tau05_",ws,"_",g,".csv"))
  }
  # network analysis
    rpd = (n - ws + 1);# number of sub-windows
  # since each firm does not regress on itself, we need to insert a zero column
  # vector in the position of every firms' partial derivative matrix
    vec_zero = matrix(0, rpd, 1)
    der.c = array(0, dim = c(rpd, cpd, cpd))
    for (i in 1:cpd) {
      der.c[, , i] = insertCol(as.matrix(read.csv(file=paste0("beta_roll_tau01_",ws,"_",i,".csv")))[, 2:cpd], i, vec_zero)
    }
  # generate the network matrix
   con = array(0, dim = c(cpd, cpd, rpd))
   for (i in 1:rpd) {
     con.v = rep(0, cpd)
     for (j in 1:cpd) {
       con.v = rbind(con.v, der.c[i, , j])
     }
     con[, , i] = con.v[-1, ]
   }
  # the total connectedness
    total.c = rep(0, rpd)
    for (i in 1:rpd) {
      total.c[i] = sum(abs(con[, , i]))
    }
    new_tol = scales::rescale(total.c)
    write.csv(new_tol, file = paste0("total_network_scale_",ws,".csv"), row.names = FALSE)
    write.csv(total.c, file = paste0("total_network_",ws,".csv"), row.names = FALSE)

  # in-degree 
  total.in.b = matrix(0,rpd,cpd);
  for (i in 1:rpd) {
    total.in.b[i,] = rowSums(abs(con[, , i]))
  }
  total.in.all = rowSums(total.in.b);

  # out-degree  
  total.out.b = matrix(0,rpd,cpd);
  for (i in 1:rpd) {
    total.out.b[i,] = colSums(abs(con[, , i]))
   }
  total.out.all = rowSums(total.out.b);

  tc_out = cbind(total.in.b, total.out.b,total.in.all,total.out.all)
  tc_group = tc_out;
  write.csv(tc_group, file = paste0("total_in_and_out_idio_",ws,".csv"), row.names = FALSE)
 }

