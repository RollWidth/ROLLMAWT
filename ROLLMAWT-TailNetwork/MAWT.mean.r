MAWT.mean  <- function (widths,X,Z,k,s,ww,tau,lambda,nfirm,NP,nz){
  mse.akk =  matrix(0,nfirm,ww);# MSE
  TVV =  matrix(0,nfirm,ww);#TV
  for (g in 1:nfirm) {
    cat("Firm:", g);
    Yg  =  X[g];# the return of g-th firm
    xxg  = X[,-g];# returns excepting g-th firm
    data = cbind (Yg,xxg,Z);
    rw = myFUN_roll_lasso (ws_seq = widths,data = data,tau = tau,k = k,s = s,
                  lambda = lambda,np = NP,nfirm=nfirm,nz=nz);
    mse.akk[g,]  = c(unlist(rw$mse_boot));
    TVV[g,] = unlist(rw$TV_heter); #time variation
  }  
  write.csv (mse.akk, file = "MSE.csv")
  write.csv (TVV, file = "TV.csv")

  mse.ak = as.vector(apply(mse.akk,2,mean));
  TV = as.vector(apply(TVV,2,mean));
  fun  = matrix(cbind (mse.ak,TV),ww,2);  
  #Step 3. start solve bi-objective problems
  #3.1 lower bound
  z1_lb_ak = min(mse.ak);
  z2_lb   =  min(TV );
  z_lb   =  c(z1_lb_ak,z2_lb);        
  #3.2 upper bound
  z1_ub_ak  = max(mse.ak);
  z2_ub    =  max(TV );
  #3.3 get r
  r_ak =  max(z1_ub_ak-z1_lb_ak,z2_ub-z2_lb);
  #3.4 get s
  s.mse.ak = sort (mse.ak);
  s.TV  = sort(TV);
  mse.ak.diff = vector();
  TV.diff   = vector();
  for (i in 2:ww){
    mse.ak.diff[i]= abs(s.mse.ak[i]-s.mse.ak[i-1]);
    TV.diff[i]    =  abs(s.TV [i]-s.TV [i-1]);
  }
  s.mse.ak.diff. <- min (mse.ak.diff[-1]);
  s.TV.diff.  <- min (TV.diff[-1]);
  s.ak      <- min(s.mse.ak.diff., s.TV.diff.);
  #3.5 get e
  e.ak <- s.ak/(2*2*(r_ak-s.ak));
  #3.6 initialation in the main loopprocessing
  L    <- list(c (z1_ub_ak,z2_ub));
  xe.  <- list () ;
  x.    <- vector () ;
  zn.  <- list () ;
  z.    <- list () ;
  j      <- 0 ;
  #3.7  start main loop
  while (length(L) >=1 ) {
    j        <- j+1 ;     	
    z.[j]    <- L[1] ;	
    L        <- setdiff(L, z.[j]);
    w        <- vector ();
    for (i in 1:2){
      w[i]   <- 1/(max(s.ak, z.[j][[1]][i] - z_lb[i]));          	
    }          
    g.f      <-   vector ();
    for (q in 1:ww){
      g.f[q] <-   max ((w[1]*(mse.ak[q]-z_lb[1])), (w[2]*(TV[q]-z_lb[2])))+e.ak*(w[1]*(mse.ak[q]-z_lb[1])+ w[2]*(TV[q]-z_lb[2]))   
    }                         	                                	                    
    loc      <- which.min (g.f)
    x.       <-  unlist(widths)[loc];#loc +(min(widths)-1);
    if (g.f[loc] < 1) {
      xe.    <- union(x.,xe.); 
      zn.    <- union(list(fun[loc,]),zn.);          
      z.k    <- list();
      if (fun[loc,1] > z_lb[1]){
        z.k[[1]]<-  union(fun[loc,1],z.[j][[1]][2]) ;
      }      
      if (fun[loc,2] > z_lb[2]){
        z.k[[2]] <-  union(z.[j][[1]][1],fun[loc,2]) ;
      } 
      z.k.n <- z.k[lapply(z.k,length)>0];#this sentence if for removing the null list element
      L <-  c(z.k.n,L);               	
    }                        		
  }
  unlist(xe.);  # the decision set
  nl <- length(unlist(xe.));
  zn.; # the solution set
  znmatrix. <- matrix(0,nl,2)
  for (i in 1:nl){
    znmatrix.[i,] <- zn.[[i]]
  }     
  znxe <-  cbind(znmatrix.,unlist(xe.)); ##the set of mse, tv,rolling window size, ratio between approximate and real mse.
  if(nl <= 1){
    K.o  = znxe ;
    C.o = znxe ;
  }else {
    # 3.8 get the best one from the Pareto solution set
    # 3.8.2 the second method, knee point
    max.f1<- max(znxe[,1]);#find the maximum of mse, the first objective;
    max.f2<- max(znxe[,2]);#find the maximum of (-tv), the second objective;       
    min.f1<- znxe[which.max(znxe[,2]),1];#find the minimum of mse, the first objective;
    min.f2<- znxe[which.max(znxe[,1]),2];#find the minimum of (-tv), the second objective;       
    # the line of two extreme points Ax+By+C=0, where, x = mse, Y=(-tv)
    A.f12<-  (min.f2-max.f2);
    B.f12<-  (min.f1-max.f1); 
    C.f12<-  max.f1*max.f2-min.f1*min.f2; 
    # then calculate the distance between any point from Pareto set and extreme line
    D.f12<- vector(); # the distance vector for all points
    for (i in 1:nl){
      D.f12[i]<-  abs ( A.f12*znxe[i,1] + B.f12*znxe[i,2] + C.f12 ) / ( sqrt (A.f12^2 + B.f12^2 ) )
    }     
    K.o      <-   matrix(znxe[which.max( D.f12),], 1, 3, dimnames=list(c("knee"),c("mse","tv  ","   size"))) ;##this is the knee ponit, that is, the optimal rolling wincow size.
    #3.8.3  the third method, apply the compromise solution method to get the unique optimal solution 
    #ideal points   
    ZZN.ST   <-  znmatrix.;
    min.f1.1 <-  min(ZZN.ST[,1]);#find the minimum of mse, the first objective;  
    min.f2.2 <- min(ZZN.ST[,2]);#find the minimum of (-tv), the second objective;

    max.f1.1 <-  ZZN.ST[which.min(ZZN.ST[,2]),1]; 
    max.f2.2 <-  ZZN.ST[which.min(ZZN.ST[,1]),2];
    # generate weights vector
    lamda <- c(0.7,0.3) ;
    ZZ  <- vector();
    for (i in 1:nl)   { 
      ZZ[i]  <-  (lamda[1]^2) *( ( ( min.f1.1 - ZZN.ST[i,1]  ) /(min.f1.1 - max.f1.1  )  ) ^2) +
        (lamda[2]^2) *( ( ( min.f2.2  - ZZN.ST[i,2]  ) /(min.f2.2 - max.f2.2  )  ) ^2) 
    } 
    C.o  <-   matrix(znxe[which.min( ZZ ),], 1, 3, dimnames=list(c("compromise"),c("mse","tv  ","   size")));#this is the compromise ponit, that is, the optimal rolling wincow size.  
  }                          
  # 4. the last stop, list the ultimate results
  list (kneepoint = K.o, comprosepoint =  C.o);#the final results
}      
