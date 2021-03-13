MAWT.sim  <- function (data, widths,k,s,nx,lmda,ww){
    .ptime_start <- proc.time()
    rw  <- myFUN.roll.sim(ws_seq = widths,data = data,k = k,s = s, nx = nx); # solve the objective  
    mse.ak <- unlist(rw$mse_boot);# approximate mse
    mse.rw <- unlist(rw$mse_true); #real mse
    TV     <- unlist(rw$TV_heter); #time variation

    fun    <- matrix(cbind(mse.ak,TV),ww,2);  
  #lower bound
    z1_lb_ak    <- min(mse.ak);
    z2_lb_tv    <- min(TV ); 
    z_lb        <- c(z1_lb_ak,z2_lb_tv);        
  #upper bound
    z1_ub_ak   <- max(mse.ak);
    z2_ub_tv   <- max(TV ); 
    z_ub        <- c(z1_ub_ak,z2_ub_tv); 
  # r
    r_ak       <- max(z1_ub_ak - z1_lb_ak,z2_ub_tv - z2_lb_tv);
  # s
    s.mse.ak   <- sort (mse.ak); ##sort all mse
    s.TV       <- sort(TV);
    mse.ak.diff <- vector();##the each pair of difference
    TV.diff     <- vector();
    for (i in 2:ww){
        mse.ak.diff[i]  <- abs(s.mse.ak[i]-s.mse.ak[i-1]);
       TV.diff[i]       <- abs(s.TV [i]-s.TV [i-1]);
    }
    s.mse.ak.diff.    <- min (mse.ak.diff[-1]);
    s.TV.diff.        <- min (TV.diff[-1]);
    s.ak              <- min(s.mse.ak.diff., s.TV.diff.); #finally, we get the s
 # get e
    e.ak         <- s.ak/(2*2*(r_ak-s.ak));# get e
 # 
    L            <- list(c (z1_ub_ak,z2_ub_tv));
    xe.          <- list () ;
    x.           <- vector () ;
    zn.          <- list () ;
    z.           <- list () ;
    j            <- 0 ;
 # main loop
   while (length(L) >=1 ) {
           j     <- j+1 ;     	
           z.[j] <- L[1] ;	
           L     <- setdiff(L, z.[j]);
           w     <- vector ();
           for (i in 1:2){
              w[i] <- 1/(max(s.ak, z.[j][[1]][i] - z_lb[i]));          	
           }          
           g.f     <-   vector ();
           for (q in 1:ww){
               g.f[q]  <-   max ((w[1]*(mse.ak[q] - z_lb[1])), (w[2]*(TV[q] - z_lb[2]))) + e.ak * (w[1]*(mse.ak[q] - z_lb[1]) + w[2]*(TV[q] - z_lb[2]))   
            }                         	                                	                    
           loc      <- which.min (g.f)
           x.       <-loc +(min(widths)-1);   
           if (g.f[loc] < 1) {
            xe.     <- union(x.,xe.); 
            zn.     <- union(list(fun[loc,]),zn.);          
            z.k     <- list();
            if (fun[loc,1] > z_lb[1]){
              z.k[[1]]  <-  union(fun[loc,1],z.[j][[1]][2]) ;
            }      
            if (fun[loc,2] > z_lb[2]){
              z.k[[2]]  <-  union(z.[j][[1]][1],fun[loc,2]) ;
            } 
            z.k.n <- z.k[lapply(z.k,length)>0];#this sentence if for removing the null list element
               L        <-  c(z.k.n,L);               	
            }                        		
       }
      unlist(xe.);  # the decision set
      nl <- length(unlist(xe.));
      zn.;  # the solution set
      znmatrix. <- matrix(0,nl,2)
      for (i in 1:nl){
            znmatrix.[i,] <- zn.[[i]]
       }     
       mse.rw.n  <-  mse.rw [( unlist(xe.) - min(widths) + 1 ) ];# real mse
       mse.a.r   <- (znmatrix.[,1])/(mse.rw.n); # ratio = mse.app/mse.real
       znxe      <-  cbind(znmatrix.,mse.rw.n,unlist(xe.),mse.a.r); #set: size,tv,mse,ratio
       if(nl <= 1){
       	K.o  <- znxe ;
       	C.o  <- znxe ;
       }else {

      #  knee point
       max.f1        <- max(znxe[,1]);# max mse
       max.f2        <- max(znxe[,2]);# max -tv       
       min.f1        <- znxe[which.max(znxe[,2]),1];# min mse
       min.f2        <- znxe[which.max(znxe[,1]),2];# min -tv      
       ## the line of two extreme points Ax+By+C=0, where, x = mse, Y=(-tv)
       A.f12         <-  (min.f2-max.f2);##
       B.f12         <-  (min.f1-max.f1); ##
       C.f12         <-  max.f1*max.f2-min.f1*min.f2; ##
       ## 
       D.f12         <- vector(); # the distance vector for all points
       for (i in 1:nl){
             D.f12[i]<-  abs (A.f12*znxe[i,1] + B.f12*znxe[i,2] + C.f12 ) / ( sqrt (A.f12^2 + B.f12^2 ) )
        }     
       K.o           <-   matrix(znxe[which.max( D.f12),], 1, 5, dimnames=list(c("knee"),c("mse.app","tv","mse.true","size","mse.a.r"))) ;##this is the knee ponit, that is, the optimal rolling wincow size.
      ## compromise solution method 
       ##ideal points   
         ZZN.ST      <-  znmatrix.; ##
        min.f1.1     <-  min(ZZN.ST[,1]);#min mse
        min.f2.2     <-  min(ZZN.ST[,2]);# min -tv
        ##
        max.f1.1     <-  ZZN.ST[which.min(ZZN.ST[,2]),1];#max mse 
        max.f2.2     <-  ZZN.ST[which.min(ZZN.ST[,1]),2];#max -tv
        ## generate weights vector
          lamda <- lmda ;## 
          ZZ    <- vector();
          for (i in 1:nl)   { 
              ZZ[i] <-  (lamda[1]^2) *( ( ( min.f1.1 - ZZN.ST[i,1]  ) /(min.f1.1 - max.f1.1  )  ) ^2) +
                         (lamda[2]^2) *( ( ( min.f2.2  - ZZN.ST[i,2]  ) /(min.f2.2 - max.f2.2  )  ) ^2) 
           } 
           C.o      <-   matrix(znxe[which.min( ZZ ),], 1, 5, dimnames=list(c("compromise"),c("mse.app","tv","mse.true","size","mse.a.r"))) ;##this is the compromise ponit, that is, the optimal rolling wincow size.                            
  }
# the last stop, list the ultimate results
    timings         <- (proc.time() - .ptime_start)[3] ;
    list (kneepoint = K.o, comprosepoint =  C.o, runtime = timings );## the final results
}      
