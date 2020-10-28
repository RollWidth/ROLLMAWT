MAWT.PRE.sim <- function(Y,xx1,xx2,n,m,k,widths,ww,s,lmda){
        # method1. knee point       
        method1 = matrix (0,m,5, dimnames=list( c(1:m), c("mse.app","tv","mse.true","size","mse.a.r") ) );
        # method2. compromise point.   
        method2  = matrix (0,m,5, dimnames=list( c(1:m), c("mse.app","tv","mse.true","size","mse.a.r") ) ); 
        # run time for each repeat process   
        run.time = list (); 

        mse.app.f  = vector();
        mse.true.f = vector();

     for (i in 1:m){
          cat("times_",i); 
          XX           = cbind(xx1[,i],xx2[,i]);
          data         = cbind(Y[,i],XX);
          nx           = ncol(XX)
          mawt         = MAWT.sim (data = data, widths = widths,k = k,s = s,nx = nx, lmda = lmda, ww = ww)
          method1[i,]  = mawt$kneepoint;# knee point
          method2[i,]  = mawt$comprosepoint;# compromise point.
          run.time[[i]] = mawt$runtime;  #the run time  

          Fu                 <- full (y = Y[,i],X=XX,n = n,k = k,ws = method1[i,4]);# the full sample estimation
          mse.app.f[i]       <- Fu$mse.app.f;
          mse.true.f[i]      <- Fu$mse.true.f;       
        }
       list( method1 = method1, method2 = method2,run.time = run.time,
             mse.app.f = mse.app.f, mse.true.f = mse.true.f);
 }      
