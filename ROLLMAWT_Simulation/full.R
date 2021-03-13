full <- function (y,X,n,k){
       bwf        <- lm(y~X);
       betaf      <- coef(bwf)[2:3]; 
       var_b       <- c(vcov(bwf)[2,2],vcov(bwf)[3,3])         
       mse_full_real <- sum(var_b);
      
       # bootstrap method
         fityf         <- fitted(bwf);
         redid_center <- residuals(bwf);
         mse.boot1 <- replicate(k, expr = {
            bootsam <- as.numeric(sample(redid_center, size = n, replace = TRUE)); #bootstrap sample process
            betaboot <- coef(lm((bootsam + fityf)~X))[2:3];
            betaboot
           })
        mseboot <- sum(apply(mse.boot1,1,sd)^2)
  list(mse.app.f = mseboot, mse.true.f = mse_full_real)
}