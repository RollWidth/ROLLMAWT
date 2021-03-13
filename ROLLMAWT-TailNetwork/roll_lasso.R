  roll_lasso = function(data,tau,k,lambda,nfirm,nz){
      yw        =  data[,1]
      Zw        =  data[,(1+nfirm):(nz+nfirm)];
      mb        =  matrix(0,nrow(Zw), ncol(Zw))
      # standardize macro state variables
      for (j in 1:ncol(Zw)) {
        mb[, j] = (Zw[, j] - min(Zw[, j]))/(max(Zw[, j]) - min(Zw[, j]))
      }
      mb[is.na(mb)] = 0
      Zw[is.na(Zw)] = 0
      xxw           = data[,2:nfirm]; 
      pp            = ncol(xxw);# the number of firms4
      explanvarw    = as.matrix(cbind(xxw,mb));# merge all explain variables
      qw            = rq.fit.lasso(explanvarw,yw, tau,lambda); #quantile regression + lasso
      beta_roll     = as.vector(coef(qw))
      # bootstrap method
         fity       = yw-(qw$residuals);
         redid      = as.vector(qw$residuals);
         mse.boot1  = replicate(k, expr = {
            bootsam = as.numeric(sample(redid, size = nrow(Zw), replace = TRUE)); #bootstrap sample process
            betaboot= as.vector(coef(rq.fit.lasso(explanvarw, (bootsam + fity), tau = tau, lambda)))
            betaboot
           })
        mse.boot = sum(apply(mse.boot1,1,sd)^2) + sum((apply(mse.boot1,1,mean)- beta_roll)^2);
        list(mseboot = mse.boot,beta_roll = beta_roll)
     }