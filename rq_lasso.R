  rq_lasso = function(data,tau,lambda){
      yw        =  data[,1]
      Zw        =  data[,21:23];
      mb        =  matrix(0,nrow(Zw), ncol(Zw))
      # standardize macro state variables
      for (j in 1:ncol(Zw)) {
        mb[, j] = (Zw[, j] - min(Zw[, j]))/(max(Zw[, j]) - min(Zw[, j]))
      }
      mb[is.na(mb)] = 0
      Zw[is.na(Zw)] = 0
      xxw           = data[,2:20]; 
      pp            = ncol(xxw);# the number of firms4
      explanvarw    = as.matrix(cbind(xxw,mb));# merge all explan variables
      qw            = rq.fit.lasso(explanvarw,yw, tau,lambda); #quantile regression + lasso
      beta_roll     = as.vector(coef(qw))
      list(beta_roll = beta_roll)
     }