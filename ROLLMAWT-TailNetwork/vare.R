  vare = function(data2,q1,q2,nzw){
      yw        =  data2[,1]
      Zw        =  data2[,nzw];
      mb        =  matrix(0,nrow(Zw), ncol(Zw))
      # standardize macro state variables
      for (j in 1:ncol(Zw)) {
        mb[, j] = (Zw[, j] - min(Zw[, j]))/(max(Zw[, j]) - min(Zw[, j]))
      }
      mb[is.na(mb)] = 0
      Zw[is.na(Zw)] = 0
      qw1      = rq(yw~mb,tau = q1);
      qw2      = rq(yw~mb,tau = q2);
      var1     = as.vector(yw - resid(qw1));# 10%
      var2     = as.vector(yw - resid(qw2));# 50%
      list(var10 = var1, var50 = var2)
     }