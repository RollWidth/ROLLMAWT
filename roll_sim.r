roll_sim = function(data,k){
        yw  = data[,1]
        xw  = data[,2:3];
        nw  = length(yw)
        bw  = lm(yw~xw);
        beta_roll = coef(bw)[2:3];
        var_b = c(vcov(bw)[2,2],vcov(bw)[3,3])         
        mse.true =  sum(var_b);
        # bootstrap method
        fity     = fitted(bw);
        redid_center = residuals(bw);
        mse.boot1 = replicate(k, expr = {
            bootsam = as.numeric(sample(redid_center, size = nw, replace = TRUE)); #bootstrap sample process
            betaboot = coef(lm((bootsam + fity)~xw))[2:3];
            betaboot
           })
        mse.boot <- sum(apply(mse.boot1,1,sd)^2)
        list(mse.true = mse.true, mse.boot = mse.boot, beta_roll = beta_roll)
}