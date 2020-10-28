
README:

Name of MAWT_Method

Published in: MAWTROLL

Descirption: 'the main code for the simulation section. In this part, we simulate four data
generate processing,replicating 200 times. We use MAWT algorithm to calculate the solution of
discrete bi-objective problems,that is, MSE and hetergeneity.'


Keywords: 'simulation, MAWT algorithm, MSE (mean square error), Euclidean distance'

See also: 'MAWT.sim, full, roll_ws_sim, roll_sim, myFUN.roll.sim, MAWT.PRE.sim'

Author: Xingmin Zhan

R Code

# Step1: simulate different data generating processes
# model setting: y = x*beta + eps

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
#set the working directory
# setws('C:/...')
# innstall and load packages
libraries = c("bootstrap","zoo")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
   install.packages(x)
 })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
 
source("MAWT.sim.r");
source("full.r");
source("roll_ws_sim.r") 
source("roll_sim.r") 
source("myFUN.roll.sim.r") 
source("MAWT.PRE.sim.r") 


#  Step1. simulate different data generating processes
# DGP1: y(t) = 0.3*x1(t) + 0.7*x2(t)
# DGP2: y(t) = 1*x1(t) + (5-7.9*(t/T))*x2(t)
# DGP3: y(t) = 1*x1(t) + (5-7.9*(t/T)^2)*x2(t)
# DGP4: y(t) = 1*x1(t) + b(t)*x2(t), b(t)=b(t-1)+0.1/sqrt(T)*u(t), b(0)=0.7

# the number of observations in our empricial part
n = 200;
# repeated times
m = 200; 

# DGP1- constant parameter, a = 0.3, b = 0.7
# true paramter setting
betatrue_dgp1  = cbind(rep(0.3,n),rep(0.7,n));
# generate m times sample. dependent variable: y, independent variables: x11,x22
x11_dgp1   = matrix(0,n,m);
x22_dgp1   = matrix(0,n,m);
e_dgp1     = matrix(0,n,m);
yy_dgp1    = matrix(0,n,m);
for (i in 1:m){
  x11_dgp1[,i] = rnorm(n);
  x22_dgp1[,i] = rnorm(n);        	
  e_dgp1[,i]  =rnorm(n);
  X_dgp1 = cbind(x11_dgp1[,i],x22_dgp1[,i]);
  for(t in 1:n){
    yy_dgp1[t,i] = betatrue_dgp1[t,1]*X_dgp1[t,1]+betatrue_dgp1[t,2]*X_dgp1[t,2]+e_dgp1[t,i];
  }
}

# DGP2, time-varying linear parameter
# true parameter
betatrue_dgp2  = matrix(0,n,2);
for(t in 1:n){
  betatrue_dgp2[t,]  = c(1,(5-(7.9*t)/n));
}
write.csv(betatrue_dgp2, file="betatrue_DGP2.csv")
# generate m times sample. dependent variable: y, independent variables: x11,x22 
x11_dgp2     = matrix(0,n,m);
x22_dgp2    = matrix(0,n,m); 
e_dgp2     = matrix(0,n,m);
yy_dgp2    = matrix(0,n,m);
for (i in 1:m){
  x11_dgp2[,i] = rnorm(n,mean=0,sd=1);
  x22_dgp2[,i] = rnorm(n,mean=0,sd=1);        	
  e_dgp2[,i]  =rnorm(n,mean=0,sd=1);
  X_dgp2 = cbind(x11_dgp2[,i],x22_dgp2[,i]);
  for(t in 1:n){
    yy_dgp2[t,i] = betatrue_dgp2[t,1]*X_dgp2[t,1]+betatrue_dgp2[t,2]*X_dgp2[t,2]+e_dgp2[t,i];
  }
}

# DGP3. time-varying nonlinear parameter
# true parameter
betatrue_dgp3  = matrix(0,n,2);
for(t in 1:n){
  betatrue_dgp3[t,] = c(1,(5-(7.9*(t/n)^2)));
}
write.csv(betatrue_dgp3, file="betatrue_DGP3.csv")
##generate m times sample.dependent variable: y, independent variables: x11,x22  
x11_dgp3     = matrix(0,n,m);
x22_dgp3    = matrix(0,n,m);
e_dgp3     = matrix(0,n,m);
yy_dgp3    = matrix(0,n,m);
for (i in 1:m){
  x11_dgp3[,i] = rnorm(n);
  x22_dgp3[,i] = rnorm(n);        	
  e_dgp3[,i]  =rnorm(n);
  X_dgp3 = cbind(x11_dgp3[,i],x22_dgp3[,i]);
  for(t in 1:n){
    yy_dgp3[t,i] = betatrue_dgp3[t,1]*X_dgp3[t,1]+betatrue_dgp3[t,2]*X_dgp3[t,2]+e_dgp3[t,i];
  }
}

# DGP4. time-varying random walk parameter
# true parameter
betatrue_dgp4  = matrix(0,n,2);
u         = rnorm(n);
tt        = (n)^(1/2);
a         = as.vector(rep(1,n));
b         = vector();
b[1]      = 0.7;
t         = 1;
repeat{
  b[t+1]  = b[t]+(1/tt)*u[t+1];
  t       = t+1;
  if(t>=n) break
}
betatrue_dgp4   = cbind(a,b)
write.csv(betatrue_dgp4, file="betatrue_DGP4.csv")
##generate m times sample.dependent variable: y, independent variables: x11,x22 
x11_dgp4   = matrix(0,n,m);
x22_dgp4   = matrix(0,n,m);
e_dgp4     = matrix(0,n,m);
yy_dgp4    = matrix(0,n,m);
for (i in 1:m){
  x11_dgp4[,i] = rnorm(n, mean = 0, sd = 1);
  x22_dgp4[,i] = rnorm(n, mean = 0, sd = 1);        	
  e_dgp4[,i]   = rnorm(n);
  X_dgp4 = cbind(x11_dgp4[,i],x22_dgp4[,i]);
  for(t in 1:n){
    yy_dgp4[t,i] = betatrue_dgp4[t,1]*X_dgp4[t,1]+betatrue_dgp4[t,2]*X_dgp4[t,2]+e_dgp4[t,i];
  }
}

# merge all DGPs data
  yde = list(yy_dgp1 = yy_dgp1,yy_dgp2 = yy_dgp2,yy_dgp3 = yy_dgp3,yy_dgp4 = yy_dgp4)
  x1all =  list(x11_dgp1 = x11_dgp1,x11_dgp2 = x11_dgp2,x11_dgp3 = x11_dgp3,x11_dgp4 = x11_dgp4)
  x2all =  list(x22_dgp1 = x22_dgp1,x22_dgp2 = x22_dgp2,x22_dgp3 = x22_dgp3,x22_dgp4 = x22_dgp4)



# Step2. solve the optimal rolling window size for each DGP
# the rolling windows list
widths = data.frame(as.integer(max(1.5*(n)^(2/3),30)):(as.integer(min(3*(n)^(2/3),n-1))));
# the number of the rolling windows
wie    = c(as.integer(max(1.5*(n)^(2/3),30)):(as.integer(min(3*(n)^(2/3),n-1))));
ww     = length(wie);
# the bootstrap times
k = 500;
# the weights for two goals in the compromise method
lmda <- c(0.7,0.3)


# for each DGP, we run the following code to solve optimal rolling window size
# replicate m=200 times.
  for (i in 1:4){
    yy = yde[[i]]
    x11 = x1all[[i]];
    x22 = x2all[[i]];
    mawt.pre = MAWT.PRE.sim (Y = yy,xx1 = x11,xx2 = x22,n = n,
                        m = m,k = k,widths = widths,ww = ww,
                        s = 1,lmda = lmda);
# results
        mse.app.full  = mawt.pre$mse.app.f;# approximate full mse
        mse.true.full = mawt.pre$mse.true.f;#true full mse b
# method 1. knee point method
        method1     =  mawt.pre$method1;
        M1.MSER     =  sqrt(sum(method1[,1])/sum(method1[,3]));#  MSER = mse.app / mse.true;Eq.(20)
        M1.MSERF    =  sqrt(sum(method1[,1])/sum(mse.app.full));# MSERF = mse.app / mse.app.full;Eq.(21)
        M1.MSET     =  mean(method1[,3]);# the mean of true mse, Eq.(22)
        M1.MSETF    =  sqrt(sum(method1[,3])/sum(mse.true.full));# MSETF = mse.true.roll / mse.true.full;Eq.(23)
        M1.AHETER   =  -mean(method1[,2]);# AHETER=mean of tv,Eq.(24)
        # we use the negative sign of distance because we transform the 
        # maximum goal of heterogeneity as the minimum goal when we solve the bi-objective problems
# method 2. compromise method
        method2     = mawt.pre$method2;
        M2.MSER     =  sqrt(sum(method2[,1])/sum(method2[,3]));#  MSER = mse.app / mse.true; Eq.(20)
        M2.MSERF    =  sqrt(sum(method2[,1])/sum(mse.app.full));# MSERF = mse.app / mse.app.full; Eq.(21)
        M2.MSET     =  mean(method2[,3]);# the mean of true mse, Eq.(22)
        M2.MSETF    =  sqrt(sum(method2[,3])/sum(mse.true.full));# MSETF = mse.true.roll / mse.true.full;Eq.(23)
        M2.AHETER   =  -mean(method2[,2]);# AHETER=mean of tv,Eq.(24)
 
# merge the results
        method1.final  =  c(M1.MSER,M1.MSERF,M1.MSET,M1.MSETF,M1.AHETER);
        method2.final  =  c(M2.MSER,M2.MSERF,M2.MSET,M2.MSETF,M2.AHETER);

        final.results  =  matrix(t(cbind(method1.final, method2.final)),nrow = 2, ncol = 5, dimnames = list(c("M1","M2"),c("MSER","MSERF","MSET","MSETF","AHETER")) );
        final.results;
# write the final results
        write.csv(final.results, paste0("results_DGP_",i,".csv");
  }
      