rm(list=ls())
library(amen)
library(ggplot2)
library(foreach)
library(doParallel)


n<-100
rho<-.5
mu<- -2

BFIT0<-NFIT0<-BFIT1<-NFIT1<-NFITO<-BFITO<-NULL
for(sim in 1:10)
{ 
  set.seed(sim) 
  X<-matrix(rnorm(n*2),n,2)  # multiply X here by some matrix to make X[,1] and X[,2]  correlated
  X1<-tcrossprod(X[,1]) 
  X2<-tcrossprod(X[,2])
  X12<-array(dim=c(n,n,2)) ; X12[,,1]<-X1 ; X12[,,2]<-X2

  Z<- mu + X1 + X2 + matrix(rnorm(n*n),n,n) 
  Y<- 1*(Z>0) 

  #### parallelize here
  ### params to foreach will be designarray, R, and model
  ### save only BETA[,2] vec from each AME run to save space
  nfit1<-ame(Z,X1,R=1,rvar=FALSE,cvar=FALSE,dcor=FALSE,print=FALSE,plot=FALSE)  
  nfit0<-ame(Z,X1,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,print=FALSE,plot=FALSE) 
  nfitO<-ame(Z,X12,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,print=FALSE,plot=FALSE)     

  bfit1<-ame(Y,X1,R=1,rvar=FALSE,cvar=FALSE,dcor=FALSE,print=FALSE,plot=FALSE,model="bin") 
  bfit0<-ame(Y,X1,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,print=FALSE,plot=FALSE,model="bin") 
  bfitO<-ame(Y,X12,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,print=FALSE,plot=FALSE,model="bin")     

  # organize following into df output, can just turn the code here into an fn
  # that takes in the BETA[,2] vec from above
  BFIT0<-rbind(BFIT0,mean(bfit0$BETA[,2])+sd(bfit0$BETA[,2])*qnorm(c(.025,.5,.975)))
  NFIT0<-rbind(NFIT0,mean(nfit0$BETA[,2])+sd(nfit0$BETA[,2])*qnorm(c(.025,.5,.975)))

  BFIT1<-rbind(BFIT1,mean(bfit1$BETA[,2])+sd(bfit1$BETA[,2])*qnorm(c(.025,.5,.975)))
  NFIT1<-rbind(NFIT1,mean(nfit1$BETA[,2])+sd(nfit1$BETA[,2])*qnorm(c(.025,.5,.975)))

  BFITO<-rbind(BFITO,mean(bfitO$BETA[,2])+sd(bfitO$BETA[,2])*qnorm(c(.025,.5,.975)))
  NFITO<-rbind(NFITO,mean(nfitO$BETA[,2])+sd(nfitO$BETA[,2])*qnorm(c(.025,.5,.975)))

  cat(sim,"\n") 
}

 

### Results from checking for omitted variable bias. 
### For binary fits, AME with R=1 (BFIT1) doeas as well as 
### the "oracle" fit (BFITO) that includes both variables. 
### A probit fit (BFIT0) fails.  

### For Gaussian/normal fits (NFITX), the estimates are all 
### pretty good, and seem unbiased. This would presumably 
### change if X[,1] and X[,2] were correlated. 

### Another thing to check is the calibration of the standard errors. 
### Do the posterior intervals contain the true parameter 
### 1-\alpha percent of the time? To evaluate this accurately, 
### would need to do a lot of simulations.