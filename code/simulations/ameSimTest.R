######
rm(list=ls())
library(amen)
library(foreach)
library(doParallel)
######

######
dPath = '~/Dropbox/Research/netModels/'
dataPath = paste0(dPath, "data/")
resultsPath = paste0(dPath, 'rdaResults/')
graphicsPath = paste0(dPath, 'summResults/')
######

######
# net params
n<-100
rho<-.5
mu<- -2

# sim params
cores=8
sims=100

# output objects
BFIT0<-NFIT0<-BFIT1<-NFIT1<-NFITO<-BFITO<-matrix(NA, nrow=sims, ncol=3, dimnames=list(NULL,c('lo','med','hi')))
xCor = vector(mode='numeric', length=sims)
######

######
for(sim in 1:sims) { 
  #
  set.seed(sim) 
  
  # design array
  X<-matrix(rnorm(n*2),n,2)  # multiply X here by some matrix to make X[,1] and X[,2]  correlated
  # r = .8 ; X = MASS::mvrnorm(n=n, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)  
  X1<-tcrossprod(X[,1]) 
  X2<-tcrossprod(X[,2])
  X12<-array(dim=c(n,n,2)) ; X12[,,1]<-X1 ; X12[,,2]<-X2

  # dv
  Z<- mu + X1 + X2 + matrix(rnorm(n*n),n,n) 
  Y<- 1*(Z>0) 

  # set up param list for parallelization
  paramList = list(
    nfit1 = list(y=Z, x=X1, R=1, model='nrm'),
    nfit0 = list(y=Z, x=X1, R=0, model='nrm'),
    nfitO = list(y=Z, x=X12, R=0, model='nrm'),
    bfit1 = list(y=Y, x=X1, R=1, model='bin'),
    bfit0 = list(y=Y, x=X1, R=0, model='bin'),
    bfitO = list(y=Y, x=X12, R=0, model='bin')
    )

  # prlz
  cl=makeCluster(cores) ; registerDoParallel(cl)
  res = foreach(ii=1:length(paramList), .packages=c("amen")) %dopar% {
    p = paramList[[ii]]
    fit = ame(
      p$y, p$x, R=p$R, model=p$model, # params
      rvar=FALSE,cvar=FALSE,dcor=FALSE, # no SRM stuff
      print=FALSE,plot=FALSE,gof=FALSE, # no unnecessary calcs
      nscan=10000, burn=5000, odens=25
      )
    beta = mean(fit$BETA[,2])+sd(fit$BETA[,2])*qnorm(c(.025,.5,.975)) # beta hpd
    names(beta) = c('lo','med','hi')
    return(beta)
  }
  stopCluster(cl)
  names(res) = names(paramList)

  # org output
  NFIT1[sim,] = res$nfit1
  NFIT0[sim,] = res$nfit0
  NFITO[sim,] = res$nfitO
  BFIT1[sim,] = res$bfit1
  BFIT0[sim,] = res$bfit0
  BFITO[sim,] = res$bfitO
  xCor[sim] = cor(c(X1), c(X2))

  #
  cat(sim,"\n") 
}
######

######
save(
  NFIT1, NFIT0, NFITO,
  BFIT1, BFIT0, BFITO,
  xCor, 
  file=paste0(resultsPath, 'ameSimTest_100.rda')
  )
######