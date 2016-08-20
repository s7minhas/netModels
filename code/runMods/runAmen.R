rm(list=ls())
seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))

# install amen from s7minhas/github
loadPkg('devtools')
devtools::install_github('s7minhas/amen')
library(amen)

# load data
load(paste0(dataPath, 'data.rda'))

# mcmc params
imps = 100000
brn = 50000
ods = 10
latDims = rep(c(2, 4, 6),2)
rcLogic = rep(c(TRUE,FALSE),3)

# Run amen in parallel
loadPkg(c('doParallel', 'foreach'))
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {

	ameFit = ame(Y=Y, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
		model='bin', symmetric=FALSE, R=latDims[ii],
		nscan=imps, seed=seed, burn=brn, odens=ods,
		rvar=rcLogic[ii], cvar=rcLogic[ii],
		plot=FALSE, print=FALSE)

	if(rcLogic){fPrefix = 'ameFitSR_'} else { fPrefix = 'ameFit_'  }	
	save(ameFit, file=paste0(resultsPath, fPrefix, latDims[ii], '.rda'))
}