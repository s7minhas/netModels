rm(list=ls())
seed <- 12345
set.seed(seed)
source('helpers/paths.R')
source(paste0(funcPath, 'functions.R'))

# install amen from s7minhas/github
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)

# load data
load(paste0(dataPath, 'data.rda'))

# mcmc params
imps = 100000
brn = 50000
ods = 10
latDims = 1:4

# Run amen in parallel
loadPkg(c('doParallel', 'foreach'))
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
	
	ameFit = ameTest(Y=Y, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
		model='bin', symmetric=FALSE, R=latDims[ii], 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		rvar=TRUE, cvar=TRUE,  # row and column effects, standard in srm
		plot=FALSE, print=FALSE) 
	
	save(ameFit, file=paste0(resultsPath, 'ameFitSR_', latDims[ii], '.rda'))
}
stopCluster(cl)