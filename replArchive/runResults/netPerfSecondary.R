rm(list=ls())
source('../helpers/paths.R')
source(paste0(funcPath, 'functions.R')) ; source(paste0(funcPath, 'netPerfHelpers.R'))
loadPkg( c( 'devtools', 'latentnet' ) ) ; devtools::install_github('s7minhas/amen') ; library(amen)

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# actual values
actVals = gofstats(Y)
perfNetKey = cbind(v=names(actVals), 
	c=c('Sender variation', 'Receiver variation', 'Dyadic dependency', 'Triadic dependency') )
################################################

################################################
# ERGM
load(paste0(resultsPath, 'ergmResults.rda'))
ergmPerf = do.call('rbind', lapply(ergmSims, function(x){
	gofstats( as.sociomatrix( x ) ) }))

# AME
load(paste0(resultsPath, 'ameFitSR_2.rda'))
amePerf = ameFit$'GOF'[-1,]

# LS - EUCL
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
lsEuclSim = simulate.ergmm(model.ls, nsim=100)
lsEuclPerf = do.call('rbind', lapply(lsEuclSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# org
perfList = list(AME=amePerf, ERGM=ergmPerf, LSM=lsEuclPerf)
################################################

################################################
# Viz 
getNetPerfCoef(perfList, perfNetKey, actVals, pRows=1, save=TRUE, fPath=paste0(graphicsPath, 'netPerfCoef.pdf'))
getNetPerfDist(perfList, perfNetKey, actVals, save=TRUE, fPath=paste0(graphicsPath, 'netPerfDist.pdf'))
################################################