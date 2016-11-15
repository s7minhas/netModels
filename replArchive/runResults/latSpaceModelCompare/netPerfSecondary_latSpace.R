rm(list=ls())
source('../../helpers/paths.R')
source(paste0(funcPath, 'functions.R')) ; source(paste0(funcPath, 'netPerfHelpers.R'))
loadPkg( c( 'devtools' ) ) ; devtools::install_github('s7minhas/amen') ; library(amen)

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# actual values
actVals = gofstats(Y)
perfNetKey = cbind(v=names(actVals), 
	c=c('Sender variation', 'Receiver variation', 'Dyadic dependency', 'Triadic dependency') )
################################################

################################################
# load rel rdas
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
load(paste0(resultsPath, 'bilLatSpaceResults.rda'))

# LS - EUCL
lsEuclSim = simulate.ergmm(model.ls, nsim=100)
lsEuclPerf = do.call('rbind', lapply(lsEuclSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# LS - EUCL SR
lsEuclSRSim = simulate.ergmm(model.lsSR, nsim=100)
lsEuclSRPerf = do.call('rbind', lapply(lsEuclSRSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# LS - BIL
lsBilSim = simulate.ergmm(model.lsBil, nsim=100)
lsBilPerf = do.call('rbind', lapply(lsBilSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# LS - BIL SR
lsBilSRSim = simulate.ergmm(model.lsBilSR, nsim=100)
lsBilSRPerf = do.call('rbind', lapply(lsBilSRSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# AME
load(paste0(resultsPath, 'ameFitSR_2.rda'))
amePerf = ameFit$'GOF'[-1,]

# org
perfList = list(
	LSM=lsEuclPerf, 'LSM (Bilinear)'=lsBilPerf, 'LSM (SR)'=lsEuclSRPerf, 
	'LSM (Bilinear + SR)'=lsBilSRPerf, AME=amePerf
	)
################################################

################################################
# Viz 
getNetPerfCoef(perfList, perfNetKey, actVals, pRows=1, save=TRUE, fPath=paste0(graphicsPath, 'netPerfCoef_latSpace.pdf'))
getNetPerfDist(perfList, perfNetKey, actVals, pRows=2, save=TRUE, fPath=paste0(graphicsPath, 'netPerfDist_latSpace.pdf'))
################################################