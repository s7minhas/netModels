rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
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
# AME
ameSRFileNames = paste0(resultsPath, 'ameFitSR_',1:4,'.rda')
perfListSR = lapply(ameSRFileNames, function(x){ load(x) ; ameFit$'GOF'[-1,] })
names(perfListSR) = c( 'AME (k=1)','AME (k=2)','AME (k=3)','AME (k=4)')
################################################

################################################
# Viz 
getNetPerfCoef(perfListSR, perfNetKey, actVals, pRows=1, save=TRUE, fPath=paste0(graphicsPath, 'netPerfCoef_ameSR.pdf'))
getNetPerfDist(perfListSR, perfNetKey, actVals, pRows=2, save=TRUE, fPath=paste0(graphicsPath, 'netPerfDist_ameSR.pdf'))
################################################