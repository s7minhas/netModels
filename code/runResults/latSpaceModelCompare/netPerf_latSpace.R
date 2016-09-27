rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R')) ; source(paste0(funcPath, 'netPerfHelpers.R'))
loadPkg( c( 'ergm', 'latentnet', 'btergm', 'lme4' ) )

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# actual values
actSumm = getNetStats(Y)
actData = lapply(1:length(actSumm), function(ii){ x=data.frame(value=actSumm[[ii]]) ; x$obs = rownames(x) ; return(x) })
names(actData) = names(actSumm)
################################################

################################################
# load binaries
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
load(paste0(resultsPath, 'bilLatSpaceResults.rda'))
load(paste0(resultsPath, 'ameFitSR_2.rda'))

# Latent space - eucl
lsEuclDist = getLsGof(gofObject=gof.ls)
lsEuclDistSR = getLsGof(gofObject=gof.lsSR)
lsBilDist = getLsGof(gofObject=gof.lsBil)
lsBilDistSR = getLsGof(gofObject=gof.lsBilSR)

# Amen
ameDist = getAmeGOF('ameFitSR_2.rda')

# org
gofDist = list(
	LSM=lsEuclDist, 'LSM (Bilinear)'=lsBilDist, 'LSM (SR)'=lsEuclDistSR, 
	'LSM (Bilinear + SR)'=lsBilDistSR, AME=ameDist)
################################################

################################################
# create gof df
ggData = getGofData(gofDist, actData)

# limit to network related approaches and to net var that include all models
varsToKeep = unique(ggData$var) ; ggData = ggData[which(ggData$var %in% varsToKeep[-6]),]

# plot
ggGof( ggData, pRows=length(varsToKeep), pCols=1, modSpace=.6, 
	save=TRUE, fPath=paste0(graphicsPath, 'ggGofAll_latSpace.pdf'), fWidth=12, fHeight=16)
################################################