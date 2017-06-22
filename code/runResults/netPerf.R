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
# gof names
gofNames = c("Dyad-wise shared partners", "Edge-wise shared partners", "Geodesic distances", "Indegree", "Outdegree", "Incoming k-star")
gofMethods = c(btergm::dsp, btergm::esp, btergm::geodesic, btergm::ideg, btergm::odeg, btergm::istar)
getProp = function(x){ sweep(x,2,colSums(x),'/') }

# Logit Mod
load(paste0(resultsPath, 'logitResults.rda'))
logitDist = lapply(gof.logit, function(x){ t(data.matrix( getProp(x$'raw') )) })

# ERGM
load(paste0(resultsPath, 'ergmResults.rda'))
ergmDist = lapply(gof.ergm, function(x){ t(data.matrix( getProp(x$'raw') )) })

# QAP
load(paste0(resultsPath, 'qapResults.rda'))
qapDist = lapply(gof.qap, function(x){ t(data.matrix( getProp(x$'raw') )) })

# Latent space - eucl
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
lsEuclDist = getLsGof(gofObject=gof.ls)

# Amen
ameDist = getAmeGOF('ameFitSR_2.rda')

# org
gofDist = list(Logit=logitDist, ERGM=ergmDist, MRQAP=qapDist, LSM=lsEuclDist, AME=ameDist)
################################################
names(gofDist)[4] = 'LDM'
################################################
# create gof df
ggData = getGofData(gofDist, actData)

# limit to network related approaches and to net var that include all models
modsToKeep = unique(ggData$model) ; ggData = ggData[which(ggData$model %in% modsToKeep[c(2,4,5)]),]

# plot
graphicsPath='~/Research/netModels/paper/toSubmit/'
ggGof( ggData, pRows=length(unique(ggData$var)), pCols=1, modSpace=.6, 
	save=TRUE, fPath=paste0(graphicsPath, 'ggGofAll.pdf'), fWidth=12, fHeight=16)

ggGof( ggData, pRows=length(unique(ggData$var)), pCols=2, modSpace=.6, 
	save=TRUE, fPath=paste0(graphicsPath, 'ggGofAll_preeze.pdf'), fWidth=16, fHeight=12)
################################################