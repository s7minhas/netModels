rm(list=ls())
source('~/Research/netModels/code/replicationSetup.R')
# install amen from s7minhas/github
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
loadPkg( c( 'ergm', 'latentnet', 'btergm', 'lme4' ) )

# load data
load(paste0(dataPath, 'data.rda'))

# make basic ame gof
ameSRFileNames = paste0('ameFitSR_',0:4,'.rda') ; lapply(ameSRFileNames, getAmeNetPerf)
################################################

################################################
# actual values
actSumm = getNetStats(Y)
actData = lapply(1:length(actSumm), function(ii){ x=data.frame(value=actSumm[[ii]]) ; x$obs = rownames(x) ; return(x) })
names(actData) = names(actSumm)
################################################

################################################
# Amen
ameSRGofDist = lapply(ameSRFileNames, getAmeGOF)
names(ameSRGofDist) = c('AME (k=0)', 'AME (k=1)','AME (k=2)','AME (k=3)','AME (k=4)')
################################################

################################################
# create gof data
ggData = getGofData(ameSRGofDist, actData)

# plot
ggGof( ggData, pRows=length(unique(ggData$var)), pCols=1, modSpace=.6, 
	save=TRUE, fPath=paste0(graphicsPath, 'ggGofAll_ameSR.pdf'), fWidth=12, fHeight=16)
################################################