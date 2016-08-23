rm(list=ls())
source('~/Research/netModels/code/replicationSetup.R')
# install amen from s7minhas/github
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
loadPkg( c( 'ergm', 'latentnet', 'btergm', 'lme4' ) )

# load data
load(paste0(dataPath, 'data.rda'))

# make basic ame gof
ameFileNames = paste0('ameFit_',0:4,'.rda') ; lapply(ameFileNames, getAmeNetPerf)
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
ameGofDist = lapply(ameFileNames, getAmeGOF)
names(ameGofDist) = c('AME (k=0)', 'AME (k=1)','AME (k=2)','AME (k=3)','AME (k=4)')

ameSRGofDist = lapply(ameSRFileNames, getAmeGOF)
names(ameSRGofDist) = c('AME SR (k=0)', 'AME SR (k=1)','AME SR (k=2)','AME SR (k=3)','AME SR (k=4)')
################################################

################################################
# create gof df
ggData = getGofData(ameGofDist, actData)

# limit to network related approaches and to net var that include all models
varsToKeep = unique(ggData$var) ; ggData = ggData[which(ggData$var %in% varsToKeep[-6]),]

# plot
ggGof( ggData, pRows=length(varsToKeep), pCols=1, modSpace=.6, 
	save=TRUE, fPath=paste0(graphicsPath, 'ggGofAll_ame.pdf'), fWidth=12, fHeight=16)

# create gof data
ggData = getGofData(ameSRGofDist, actData)

# limit to network related approaches and to net var that include all models
varsToKeep = unique(ggData$var) ; ggData = ggData[which(ggData$var %in% varsToKeep[-6]),]

# plot
ggGof( ggData, pRows=length(varsToKeep), pCols=1, modSpace=.6, 
	save=TRUE, fPath=paste0(graphicsPath, 'ggGofAll_ameSR.pdf'), fWidth=12, fHeight=16)
################################################