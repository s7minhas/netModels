#set up workspace ################### 
rm(list=ls())

seed <- 12345
set.seed(seed)
# # example linux path
# mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/dataverseRepl/' 
resultsPath = paste0(mainPath, 'results/')
dataPath = paste0(mainPath, 'data/')
graphicsPath = paste0(mainPath, 'appendix_results_floats/')

# install lme4 if not there
if(!'lme4' %in% installed.packages()){
  install.packages('lme4', repos="https://cloud.r-project.org") }

# load libraries
pkgs = c(
    'ergm', 'latentnet', 'btergm', 'lme4',
    'reshape2','plyr','ggplot2','latex2exp',
    'xtable','ROCR','caTools','RColorBrewer','png','grid')
shh=lapply(pkgs, library, character.only=TRUE)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
source(paste0(mainPath, 'netPerfHelpers.R'))

# load data
load(paste0(mainPath, 'data/data.rda'))
####################

################################################
# actual values
actSumm = getNetStats(Y)
actData = lapply(1:length(actSumm), function(ii){ x=data.frame(value=actSumm[[ii]]) ; x$obs = rownames(x) ; return(x) })
names(actData) = names(actSumm)
################################################

################################################
# gof names
gofNames = c(
	"Dyad-wise shared partners", "Edge-wise shared partners", 
	"Geodesic distances", "Indegree", "Outdegree", "Incoming k-star")
gofMethods = c(
	btergm::dsp, btergm::esp, 
	btergm::geodesic, btergm::ideg, btergm::odeg, btergm::istar)
getProp = function(x){ sweep(x,2,colSums(x),'/') }

# ERGM
load(paste0(resultsPath, 'ergmEst.rda'))
gof.ergm <- gof(model.ergm, MCMC.burnin = 30000, MCMC.interval = 10000, 
    statistics = c(dsp, esp, geodesic, ideg, odeg, istar))
ergmDist = lapply(gof.ergm, function(x){ t(data.matrix( getProp(x$'raw') )) })

# Latent space - eucl
load(paste0(resultsPath, 'lsmEst.rda'))
gof.ls <- gof.ergmm(model.ls, 
    GOF = ~ dspartners + espartners + distance + idegree + odegree, 
    control = control.gof.ergmm(seed = seed))
lsEuclDist = getLsGof(gofObject=gof.ls)

# Amen
load(paste0(resultsPath, 'ameEst.rda'))
ameDist = getAmeGOF(ameFit, gofNames, gofMethods)

# org
gofDist = list(ERGM=ergmDist, LSM=lsEuclDist, AME=ameDist)
################################################

################################################
# create gof df
ggData = getGofData(gofDist, actData)

# plot
ggGof( ggData, 
    pRows=length(unique(ggData$var)), pCols=1, modSpace=.6, 
	save=TRUE, 
    fPath=paste0(graphicsPath, 'FigureA3.pdf'), 
    fWidth=12, fHeight=16)
################################################