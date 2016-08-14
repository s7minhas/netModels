rm(list=ls())
seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
source(paste0(funcPath, 'binPerfHelpers.R'))
loadPkg(c('reshape2', 'separationplot','ergm','latentnet','sna'))

# load data
load(paste0(dataPath, 'latFacData.rda'))

################################################
# Logit Mod
load( paste0(resultsPath, 'logitResults.rda') )
logitModProbs = 1/(1+exp(-predict(model.logit)))
logitPred = data.frame(
	actual=logit.data$collab,prob=logitModProbs,
	row=logit.data$rows,col=logit.data$cols)
logitPred = logitPred[logitPred$row != logitPred$col,]

getAUC(logitPred$prob, logitPred$actual)
auc_pr(logitPred$actual, logitPred$prob)
# separationplot(logitPred$prob, logitPred$actual)
################################################

################################################
# ERGM
load(paste0(resultsPath, 'ergmResults.rda'))
probs = Reduce('+', lapply(ergmSims, as.sociomatrix) ) / length(ergmSims)
rownames(probs) = colnames(probs) = char(1:34)
ergmPred = melt(probs)
names(ergmPred) = c('row', 'col', 'prob')
ergmPred$actual = logit.data$collab
ergmPred = ergmPred[ergmPred$row != ergmPred$col,]

getAUC(ergmPred$prob, ergmPred$actual)
auc_pr(ergmPred$actual, ergmPred$prob)
################################################

################################################
# Latent space - eucl
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
probs = 1/(1+exp(-predict(model.ls)))
rownames(probs) = colnames(probs) = char(1:34)

lsEuclPred = melt(probs)
names(lsEuclPred) = c('row', 'col', 'prob')
lsEuclPred$actual = logit.data$collab
lsEuclPred = lsEuclPred[lsEuclPred$row != lsEuclPred$col,]

getAUC(lsEuclPred$prob, lsEuclPred$actual)
auc_pr(lsEuclPred$actual, lsEuclPred$prob)
################################################

################################################
# QAP
load(paste0(resultsPath, 'qapResults.rda'))

coef(model.qap)

probs = predict(model.qap)
################################################