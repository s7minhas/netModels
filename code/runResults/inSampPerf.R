rm(list=ls())
seed <- 12345; set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
source(paste0(funcPath, 'binPerfHelpers.R'))
loadPkg(c('reshape2', 'separationplot','ergm','latentnet','sna','xtable'))

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# Logit Mod
load(paste0(resultsPath, 'logitResults.rda'))
logitModProbs = 1/(1+exp(-predict(model.logit)))
logitPred = data.frame(
	actual=logit.data$collab,prob=logitModProbs,
	row=logit.data$rows,col=logit.data$cols)
logitPred = logitPred[logitPred$row != logitPred$col,]

aucLogit = getAUC(logitPred$prob, logitPred$actual)
aucPrLogit = auc_pr(logitPred$actual, logitPred$prob)
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

aucERGM = getAUC(ergmPred$prob, ergmPred$actual)
aucPrERGM = auc_pr(ergmPred$actual, ergmPred$prob)
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

aucLsEucl = getAUC(lsEuclPred$prob, lsEuclPred$actual)
aucPrLsEucl = auc_pr(lsEuclPred$actual, lsEuclPred$prob)
################################################

################################################
# QAP
load(paste0(resultsPath, 'qapResults.rda'))

qapPred=data.frame(row=lsEuclPred$row, col=lsEuclPred$col, actual=lsEuclPred$actual, prob=model.qap$'fitted.values')

aucQAP = getAUC(qapPred$prob, qapPred$actual)
aucPrQAP = auc_pr(qapPred$actual, qapPred$prob)
################################################

################################################
# Amen
load(paste0(resultsPath, 'ameFit_2.rda'))

rownames(Y) = colnames(Y) = char(1:34)
diag(Y) = NA; actual = melt(Y)

preds = ameFit$'EZ'
rownames(preds) = colnames(preds) = char(1:34)
diag(preds) = NA; amePred = melt(preds)

amePred$prob = 1/(1+exp(-amePred$value))
amePred$actual = actual$value
amePred = na.omit(amePred)

aucAme = getAUC(amePred$prob, amePred$actual)
aucPrAme = auc_pr(amePred$actual, amePred$prob)
################################################

################################################
# Summary table of AUCs
modNames = c('Logit','ERGM','Latent Space (Euclidean)', 'QAP', 'Latent Space (Bilinear)')
aucSumm = matrix(
	c(aucLogit,aucPrLogit,aucERGM,aucPrERGM,aucLsEucl,aucPrLsEucl,aucQAP,aucPrQAP,aucAme,aucPrAme),
	ncol=2,byrow=TRUE,
	dimnames=list(modNames,c('AUC', 'AUC (PR)'))
	)

# Roc Plot
predDfs = list(logitPred, ergmPred, lsEuclPred, qapPred, amePred)
rocData = lapply(1:length(predDfs), function(ii){
	r = roc(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual')
	p = cbind(r, model=modNames[ii])
	return(p) })
rocData = do.call('rbind', rocData)
rocPlot(rocData)

# area under precision-recall curves (Beger 2016 [arxiv])
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=modNames[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)
rocPlot(rocPrData, type='pr')

# Separation plots
pdf(file=paste0(graphicsPath, 'sepPlots.pdf'))
par(mfrow=c(3,2))
for(ii in 1:length(predDfs)){
	separationplot(pred=predDfs[[ii]]$'prob', actual=predDfs[[ii]]$'actual', newplot=FALSE)	
}
dev.off()	
################################################