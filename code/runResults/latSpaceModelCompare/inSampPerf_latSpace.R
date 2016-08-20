rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
source(paste0(funcPath, 'binPerfHelpers.R'))
loadPkg(c('latentnet','sna','png','grid'))

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# Latent space - eucl
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
probs = predict(model.ls)
rownames(probs) = colnames(probs) = char(1:34)
lsEuclPred = melt(probs)
names(lsEuclPred) = c('row', 'col', 'prob')
lsEuclPred$actual = logit.data$collab
lsEuclPred = lsEuclPred[lsEuclPred$row != lsEuclPred$col,]
################################################

################################################
# Latent space - eucl sr
probs = predict(model.lsSR)
rownames(probs) = colnames(probs) = char(1:34)
lsEuclPredSR = melt(probs)
names(lsEuclPredSR) = c('row', 'col', 'prob')
lsEuclPredSR$actual = logit.data$collab
lsEuclPredSR = lsEuclPredSR[lsEuclPredSR$row != lsEuclPredSR$col,]
################################################

################################################
# Latent space - bilinear
load(paste0(resultsPath, 'bilLatSpaceResults.rda'))
probs = predict(model.lsBil)
rownames(probs) = colnames(probs) = char(1:34)
lsBilPred = melt(probs)
names(lsBilPred) = c('row', 'col', 'prob')
lsBilPred$actual = logit.data$collab
lsBilPred = lsBilPred[lsBilPred$row != lsBilPred$col,]
################################################

################################################
# Latent space - bilinear sr
probs = predict(model.lsBilSR)
rownames(probs) = colnames(probs) = char(1:34)
lsBilPredSR = melt(probs)
names(lsBilPredSR) = c('row', 'col', 'prob')
lsBilPredSR$actual = logit.data$collab
lsBilPredSR = lsBilPredSR[lsBilPredSR$row != lsBilPredSR$col,]
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
################################################

################################################
# Organize pred DFs
predDfs = list(
	LSM=lsEuclPred,	'LSM (Bilinear)'=lsBilPred, 
	'LSM (SR)'=lsEuclPredSR, 'LSM (Bilinear + SR)'=lsBilPredSR,
	'AME'=amePred
	)

# get auc summary
aucSumm = do.call('rbind', 
	lapply(predDfs, function(x){ 
		cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) ) 
		} ) ) ; rownames(aucSumm) = names(predDfs)
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))

# Write out tex
print.xtable( xtable(aucSumm, align='lcc', 
		caption='Area under the curve (AUC) comparison for latent space approaches.', label='tab:aucTable_latSpace'), 
	include.rownames=TRUE, sanitize.text.function=identity,
	hline.after=c(0, 0, nrow(frame), nrow(frame)),
	size='normalsize', file=paste0(graphicsPath, 'aucTable_latSpace.tex') )

# Roc Plot
rocData = lapply(1:length(predDfs), function(ii){
	r = roc(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocData = do.call('rbind', rocData)

# model col/lty
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')
ggLty = c('dashed', 'dotdash', 'dotted', 'twodash', 'solid')

# Separation plots
sepPngList = lapply(1:length(predDfs), function(ii){
	fSepPath = paste0(graphicsPath,'sep_',names(predDfs)[ii],'.png')
	# save as pngs for potential use outside of roc
	ggSep(actual=predDfs[[ii]]$'actual', proba=predDfs[[ii]]$'prob', 
		color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
	sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
	return(sepG)
})

tmp = rocPlot(rocData, linetypes=ggLty)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
	yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1), label=names(predDfs), family="Source Sans Pro Light")
ggsave(tmp, file=paste0(graphicsPath, 'roc_latSpace.pdf'), width=5, height=5, device=cairo_pdf)

# area under precision-recall curves (Beger 2016 [arxiv])
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty) +
	# guides(linetype=guide_legend(reverse=TRUE), color=guide_legend(reverse=TRUE))
	guides(linetype=FALSE, color=FALSE)
ggsave(tmp, file=paste0(graphicsPath, 'rocPr_latSpace.pdf'), width=5, height=5, device=cairo_pdf)
################################################