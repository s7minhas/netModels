rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
source(paste0(funcPath, 'binPerfHelpers.R'))
loadPkg(c('png','grid'))

################################################
# load mod results
if( !file.exists( paste0(graphicsPath,'predData_outSample.rda') ) ){
	load(paste0(resultsPath, 'logitOutPerfResults.rda')) # logit
	logitPred = do.call('rbind', lapply(modsLogit, function(x){ x$pred })) ; rm(list='modsLogit')

	load(paste0(resultsPath, 'qapOutPerfResults.rda')) # qap
	qapPred = do.call('rbind', lapply(modsQap, function(x){ x$pred })) ; rm(list='modsQap')

	load(paste0(resultsPath, 'ergmOutPerfResults.rda')) # ergm
	ergmPred = do.call('rbind', lapply(modsErgm, function(x){ x$pred })) ; rm(list='modsErgm')

	load(paste0(resultsPath, 'euclLatSpaceOutPerfResults.rda')) # ls eucl
	lsEuclPred = do.call('rbind', lapply(modsLs, function(x){ x$pred })) ; rm(list='modsLs')

	latDims=2; load(paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda')) # ame
	amePred = do.call('rbind', lapply(modsAme, function(x){ x$pred })) ; rm(list='modsAme')

	# Organize pred DFs
	predDfs = list( LSM=lsEuclPred, MRQAP=qapPred, Logit=logitPred, 
		ERGM=ergmPred, AME=amePred )
	save(predDfs, file=paste0(graphicsPath,'predData_outSample.rda'))	
	} else {
	load(paste0(graphicsPath,'predData_outSample.rda'))
	}
################################################

graphicsPath='~/Research/netModels/paper/toSubmit/'
predDfs = predDfs[c('LSM','ERGM','AME')]

################################################
# get auc summary
aucSumm = do.call('rbind', 
	lapply(predDfs, function(x){ 
		cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) ) 
		} ) ) ; rownames(aucSumm) = names(predDfs)
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))
 
# # Write out tex
# print.xtable( xtable(aucSumm, align='lcc', 
# 		caption='Area under the curve (AUC) comparison.', label='tab:aucTable'), 
# 	include.rownames=TRUE, sanitize.text.function=identity,
# 	hline.after=c(0, 0, nrow(frame), nrow(frame)),
# 	size='normalsize', file=paste0(graphicsPath, 'aucTable_outSample.tex') )

# Roc Plot
rocData = lapply(1:length(predDfs), function(ii){
	r = roc(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocData = do.call('rbind', rocData)

# model col/lty
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[c(3,2,1)]
ggLty = c('dashed', 'dotdash', 'dotted', 'twodash', 'solid')[c(2,4,5)]

# Separation plots
sepPngList = lapply(1:length(predDfs), function(ii){
	fSepPath = paste0(graphicsPath,'sep_',names(predDfs)[ii],'_outSampleSmall.png')
	# save as pngs for potential use outside of roc
	ggSep(actual=predDfs[[ii]]$'actual', proba=predDfs[[ii]]$'prob', 
		color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
	sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
	return(sepG)
})

tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
	yLo = yLo + .1 ; yHi = yHi + .1 }
# tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1), label=names(predDfs), family="Source Sans Pro Light")
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1)[1:3], label=names(predDfs), family="Source Sans Pro Light")
ggsave(tmp, file=paste0(graphicsPath, 'roc_outSampleSmall.pdf'), width=5, height=5, device=cairo_pdf)

# area under precision-recall curves (Beger 2016 [arxiv])
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
	guides(linetype=FALSE, color=FALSE) + 
	# geom_rect(xmin=-.05, ymin=.01, xmax=.45, ymax=.55, color='white', fill='white', size=.5) + 
	# annotate('text', hjust=0, x=c(-.1, .09, .28), y=.55, 
	annotate('text', hjust=0, x=c(-.1, .09, .28), y=.35, 		
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), family='Source Sans Pro Black', size=4) + 
	# annotate('text', hjust=0, x=-.1, y=seq(.05, .45, .1), 
	annotate('text', hjust=0, x=-.1, y=seq(.05, .45, .1)[1:3], 		
		label=rev(rownames(aucSumm)), family='Source Sans Pro Light') + 
	# annotate('text', hjust=0, x=.1, y=seq(.05, .45, .1), 
	annotate('text', hjust=0, x=.1, y=seq(.05, .45, .1)[1:3], 		
		label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')})),
		family='Source Sans Pro Light')
ggsave(tmp, file=paste0(graphicsPath, 'rocPr_outSampleSmall.pdf'), width=5, height=5, device=cairo_pdf)
################################################