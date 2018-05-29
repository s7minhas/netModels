rm(list=ls())
source('helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
source(paste0(funcPath, 'binPerfHelpers.R'))
loadPkg(c('png','grid'))

################################################
# load mod results
if( !file.exists( paste0(resultsPath,'predData_outSample_amen.rda') ) ){
	latDims=1; load(paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda')) # ame
	amePred1 = do.call('rbind', lapply(modsAme, function(x){ x$pred })) ; rm(list='modsAme')

	latDims=2; load(paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda')) # ame
	amePred2 = do.call('rbind', lapply(modsAme, function(x){ x$pred })) ; rm(list='modsAme')

	latDims=3; load(paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda')) # ame
	amePred3 = do.call('rbind', lapply(modsAme, function(x){ x$pred })) ; rm(list='modsAme')

	latDims=4; load(paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda')) # ame
	amePred4 = do.call('rbind', lapply(modsAme, function(x){ x$pred })) ; rm(list='modsAme')

	# Organize pred DFs
	predDfs = list( 'AME (k=2)'=amePred2, 'AME (k=1)'=amePred1, 
		'AME (k=4)'=amePred4, 'AME (k=3)'=amePred3 )
	save(predDfs, file=paste0(resultsPath, 'predData_outSample_amen.rda'))
	} else {
	load( paste0(resultsPath, 'predData_outSample_amen.rda') )		
	}
################################################

################################################
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
	size='normalsize', file=paste0(graphicsPath, 'aucTable_ameSR_outSample.tex') )

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
	fSepPath = paste0(graphicsPath,'sep_',names(predDfs)[ii],'_outSample.png')
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
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.35,.1), label=names(predDfs))
ggsave(tmp, file=paste0(graphicsPath, 'roc_ameSR_outSample.pdf'), width=5, height=5, device=cairo_pdf)

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty) +
	guides(linetype=FALSE, color=FALSE) + 
	# geom_rect(xmin=.05, ymin=.01, xmax=.58, ymax=.55, color='white', fill='white', size=.5) + 
	annotate('text', hjust=0, x=c(.01, .29, .47), y=.45, 
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), size=4) + 
	annotate('text', hjust=0, x=.01, y=seq(.05, .35, .1), 
		label=rev(apply(cbind(rownames(aucSumm), aucSumm), 1, function(x){paste(x, collapse='     ')})) )
ggsave(tmp, file=paste0(graphicsPath, 'rocPr_ameSR_outSample.pdf'), width=5, height=5, device=cairo_pdf)
################################################