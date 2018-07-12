#set up workspace ################### 
rm(list=ls())

seed <- 12345
set.seed(seed)
# example linux path
mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/Research/netModels/dataverseRepl/' 
resultsPath = paste0(mainPath, 'results/')
dataPath = paste0(mainPath, 'data/')
graphicsPath = paste0(mainPath, 'appendix_results_floats/')

# install lme4 if not there
if( any( !c('foreach','doParallel') %in% installed.packages() ) ){
  install.packages(c('foreach','doParallel'), repos="https://cloud.r-project.org") }

# load libraries
pkgs = c(
    'amen',
    'reshape2','plyr','ggplot2','latex2exp',
    'foreach', 'doParallel',
    'xtable','ROCR','caTools','RColorBrewer','png','grid')
shh=lapply(pkgs, library, character.only=TRUE)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
source(paste0(mainPath, 'coefHelpers.R'))
source(paste0(mainPath, 'netPerfHelpers.R'))
source(paste0(mainPath, 'binPerfHelpers.R'))

# load data
load(paste0(mainPath, 'data/data.rda'))
####################

#run amen ################### 
cl = makeCluster(3) ; registerDoParallel(cl)
shhh = foreach(k = c(1,3,4), .packages=c('amen')) %dopar% {
	if(!file.exists(paste0(graphicsPath, 'ameEst_k',k,'.rda'))){
	# Run amen in parallel
	ameFit = ameTest(Y=Y, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
	    model='bin', symmetric=FALSE, R=k, 
	    nscan=100000, seed=seed, burn=50000, odens=10, 
	    rvar=TRUE, cvar=TRUE,
	    plot=FALSE, print=FALSE) 
    # reduce obj to save
    ameFit = ameFit[c('BETA','GOF')]
    save(ameFit, file=paste0(graphicsPath, 'ameEst_k',k,'.rda')) }
} ; stopCluster(cl)
#################### 

################################################
# load model results
load(paste0(graphicsPath, 'ameEst_k1.rda'))
ameCoef1 = getAmeCoef(ameFit) ; rownames(ameCoef1) = gsub('.col','',rownames(ameCoef1))
ameTab1 = lazyCleanTable( ameCoef1[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(resultsPath, 'ameEst.rda'))
ameCoef2 = getAmeCoef(ameFit) ; rownames(ameCoef2) = gsub('.col','',rownames(ameCoef2))
ameTab2 = lazyCleanTable( ameCoef2[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(graphicsPath, 'ameEst_k3.rda'))
ameCoef3 = getAmeCoef(ameFit) ; rownames(ameCoef3) = gsub('.col','',rownames(ameCoef3))
ameTab3 = lazyCleanTable( ameCoef3[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(graphicsPath, 'ameEst_k4.rda'))
ameCoef4 = getAmeCoef(ameFit) ; rownames(ameCoef4) = gsub('.col','',rownames(ameCoef4))
ameTab4 = lazyCleanTable( ameCoef4[,c('pmean','lo95','hi95','p-val')], sigCol=4 )
################################################

################################################
# summarize in q & d table
coefDfs = list(
	'AME (k=1)'=ameTab1, 'AME (k=2)'=ameTab2,
	'AME (k=3)'=ameTab3, 'AME (k=4)'=ameTab4 )

# table
frameRows = rep(varKey[,2], each=2)
frameRows = append(frameRows, '\\textbf{Conflicting policy preferences}', which(frameRows=='Intercept/Edges')[2])
frameRows = append(frameRows, '\\textbf{Transaction costs}', which(frameRows=='Preference dissimilarity')[2])
frameRows = append(frameRows, '\\textbf{Influence}', which(frameRows=='Joint forum participation')[2])
frameRows = append(frameRows, '\\textbf{Functional requirements}', which(frameRows=='Alter = Government actor')[2])
frameRows = append(frameRows, '\\textbf{Endogenous dependencies}', which(frameRows=='Same actor type')[2])
frame = matrix('', nrow=length(frameRows), ncol=length(coefDfs)+1, 
	dimnames=list( frameRows, c(' ', names(coefDfs)) ))

# helpful ids
varPos = which(frameRows %in% varKey[,2])
estRows = varPos[seq(1,length(varPos),2)]; errorRows = varPos[seq(2,length(varPos),2)]
varLab = frameRows; varLab[errorRows] = ''; frame[,1] = varLab; rownames(frame)[estRows] = ''

# Add logit results
frame = insertCoefInfo(frame, model='AME (k=1)', error='int')
frame = insertCoefInfo(frame, model='AME (k=2)', error='int')
frame = insertCoefInfo(frame, model='AME (k=3)', error='int')
frame = insertCoefInfo(frame, model='AME (k=4)', error='int')

# cleanup
rownames(frame) = NULL
frame[estRows[-1],1] = paste0('$\\;\\;\\;\\;$ ', frame[estRows[-1],1])
frame = frame[-(grep('Endogenous dependencies', frame[,1]):nrow(frame)),]

# print
print.xtable(
	xtable(frame, align='llcccc',
		caption='* p $<$ 0.05 (or 0 outside the 95\\% confidence interval).',
		label='tab:regTable_latSpace'), 
	include.rownames=FALSE,
	sanitize.text.function=identity,
	hline.after=c(0,0,nrow(frame),nrow(frame)),
	size='tiny', file=paste0(graphicsPath, 'TableA3.tex')
)
################################################

################################################
# actual values
actVals = gofstats(Y)
perfNetKey = cbind(v=names(actVals), 
	c=c('Sender variation', 'Receiver variation', 'Dyadic dependency', 'Triadic dependency') )
################################################

################################################
# AME
ameSRFileNames = c(
	paste0(graphicsPath, 'ameEst_k1.rda'),
	paste0(resultsPath, 'ameEst.rda'),
	paste0(graphicsPath, 'ameEst_k',3:4,'.rda')
	)

perfListSR = lapply(ameSRFileNames, function(x){ load(x) ; ameFit$'GOF'[-1,] })
names(perfListSR) = c( 'AME (k=1)','AME (k=2)','AME (k=3)','AME (k=4)')
################################################

################################################
# Viz 
getNetPerfCoef(
	perfListSR, perfNetKey, actVals, 
	pRows=1, save=TRUE, fPath=paste0(graphicsPath, 'FigureA5.pdf'))
################################################

################################################
# out of sample analysis

#amen k=1 #########
if(!file.exists(paste0(graphicsPath, 'ameOutSamp_k1.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=1, 
        nscan=100000, seed=12345, burn=50000, odens=10, 
        rvar=TRUE, cvar=TRUE,  
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = pnorm(preds)
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ) } ; stopCluster(cl)
amePred = do.call('rbind', lapply(modsAme, function(x){ x$pred }))
rm(list='modsAme')
save(amePred, file=paste0(graphicsPath, 'ameOutSamp_k1.rda'))
}
##########

#amen k=3 #########
if(!file.exists(paste0(graphicsPath, 'ameOutSamp_k3.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=3, 
        nscan=100000, seed=12345, burn=50000, odens=10, 
        rvar=TRUE, cvar=TRUE,  
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = pnorm(preds)
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ) } ; stopCluster(cl)
amePred = do.call('rbind', lapply(modsAme, function(x){ x$pred }))
rm(list='modsAme')
save(amePred, file=paste0(graphicsPath, 'ameOutSamp_k3.rda'))
}
##########

#amen k=4 #########
if(!file.exists(paste0(graphicsPath, 'ameOutSamp_k4.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=4, 
        nscan=100000, seed=12345, burn=50000, odens=10, 
        rvar=TRUE, cvar=TRUE,  
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = pnorm(preds)
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ) } ; stopCluster(cl)
amePred = do.call('rbind', lapply(modsAme, function(x){ x$pred }))
rm(list='modsAme')
save(amePred, file=paste0(graphicsPath, 'ameOutSamp_k4.rda'))
}
##########

#org preds #########
fPaths = c(
	paste0(graphicsPath, 'ameOutSamp_k1.rda'),
	paste0(resultsPath, 'ameOutSamp.rda'),
	paste0(graphicsPath, 'ameOutSamp_k',3:4,'.rda')
	)
predDfs = lapply(fPaths, function(x){load(x);return(amePred)})
names(predDfs) = paste0('AME (k=',1:4,')')
##########

#viz results #########
# get auc summary
aucSumm = do.call('rbind', 
    lapply(predDfs, function(x){ 
        cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) ) 
        } ) ) ; rownames(aucSumm) = names(predDfs)
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))
 
# Roc Plot
rocData = lapply(1:length(predDfs), function(ii){
    r = roc(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual')
    p = cbind(r, model=names(predDfs)[ii])
    return(p) })
rocData = do.call('rbind', rocData)

# model col/lty
ggCols = rev(brewer.pal(length(levels(rocData$model)), 'Set1'))
ggLty = c('dashed', 'dotdash', 'twodash', 'solid')

# Separation plots
sepPngList = lapply(1:length(predDfs), function(ii){
    fSepPath = paste0(graphicsPath,'sep_',names(predDfs)[ii],'_outSample.png')
    ggSep(actual=predDfs[[ii]]$'actual', proba=predDfs[[ii]]$'prob', 
        color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
    sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
    return(sepG)
})

tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
  tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
  yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1)[1:4],
  label=names(predDfs) )
ggsave(tmp, 
    file=paste0(graphicsPath, 'FigureA4a.pdf'), 
    width=5, height=5 )       

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
    r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
    p = cbind(r, model=names(predDfs)[ii])
    return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
  guides(linetype=FALSE, color=FALSE) + 
  annotate('text', hjust=0, x=c(-.1, .09, .24), y=.45,
    label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), size=4) + 
  annotate('text', hjust=0, x=-.15, y=seq(.05, .45, .1)[1:4],
    label=rev(rownames(aucSumm)) ) + 
  annotate('text', hjust=0, x=.1, y=seq(.05, .45, .1)[1:4],
    label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')}))
    )
tmp
ggsave(tmp, 
    file=paste0(graphicsPath, 'FigureA4b.pdf'), 
    width=5, height=5
  )
#########
################################################