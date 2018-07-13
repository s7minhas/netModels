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

# load libraries
pkgs = c(
    'network','sna','ergm','latentnet','btergm','amen',
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

#run lsm with sender receiver effects ################### 
if(!file.exists(paste0(graphicsPath, 'lsmEst_sr.rda'))){
model.lsSR <- ergmm(nw.collab ~ 
    euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
    rsender(var=1, var.df=3) + 
    rreceiver(var=1, var.df=3) + 
    edgecov(gov.ifactor) + 
    edgecov(ngo.ofactor) + 
    nodematch("orgtype") + 
    edgecov(priv.ngo) + 
    edgecov(forum) + 
    edgecov(infrep) + 
    nodeicov("influence") + 
    absdiff("influence") + 
    edgecov(prefdist) + 
    edgecov(allopp), 
    seed = seed, 
    control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
)
save(model.lsSR, file=paste0(graphicsPath, 'lsmEst_sr.rda'))
}
################### 

################### 
# load model results

# ame
load(paste0(resultsPath, 'ameEst.rda'))
ameCoef = getAmeCoef(ameFit) ; rownames(ameCoef) = gsub('.col','',rownames(ameCoef))
ameTab = lazyCleanTable( ameCoef[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

# lsm
load(paste0(resultsPath, 'lsmEst.rda'))
lsEuclCoef = summary(model.ls)$'pmean'$'coef.table'
lsEuclTab = lazyCleanTable( lsEuclCoef, sigCol=4 )

# lsm sr
load(paste0(graphicsPath, 'lsmEst_sr.rda'))
lsEuclCoefSR = summary(model.lsSR)$'pmean'$'coef.table'
lsEuclTabSR = lazyCleanTable( lsEuclCoefSR, sigCol=4 )
################### 

################################################
# summarize in q & d table
coefDfs = list(LSM=lsEuclTab,  'LSM (SR)'=lsEuclTabSR,  AME=ameTab)

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
frame = insertCoefInfo(frame, model='LSM', error='int')
frame = insertCoefInfo(frame, model='LSM (SR)', error='int')
frame = insertCoefInfo(frame, model='AME', error='int')

# cleanup
rownames(frame) = NULL
frame[estRows[-1],1] = paste0('$\\;\\;\\;\\;$ ', frame[estRows[-1],1])
frame = frame[-(grep('Endogenous dependencies', frame[,1]):nrow(frame)),]

# print
print.xtable(
	xtable(frame, align='llccc',
		caption='* p $<$ 0.05 (or 0 outside the 95\\% confidence interval).',
		label='tab:regTable_latSpace'), 
	include.rownames=FALSE,
	sanitize.text.function=identity,
	hline.after=c(0,0,nrow(frame),nrow(frame)),
	size='tiny', file=paste0(graphicsPath, 'TableA4.tex')
)
################################################

################################################
# actual values
actVals = gofstats(Y)
perfNetKey = cbind(v=names(actVals), 
	c=c('Sender variation', 'Receiver variation', 'Dyadic dependency', 'Triadic dependency') )
################################################

################################################
# LS - EUCL
lsEuclSim = simulate.ergmm(model.ls, nsim=100)
lsEuclPerf = do.call('rbind', lapply(lsEuclSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# LS - EUCL SR
lsEuclSRSim = simulate.ergmm(model.lsSR, nsim=100)
lsEuclSRPerf = do.call('rbind', lapply(lsEuclSRSim$'networks', function(x){ 
	gofstats( as.sociomatrix( x ) ) }))

# AME
amePerf = ameFit$'GOF'[-1,]

# org
perfList = list( 'LSM'=lsEuclPerf, 'LSM (SR)'=lsEuclSRPerf, 'AME'=amePerf )
################################################

################################################
# Viz 
getNetPerfCoef(
	perfList, perfNetKey, actVals, 
	pRows=1, save=TRUE, 
	fPath=paste0(graphicsPath, 'FigureA7.pdf'))
################################################

################################################
# out of sample analysis

#latentnet sr ########
if(!file.exists(paste0(graphicsPath, 'lsmOutSamp_SR.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsLs = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet') ) %dopar% {
    pergmm = TRUE ; source(paste0(mainPath, '0_replicationSetup.R'))
    dv = nw.collabMiss[[ii]]
    mod <- ergmm(dv ~ 
        euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
	    rsender(var=1, var.df=3) + 
	    rreceiver(var=1, var.df=3) +         
        edgecov(gov.ifactor) + 
        edgecov(ngo.ofactor) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp), 
        seed = seed, 
        control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
    probs = predict(mod)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,pred=pred)  
    return(summ) } ; stopCluster(cl)
lsEuclPredSR = do.call('rbind', lapply(modsLs, function(x){ x$pred }))
rm(list='modsLs')
save(lsEuclPredSR, file=paste0(graphicsPath, 'lsmOutSamp_SR.rda'))
}
##########

#org preds #########
load(paste0(resultsPath, 'ameOutSamp.rda'))
load(paste0(resultsPath, 'lsmOutSamp.rda'))
load(paste0(graphicsPath, 'lsmOutSamp_SR.rda'))

predDfs = list( 'LSM'=lsEuclPred, 'LSM (SR)'=lsEuclPredSR, 'AME'=amePred )
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
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')
ggLty = c('dotdash', 'twodash', 'solid')

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
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1)[1:3], label=names(predDfs))
ggsave(tmp, 
    file=paste0(graphicsPath, 'FigureA6a.pdf'), 
    width=5, height=5 )       

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
    r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
    p = cbind(r, model=names(predDfs)[ii])
    return(p) })
rocPrData = do.call('rbind', rocPrData)

aucLabs = rownames(aucSumm)
tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
	guides(linetype=FALSE, color=FALSE) + 
	annotate('text', hjust=0, x=c(-.1, .27, .42), y=.35, 
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), size=4) + 
	annotate('text', hjust=0, x=-.1, y=seq(.05, .45, .1)[1:3], 		
		label=rev(aucLabs)) + 
	annotate('text', hjust=0, x=.28, y=seq(.05, .45, .1)[1:3], 
		label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')}))
		)
ggsave(tmp, 
    file=paste0(graphicsPath, 'FigureA6b.pdf'), 
    width=5, height=5 )
#########
################################################