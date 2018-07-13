#set up workspace ################### 
rm(list=ls())

seed <- 12345
set.seed(seed)
# # example linux path
# mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/Research/netModels/dataverseRepl/' 

# install libraries for parallelization
oPkgs = c('doParallel','foreach')
for(pkg in oPkgs){
  if(!pkg %in% installed.packages())
    install.packages(pkg, repos="https://cloud.r-project.org") }

# load libraries
pkgs = c(
    'network','sna','ergm','latentnet','btergm','amen',
    'reshape2','plyr','ggplot2','latex2exp',
    'xtable','ROCR','caTools','RColorBrewer','png','grid',
    'doParallel','foreach')
shh=lapply(pkgs, library, character.only=TRUE)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
source(paste0(mainPath, 'binPerfHelpers.R'))

# load data
load(paste0(mainPath, 'data/data.rda'))
####################

#ergm #########
if(!file.exists(paste0(mainPath, 'results/ergmOutSamp.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsErgm = foreach(ii=1:length(nw.collabMiss), .packages=c('ergm','sna') ) %dopar% {
    dv = nw.collabMiss[[ii]]
    mod <- ergm(dv ~ 
        edges + 
        edgecov(collab.t) + 
        nodeifactor("orgtype", base = -1) + 
        nodeofactor("orgtype", base = -2) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp) + 
        odegreepopularity + 
        twopath + 
        gwidegree(2, fixed = TRUE) + 
        gwesp(1, fixed = TRUE) + 
        gwodegree(0.5, fixed = TRUE),
        eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm(seed = seed) )
    sim = simulate.ergm(mod,nsim=1000)
    probs = Reduce('+', lapply(sim, as.sociomatrix) ) / length(sim)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,sim=sim,pred=pred)
    return(summ) } ; stopCluster(cl)
ergmPred = do.call('rbind', lapply(modsErgm, function(x){ x$pred }))
rm(list='modsErgm')
save(ergmPred, file=paste0(mainPath, 'results/ergmOutSamp.rda'))
}
load(paste0(mainPath, 'results/ergmOutSamp.rda'))
##########

#latentnet ########
if(!file.exists(paste0(mainPath, 'results/lsmOutSamp.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsLs = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet') ) %dopar% {
    pergmm = TRUE ; source(paste0(mainPath, '0_replicationSetup.R'))
    dv = nw.collabMiss[[ii]]
    mod <- ergmm(dv ~ 
        euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
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
lsEuclPred = do.call('rbind', lapply(modsLs, function(x){ x$pred }))
rm(list='modsLs')
save(lsEuclPred, file=paste0(mainPath, 'results/lsmOutSamp.rda'))
}
load(paste0(mainPath, 'results/lsmOutSamp.rda'))
##########

#logit #########
if(!file.exists(paste0(mainPath, 'results/logitOutSamp.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsLogit = foreach(ii=1:length(yMiss) ) %dopar% {

    dv = yMiss[[ii]]
    logit.data$dv = c(dv)
    mod <- glm(dv ~ priv.ngo + allopp + prefdist + forum + infrep + 
        influence.icov + influence.absdiff + gov.ifactor + ngo.ofactor + type.nodematch + collab.t, 
        data = logit.data, family = "binomial")
    beta = mod$coefficients
    outDesignArr = cbind(1, logit.data[which(rposmat==ii),names(mod$'coefficients')[2:length(mod$'coefficients')]])
    yhat = data.matrix(outDesignArr) %*% beta
    prob = 1/(1+exp(-yhat))
    pred=data.frame(prob=prob, actual=yAct[[ii]])
    summ=list(mod=mod, pred=pred)
    return(summ) } ; stopCluster(cl)
logitPred = do.call('rbind', lapply(modsLogit, function(x){ x$pred }))
rm(list='modsLogit')
save(logitPred, file=paste0(mainPath, 'results/logitOutSamp.rda'))
}
load(paste0(mainPath, 'results/logitOutSamp.rda'))
##########

#amen #########
if(!file.exists(paste0(mainPath, 'results/ameOutSamp.rda'))){
cores = length(nw.collabMiss)
cl=makeCluster(cores) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=2, 
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
save(amePred, file=paste0(mainPath, 'results/ameOutSamp.rda'))
}
load(paste0(mainPath, 'results/ameOutSamp.rda'))
##########

#organize preds #########
predDfs = list(LSM=lsEuclPred, Logit=logitPred, ERGM=ergmPred, AME=amePred)
##########

################################################
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
    fSepPath = paste0(mainPath,'floats/sep_',names(predDfs)[ii],'_outSample.png')
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
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1)[1:4],
  label=names(predDfs)
  )
ggsave(tmp, 
    file=paste0(mainPath, 'floats/Figure2a_color.pdf'), 
    width=5, height=5 )       

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
    r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
    p = cbind(r, model=names(predDfs)[ii])
    return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
  guides(linetype=FALSE, color=FALSE) + 
  annotate('text', hjust=0, x=c(-.1, .09, .22), y=.45,
    label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)')
    , size=4) + 
  annotate('text', hjust=0, x=-.1, y=seq(.05, .45, .1)[1:4],
    label=rev(rownames(aucSumm))
    ) + 
  annotate('text', hjust=0, x=.1, y=seq(.05, .45, .1)[1:4],
    label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')}))
    )
ggsave(tmp, 
    file=paste0(mainPath, 'floats/Figure2b_color.pdf'), 
    width=5, height=5 )

# redo plots in grey scale
nGroups = length(levels(rocData$model))
ggCols = brewer.pal(nGroups+3, 'Greys')[4:(nGroups+3)]

# Separation plots grey scale
sepPngList = lapply(1:length(predDfs), function(ii){
    fSepPath = paste0(mainPath,'floats/sep_',names(predDfs)[ii],'_outSample_bw.png')
    # save as pngs for potential use outside of roc
    tmp=ggSep(actual=predDfs[[ii]]$'actual', proba=predDfs[[ii]]$'prob', 
        color='darkgrey', lty=ggLty[ii], fPath=fSepPath, save=FALSE ) +
        geom_line(
          aes(y = proba, x = seq(length.out = length(actual))), 
          linetype=ggLty[ii], color=ggCols[ii], lwd = 4)
    ggsave(tmp, file=fSepPath, width=12, height=2)
    sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
    return(sepG)
})

tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
  tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
  yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.45,.1)[1:4], 
  label=names(predDfs)
  )
ggsave(tmp, 
    file=paste0(mainPath, 'floats/Figure2a_bw.pdf'), 
    width=5, height=5 )       

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
    r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
    p = cbind(r, model=names(predDfs)[ii])
    return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
  guides(linetype=FALSE, color=FALSE) + 
  annotate('text', hjust=0, x=c(-.1, .09, .22), y=.45,    
    label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)')
    , family='Source Sans Pro Light'
    , size=4) + 
  annotate('text', hjust=0, x=-.1, y=seq(.05, .45, .1)[1:4],
    label=rev(rownames(aucSumm))
    ) + 
  annotate('text', hjust=0, x=.1, y=seq(.05, .45, .1)[1:4],
    label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')}))
    )
ggsave(tmp, 
    file=paste0(mainPath, 'floats/Figure2b_bw.pdf'), 
    width=5, height=5 )
################################################