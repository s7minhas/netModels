#######################################################
# clear workspace
rm(list=ls())

# install libraries
toInstall = c(
  'devtools', 'amen', 'latentnet', 'ROCR', 
  'caTools', 'foreach', 'doParallel', 
  'reshape2', 'ggplot2', 'latex2exp', 'Cairo'
  )
for(pkg in toInstall){
  if(!pkg %in% installed.packages()[,1]){
    install.packages(pkg) } }

if(!'amen' %in% installed.packages()[,1]){
  devtools::install_github('s7minhas/amen') }

# load libraries
library(devtools)
library(amen)
library(latentnet)
library(ROCR)
library(caTools)
library(foreach)
library(doParallel)
library(reshape2)
library(ggplot2)
library(latex2exp)
library(Cairo)
theme_set(theme_bw())

# helpers
char <- function(x){as.character(x)}
num <- function(x){as.numeric(char(x))}

#' Area under the ROC curve
auc_roc <- function(obs, pred) {
  pred <- prediction(pred, obs)
  auc  <- performance(pred, "auc")@y.values[[1]]
  return(auc) }

#' Area under Precision-recall curve
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  xy <- subset(xy, !is.nan(xy$precision))
  res   <- trapz(xy$recall, xy$precision)
  return(res) }
#######################################################

#######################################################
# sim function to compare effects of degree heterogeneity
runSim <- function(rho, seed){
  
  # set up covars
  set.seed(seed) ; n <- 100
  xDyad <- matrix(rnorm(n^2), nrow=n, ncol=n)
  e <- matrix(rnorm(n^2), n, n)
  
  # set up covar effects
  b0 = -1 ; b1 = 1
  
  # create dv
  y <- b0*matrix(1,n,n) + b1*xDyad + e
  y <- y + rho*t(y) ; y <- 1*(y>0)
  actRho = cor(c(y), c(t(y)), use='pairwise.complete.obs')
  
  # run AME
  ameMod <- ame(
    Y=y, model='bin', 
    symmetric=FALSE, R=2, 
    rvar=FALSE, cvar=FALSE, 
    seed=6886,
    plot=FALSE, print=FALSE, gof=FALSE)
  
  # run lsm
  lsmMod <- ergmm( y ~ euclidean(d=2) )
  
  # org ame preds
  amePred = ameMod$'EZ' ; diag(amePred) = NA
  amePred = c(amePred) ; amePred = amePred[!is.na(amePred)]
  amePred = pnorm(amePred)
  
  # org lsm preds
  lsmPred = predict(lsmMod) ; diag(lsmPred) = NA
  lsmPred = c(lsmPred) ; lsmPred = lsmPred[!is.na(lsmPred)]
  
  # reorg y
  diag(y) = NA ; y = c(y) ; y = y[!is.na(y)]
  
  # calc auc stats and store
  res = list(
    roc=c(
      ame=auc_roc(c(y), amePred),
      lsm=auc_roc(c(y), lsmPred),
      actRho=actRho
    ),
    pr=c(
      ame=auc_pr(c(y), amePred),
      lsm=auc_pr(c(y), lsmPred),
      actRho=actRho
    ) )
  return(res) }
#######################################################  

#######################################################
# params for sim
rhoEffect = seq(0,.6,.2) ; sims <- 50
# run ame v lsm sims comparing reciprocity
setwd('~/Research/netModels/code/simulations/')
if(!file.exists('sim2Results.rda')){
  # create output object to stores
  results <- list()
  # loop through rho effects
  for(i in 1:length(rhoEffect)){
    # parallelize across # of sims
    cl=makeCluster(50) ; registerDoParallel(cl)
    out <- foreach(sim=1:sims, 
      .packages=c('amen','latentnet','ROCR','caTools')
    ) %dopar% { runSim(rhoEffect[i], sim) } ; stopCluster(cl)

    # pull out roc results
    rocResults <- do.call('rbind', lapply(out, function(x){x$'roc'}))

    # pull out pr results
    prResults <- do.call('rbind', lapply(out, function(x){x$'pr'}))
    
    # combine
    rocResults <- data.frame(
      rocResults, rhoEffect=rhoEffect[i], stat='roc')
    prResults <- data.frame(
      prResults, rhoEffect=rhoEffect[i], stat='pr')
    results[[i]] <- rbind(rocResults, prResults) }

  # org results from loop across rho effects 
  results <- do.call('rbind', results)
  save(results, file='sim2Results.rda') }
load('sim2Results.rda')
#######################################################

#######################################################
# org data for plotting
ggData = melt(results, id=c('rhoEffect','actRho','stat'))

# calc rho stats across sims
rhoStats = tapply(results$actRho, results$rhoEffect, mean)
ggData$rhoVal = rhoStats[match(ggData$rhoEffect, names(rhoStats))]

# clean up labels
ggData$rhoVal = paste0('$\\bar{\\rho}$=', round(ggData$rhoVal,2))
ggData$variable = char(ggData$variable)
ggData$variable[ggData$variable=='ame'] = 'LFM'
ggData$variable[ggData$variable=='lsm'] = 'LSM'
ggData$stat = char(ggData$stat)
ggData$stat[ggData$stat=='roc'] = 'AUC (ROC)'
ggData$stat[ggData$stat=='pr'] = 'AUC (PR)'
ggData$stat = factor(ggData$stat, levels=unique(ggData$stat))

# viz
set.seed(6886)
facet_labeller = function(string){ TeX(string) }
sim2Viz = ggplot(ggData, aes(x=factor(variable), y=value)) +
  geom_boxplot(outlier.alpha = .01) +
  geom_jitter(alpha=.2) +
  facet_grid(stat~rhoVal, scales='free_y',
    labeller=as_labeller(facet_labeller, default = label_parsed)) +
  xlab('') + ylab('') +
  theme(
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(family="Source Sans Pro"),
    axis.text.x=element_text(family="Source Sans Pro"),    
    axis.title=element_text(family='Source Sans Pro Semibold'),
    strip.text = element_text(family="Source Sans Pro Semibold")        
  )
ggsave(sim2Viz, file='sim2Viz.pdf', width=8, height=5, device=cairo_pdf)  
#######################################################