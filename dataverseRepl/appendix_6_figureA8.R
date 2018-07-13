#######################################################
# clear workspace
rm(list=ls())

# paths
# # example linux path
# mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/dataverseRepl/' 
graphicsPath = paste0(mainPath, 'appendix_results_floats/')

# install libraries
toInstall = c(
  'devtools', 'amen', 'latentnet', 'ROCR', 
  'caTools', 'foreach', 'doParallel', 'reshape2', 
  'latex2exp', 'ggplot2', 'igraph', 'Cairo'
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
library(latex2exp)
library(ggplot2)
library(igraph)
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
runSim <- function(betaNode, seed){
  
  # set up covars
  set.seed(seed) ; n <- 100
  xNode <- matrix(rbinom(n, 1, .05), nrow=n, ncol=n, byrow=FALSE)
  xDyad <- matrix(rnorm(n^2), nrow=n, ncol=n)
  e <- matrix(rnorm(n^2), n, n)
  
  # set up covar effects
  b0 = -3 ; b1 = betaNode ; b2 = 1
  
  # create dv
  y <- b0*matrix(1,n,n) + b1*xNode + b2*xDyad + e
  y <- 1*(y>0)
  
  # run ame
  ameMod <- ame(
    Y=y, model='bin', symmetric=FALSE, R=2, 
    rvar=FALSE, cvar=FALSE, seed=6886,
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
      lsm=auc_roc(c(y), lsmPred)
    ),
    pr=c(
      ame=auc_pr(c(y), amePred),
      lsm=auc_pr(c(y), lsmPred)
    ) )
  return(res) }
#######################################################  

#######################################################
# params for sim
nodeEffect = seq(0,2.5,.5) ; sims <- 50
# run ame v lsm sims comparing degree heterogeneity 
setwd(graphicsPath)
if(!file.exists('sim1Results.rda')){
  # create output object to stores
  results <- list()
  # loop through node effects
  for(i in 1:length(nodeEffect)){
    # parallelize across # of sims
    cl=makeCluster(50) ; registerDoParallel(cl)
    out <- foreach(sim=1:sims, 
      .packages=c('amen','latentnet','ROCR','caTools')
    ) %dopar% { runSim(nodeEffect[i], sim) } ; stopCluster(cl)
    
    # pull out roc results
    rocResults <- do.call('rbind', lapply(out, function(x){x$'roc'}))
    
    # pull our pr results
    prResults <- do.call('rbind', lapply(out, function(x){x$'pr'}))
    
    # combine
    rocResults <- data.frame(
      rocResults, nodeEffect=nodeEffect[i], stat='roc')
    prResults <- data.frame(
      prResults, nodeEffect=nodeEffect[i], stat='pr')
    results[[i]] <- rbind(rocResults, prResults) }
  
  # org results from loop across node effects
  results <- do.call('rbind', results)
  save(results, file='sim1Results.rda') }
load('sim1Results.rda')
#######################################################

#######################################################
# calc actor variance stats for sim networks
netStats = function(betaNode, seed, returnY=FALSE){
  set.seed(seed)
  n <- 100
  xNode <- matrix(rbinom(n, 1, .05), nrow=n, ncol=n, byrow=FALSE)
  xDyad <- matrix(rnorm(n^2), nrow=n, ncol=n)
  e <- matrix(rnorm(n^2), n, n)
  b0 = -3 ; b1 = betaNode ; b2 = 1
  y <- b0*matrix(1,n,n) + b1*xNode + b2*xDyad + e
  y <- 1*(y>0)  
  aDegree <- apply(y,1,sum) + apply(y,2,sum)
  out <- c(nodeEffect=betaNode, sim=seed, actorSD=sd(aDegree))
  if(returnY){ out <- y }
  return( out ) }
yVals = do.call('rbind', lapply(nodeEffect, function(beta){
  do.call('rbind', lapply(1:sims, function(sim){
    netStats(beta,sim) })) }))
#######################################################

#######################################################
# org data for plotting
ggData = melt(results, id=c('nodeEffect','stat'))
stats=tapply(yVals[,'actorSD'],yVals[,'nodeEffect'],mean)
ggData$actorSD = stats[match(ggData$nodeEffect,names(stats))]
ggData$actorSD = round(ggData$actorSD, 2)

# clean up labels
ggData$actorSD = paste0('$\\bar{\\sigma^{2}}$=', ggData$actorSD)
ggData$variable = char(ggData$variable)
ggData$variable[ggData$variable=='ame'] = 'LFM'
ggData$variable[ggData$variable=='lsm'] = 'LSM'
ggData$variable = factor(ggData$variable)
ggData$stat = char(ggData$stat)
ggData$stat[ggData$stat=='roc'] = 'AUC (ROC)'
ggData$stat[ggData$stat=='pr'] = 'AUC (PR)'
ggData$stat = factor(ggData$stat, levels=unique(ggData$stat))

# viz
set.seed(6886)
facet_labeller = function(string){ TeX(string) }
sim1Viz = ggplot(ggData, aes(x=variable, y=value )) +
  geom_jitter(alpha=.2) +
  geom_boxplot(outlier.alpha = .01, alpha=.7, color='gray40') +
  facet_grid(stat~actorSD, scales='free_y',
    labeller=as_labeller(facet_labeller, default = label_parsed)) +
  xlab('') + ylab('') +
  theme(
    panel.border=element_blank(),
    panel.grid=element_blank(),
    axis.ticks=element_blank(),
    legend.position = 'none',
    axis.text.y=element_text(family="Source Sans Pro"),
    axis.text.x=element_text(family="Source Sans Pro"),    
    axis.title=element_text(family='Source Sans Pro Semibold'),
    strip.text = element_text(family="Source Sans Pro Semibold")    
  )
ggsave(sim1Viz, file='FigureA8_base.pdf', width=8, height=5, device=cairo_pdf)

# plot representative nets
sampleNet = function(beta){
  y <- netStats(beta, 1, TRUE)
  g <- graph_from_adjacency_matrix(y, mode='directed', weighted=NULL, diag=FALSE)
  tiesSum = degree(g) ; V(g)$size <- tiesSum
  iso <- V(g)[degree(g)==0] ; g <- delete.vertices(g, iso)

  # par(mfrow=c(1,1), bg=NA)
  fName = paste0('netViz',beta,'.pdf')
  par(mfrow=c(1,1), bg='white')
  set.seed(6886)
  pdf(file=fName)
  plot(g,
    layout=layout_nicely,
    vertex.size=V(g)$size,
    vertex.color='gray80',
    edge.width=.05,
    edge.arrow.size=.1,
    edge.color='gray90',
    vertex.label='' )
  dev.off()
  system(paste('pdfcrop',fName,fName))
}
shh = lapply(nodeEffect, sampleNet)
#######################################################