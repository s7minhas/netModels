rm(list=ls())

#
toInstall = c(
  'devtools', 'latentnet', 'ROCR', 'caTools',
  'foreach', 'doParallel', 'reshape2', 'ggplot2'
)
for(pkg in toInstall){
  if(!pkg %in% installed.packages()[,1]){
    install.packages(pkg)
  }
}

if(!'amen' %in% installed.packages()[,1]){
  devtools::install_github('s7minhas/amen')
}

#
library(devtools)
library(amen)
library(latentnet)
library(ROCR)
library(caTools)
library(foreach)
library(doParallel)
library(reshape2)
library(ggplot2)

#
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

# sim function
runSim <- function(rho, seed){
  
  #
  set.seed(seed)
  
  #
  n <- 100
  xDyad <- matrix(rnorm(n^2), nrow=n, ncol=n)
  e <- matrix(rnorm(n^2), n, n)
  
  #
  b0 = -1
  b1 = 1
  
  #
  y <- b0*matrix(1,n,n) + b1*xDyad + e
  y <- y + rho*t(y)
  y <- 1*(y>0)
  actRho = cor(c(y), c(t(y)), use='pairwise.complete.obs')
  
  #
  ameMod <- ame(
    Y=y, model='bin', 
    symmetric=FALSE, R=2, 
    rvar=FALSE, cvar=FALSE, 
    seed=6886,
    plot=FALSE, print=FALSE, gof=FALSE)
  
  #
  lsmMod <- ergmm(
    y ~ euclidean(d=2)
  )
  
  #
  amePred = ameMod$'EZ'
  diag(amePred) = NA
  amePred = c(amePred)
  amePred = amePred[!is.na(amePred)]
  amePred = pnorm(amePred)
  
  #
  lsmPred = predict(lsmMod)
  diag(lsmPred) = NA
  lsmPred = lsmPred[!is.na(lsmPred)]
  lsmPred = c(lsmPred)
  
  #
  diag(y) = NA
  y = c(y)
  y = y[!is.na(y)]
  
  #
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
    )
  )
  return(res)  
}

#
rhoEffect = seq(0,.6,.2)
results <- list()
for(i in 1:length(rhoEffect)){
  sims <- 50
  cl=makeCluster(50) ; registerDoParallel(cl)
  out <- foreach(sim=1:sims, 
                 .packages=c('amen','latentnet','ROCR','caTools')
  ) %dopar% {
    runSim(rhoEffect[i], sim)
  }
  stopCluster(cl)
  rocResults <- do.call('rbind', lapply(out, function(x){x$'roc'}))
  prResults <- do.call('rbind', lapply(out, function(x){x$'pr'}))
  rocResults <- data.frame(
    rocResults, rhoEffect=rhoEffect[i], stat='roc')
  prResults <- data.frame(
    prResults, rhoEffect=rhoEffect[i], stat='pr')
  
  results[[i]] <- rbind(rocResults, prResults)
}

results <- do.call('rbind', results)
save(results, file='sim2Results.rda')


ggData = melt(results, id=c('rhoEffect','actRho','stat'))

set.seed(6886)
sim2Viz = ggplot(ggData, aes(x=factor(variable), y=value)) +
  geom_jitter(alpha=.5) +  
  geom_boxplot(outlier.alpha=.01,alpha=.7) +
  facet_grid(stat~rhoEffect, scales='free_y') +
  theme(
    panel.border=element_blank(),
    axis.ticks=element_blank()
  )
ggsave(sim2Viz, file='sim2Viz.pdf')