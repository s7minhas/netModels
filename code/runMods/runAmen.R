rm(list=ls())
seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
library(amen)

# load data
load(paste0(dataPath, 'latFacData.rda'))

# Remove exog recip term
Xd = Xd[,,-which(dimnames(Xd)[[3]] %in% 'collab.t')]

# mcmc params
imps = 100000
brn = 50000
ods = 10
latDims = c(2, 4, 6)

# Run amen
for(latDim in latDims){
  ameFit = ame(Y=Y, Xdyad=Xd, Xrow=Xs, Xcol=NULL, 
               model='bin', symmetric=FALSE, R=latDim,
               nscan=imps, seed=seed, burn=brn, odens=ods,
               plot=TRUE, print=FALSE)
  save(ameFit, file=paste0(resultsPath, 'ameFit_', latDim, '.rda'))
}