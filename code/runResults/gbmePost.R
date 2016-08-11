rm(list=ls())
seed = 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))

# load data
load(paste0(dataPath, 'latFacData.rda'))
# Remove exog recip term
Xd = Xd[,,-which(dimnames(Xd)[[3]] %in% 'collab.t')]

# load gbme results
out = read.table(paste0(resultsPath, 'gbmeDir/OUT_2'), header=TRUE)
PS = out[out$scan>round(max(out$scan)/2),-(1:3)] 

# Check convergence
convData = data.frame( PS[,1:11], stringsAsFactors=FALSE)
convData$iter = as.numeric( rownames(convData) )
loadPkg(c('reshape2', 'ggplot2'))
convData = melt(convData, id='iter')
ggplot(convData, aes(x=iter, y=value, color=variable)) + 
	geom_line() + 
	facet_wrap(~variable, scales='free_y')

#gives mean, std dev, and .025,.5,.975 quantiles
outSumm = t(rbind( mean=apply(PS,2,mean), sd=apply(PS,2,sd), 
	apply(PS,2,quantile,probs=c(.025,.5,.975)) ))

# Add var labels
rownames(outSumm)[1:dim(Xd)[3]] = dimnames(Xd)[[3]]
rownames(outSumm)[rownames(outSumm)=='b0'] = '(Intercept)'
rownames(outSumm)[rownames(outSumm)=='bs1'] = colnames(Xs)

# coef summary
outSumm[varKey[-nrow(varKey),1],]