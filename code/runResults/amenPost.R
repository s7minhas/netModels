rm(list=ls())
seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg('amen')

# ame results
load(paste0(resultsPath, 'ameFit_2.rda'))

# load data
load(paste0(dataPath, 'latFacData.rda'))

# conv check
convData = data.frame( ameFit$'BETA', stringsAsFactors=FALSE)
convData$iter = as.numeric( rownames(convData) )
loadPkg(c('reshape2', 'ggplot2'))
convData = melt(convData, id='iter')
ggplot(convData, aes(x=iter, y=value, color=variable)) + 
	geom_line() + 
	facet_wrap(~variable, scales='free_y')

# coef
ameCoef = getAmeCoef(ameFit)
ameVar = getAmeVar(ameFit)
# ordering of vars
load(paste0(resultsPath, 'logitResults.rda'))
vars = names(coef(model.logit))

# perf
GOF = ameFit$'GOF'
par(mfrow=c(2,2))
for (k in 1:4) {
	hist(GOF[-1, k], xlim = range(GOF[, k]), 
	  main = "", prob = TRUE, xlab = colnames(GOF)[k], 
	  col = "lightblue", ylab = "", yaxt = "n")
	abline(v = GOF[1, k], col = "red")
	abline(v=quantile(GOF[-1,k], c(0.025)), col='black')
	abline(v=quantile(GOF[-1,k], c(0.975)), col='black')
}