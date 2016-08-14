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
ggConvData = melt(convData, id='iter')
ggConv = ggplot(ggConvData, aes(x=iter, y=value, color=variable)) + 
	geom_line() + 
	facet_wrap(~variable, scales='free_y')

# Check conv using coda
loadPkg('coda')
convCoda = mcmc(convData[,-ncol(convData)], start=50001, end=100000, thin=10)
geweke.diag(convCoda)
heidel.diag(convCoda)
raftery.diag(convCoda)
HPDinterval(convCoda)

# coef
ameCoef = getAmeCoef(ameFit)
ameVar = getAmeVar(ameFit)
# ordering of vars
ameCoef[c('(Intercept)',varKey[-nrow(varKey),1]),]

# in sample fit
rownames(Y) = char(1:34)
colnames(Y) = char(1:34)
diag(Y) = NA
actual = melt(Y)

preds = ameFit$'EZ'
diag(preds) = NA
rownames(preds) = char(1:34)
colnames(preds) = char(1:34)
loadPkg('reshape2')
amePred = melt(preds)
amePred$prob = 1/(1+exp(-amePred$value))
amePred$actual = actual$value
amePred = na.omit(amePred)

# loadPkg('separationplot')
# separationplot(amePred$prob, amePred$actual)

source(paste0(funcPath, 'binPerfHelpers.R'))
getAUC(amePred$prob, amePred$actual)
auc_pr(amePred$actual, amePred$prob)

# perf built into amen
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

# perf used in paper
