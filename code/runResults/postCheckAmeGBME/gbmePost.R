rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg(c('latentnet','amen'))

# load data
load(paste0(dataPath, 'data.rda'))

############################################################
# load gbme results
out = read.table(paste0(resultsPath, 'gbmeDir/OUT_2'), header=TRUE)
PS = out[out$scan>round(max(out$scan)/2),-(1:3)] 

# Check convergence
convData = data.frame( PS[,grepl('bd[1-9]|bs[1-9]|br[1-9]|b0',names(PS))], stringsAsFactors=FALSE)
convData$iter = as.numeric( rownames(convData) )

# add var names
names(convData)[grepl('bd[1-9]',names(convData))] = dimnames(Xd)[[3]]
names(convData)[grepl('bs[1-9]',names(convData))] = colnames(Xs)
names(convData)[grepl('br[1-9]',names(convData))] = colnames(Xr)
names(convData)[grepl('b0',names(convData))] = '(Intercept)'

# calc summary stats
summ = function(x){ c( mu=mean(x),med=median(x),quantile(x,c(0.025,0.975)) )  }
summConv = data.frame( t(apply(convData[,-ncol(convData)], 2, summ)) )
summConv$var = rownames(summConv) ; rownames(summConv) = NULL ; names(summConv)[3:4] = c('lo95','hi95')
# save summary stats for use in lat space comparison
save(summConv, file=paste0(resultsPath,'gbmePost.rda'))

# get hpd interval
loadPkg('coda')
convCoda = mcmc(convData[,-ncol(convData)], start=(25001*100), end=(50000*100), thin=100)
summConv = cbind(summConv, matrix(HPDinterval(convCoda),ncol=2,dimnames=list(NULL,c('hpdLo','hpdHi'))) )

# combo
ggConvData = melt(convData, id='iter')
for(v in names(summConv)[c(1:4,6:7)]){
	ggConvData$tmp = summConv[,v][match(ggConvData$var,summConv$var)]
	names(ggConvData)[ncol(ggConvData)] = v }

# density
ggDens = ggplot(ggConvData, aes(value)) + 
	geom_density() + 
	geom_vline(aes(xintercept=mu), color='red') + 
	geom_vline(aes(xintercept=med), color='blue') + 	
	geom_vline(aes(xintercept=lo95), color='grey') + 	
	geom_vline(aes(xintercept=hi95), color='grey') + 			
	geom_vline(aes(xintercept=hpdLo), color='black') + 	
	geom_vline(aes(xintercept=hpdHi), color='black') + 				
	facet_wrap(~variable, scales='free',nrow=6,ncol=2)
ggDens

# trace
ggConv = ggplot(ggConvData, aes(x=iter, y=value)) + 
	geom_line() + 
	facet_wrap(~variable, scales='free_y',nrow=6,ncol=2)
ggConv

# Check conv using coda
geweke.diag(convCoda)
heidel.diag(convCoda)
raftery.diag(convCoda)

#gives mean, std dev, and .025,.5,.975 quantiles
outSumm = t(rbind( mean=apply(PS,2,mean), sd=apply(PS,2,sd), 
	apply(PS,2,quantile,probs=c(.025,.5,.975)) ))

# Add var labels
rownames(outSumm)[1:dim(Xd)[3]] = dimnames(Xd)[[3]]
rownames(outSumm)[rownames(outSumm)=='b0'] = '(Intercept)'
rownames(outSumm)[rownames(outSumm)=='bs1'] = colnames(Xs)	
############################################################