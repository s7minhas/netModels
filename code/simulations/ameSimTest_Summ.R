######
rm(list=ls())
# libs

#helper fns
checkOverlap = function(x,y){ x[1] <= y[3] && y[1] <= x[3] }
checkCoverage = function(x,y){ y[2]>=x[1] & y[2]<=x[3] }
######

######
dPath = '~/Dropbox/Research/netModels/'
dataPath = paste0(dPath, "data/")
resultsPath = paste0(dPath, 'rdaResults/')
graphicsPath = paste0(dPath, 'summResults/')

# load(paste0(resultsPath, 'ameSimTest_100.rda'))
load(paste0(resultsPath, 'ameSimTest.rda')) # 1k
######

######
# 
vars = c('nfit0','nfit1','bfit0', 'bfit1')
summOverlap = matrix(NA,nrow(NFITO),length(vars), dimnames=list(NULL,vars))
summCoverage = matrix(NA,nrow(NFITO),length(vars), dimnames=list(NULL,vars))

vars = c('nfit0','nfit1', 'nfitO', 'bfit0', 'bfit1', 'bfitO')
summWidth = matrix(NA,nrow(NFITO),length(vars), dimnames=list(NULL,vars))

# lazy iterate
for(ii in 1:nrow(NFITO)){
	# do conf ints overlap
	summOverlap[ii,'nfit1'] = checkOverlap(NFIT1[ii,],NFITO[ii,])
	summOverlap[ii,'nfit0'] = checkOverlap(NFIT0[ii,],NFITO[ii,])
	summOverlap[ii,'bfit1'] = checkOverlap(BFIT1[ii,],BFITO[ii,])
	summOverlap[ii,'bfit0'] = checkOverlap(BFIT0[ii,],BFITO[ii,])

	# do conf ints cover oracle mu
	summCoverage[ii,'nfit1'] = checkCoverage(NFIT1[ii,],NFITO[ii,])
	summCoverage[ii,'nfit0'] = checkCoverage(NFIT0[ii,],NFITO[ii,])
	summCoverage[ii,'bfit1'] = checkCoverage(BFIT1[ii,],BFITO[ii,])
	summCoverage[ii,'bfit0'] = checkCoverage(BFIT0[ii,],BFITO[ii,])	

	# conf int width
	summWidth[ii,'nfit0'] = NFIT0[ii,3] - NFIT0[ii,1]
	summWidth[ii,'nfit1'] = NFIT1[ii,3] - NFIT1[ii,1]
	summWidth[ii,'nfitO'] = NFITO[ii,3] - NFITO[ii,1]
	summWidth[ii,'bfit0'] = BFIT0[ii,3] - BFIT0[ii,1]
	summWidth[ii,'bfit1'] = BFIT1[ii,3] - BFIT1[ii,1]
	summWidth[ii,'bfitO'] = BFITO[ii,3] - BFITO[ii,1]		
}

cbind(apply(summOverlap,2,mean))
cbind(apply(summCoverage, 2, mean))
cbind(apply(summWidth, 2, mean))
######