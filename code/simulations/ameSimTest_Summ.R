######
rm(list=ls())
# libs

#helper fns
checkCoverage = function(x){ x[1]<=1 & x[3]>=1 }
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
vars = c('nfit0','nfit1', 'nfitO', 'bfit0', 'bfit1', 'bfitO')
summCoverage = matrix(NA,nrow(NFITO),length(vars), dimnames=list(NULL,vars))
summWidth = matrix(NA,nrow(NFITO),length(vars), dimnames=list(NULL,vars))

# lazy iterate
for(ii in 1:nrow(NFITO)){

	# do conf ints cover oracle mu
	summCoverage[ii,'nfit1'] = checkCoverage(NFIT1[ii,])
	summCoverage[ii,'nfit0'] = checkCoverage(NFIT0[ii,])
	summCoverage[ii,'nfitO'] = checkCoverage(NFITO[ii,])	
	summCoverage[ii,'bfit1'] = checkCoverage(BFIT1[ii,])
	summCoverage[ii,'bfit0'] = checkCoverage(BFIT0[ii,])	
	summCoverage[ii,'bfitO'] = checkCoverage(BFITO[ii,])		

	# conf int width
	summWidth[ii,'nfit0'] = NFIT0[ii,3] - NFIT0[ii,1]
	summWidth[ii,'nfit1'] = NFIT1[ii,3] - NFIT1[ii,1]
	summWidth[ii,'nfitO'] = NFITO[ii,3] - NFITO[ii,1]
	summWidth[ii,'bfit0'] = BFIT0[ii,3] - BFIT0[ii,1]
	summWidth[ii,'bfit1'] = BFIT1[ii,3] - BFIT1[ii,1]
	summWidth[ii,'bfitO'] = BFITO[ii,3] - BFITO[ii,1]		
}

cbind(apply(summCoverage, 2, mean))
cbind(apply(summWidth, 2, mean))
######