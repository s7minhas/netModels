######
rm(list=ls())
# libs

#helper fns
checkOverlap = function(x,y){ x[1] <= y[3] && y[1] <= x[3] }
######

######
dPath = '~/Dropbox/Research/netModels/'
dataPath = paste0(dPath, "data/")
resultsPath = paste0(dPath, 'rdaResults/')
graphicsPath = paste0(dPath, 'summResults/')

load(paste0(resultsPath, 'ameSimTest_100.rda'))
######

######
# 
vars = c('nfit0','nfit1','bfit0', 'bfit1')
summ = matrix(NA,nrow(NFITO),4, dimnames=list(NULL,vars))

for(ii in 1:nrow(NFITO)){
	summ[ii,'nfit1'] = checkOverlap(NFIT1[ii,],NFITO[ii,])
	summ[ii,'nfit0'] = checkOverlap(NFIT0[ii,],NFITO[ii,])

	summ[ii,'bfit1'] = checkOverlap(BFIT1[ii,],BFITO[ii,])
	summ[ii,'bfit0'] = checkOverlap(BFIT0[ii,],BFITO[ii,])
}

apply(summ,2,mean)
######