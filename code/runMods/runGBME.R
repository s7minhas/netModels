rm(list=ls())
seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')

# load data
load(paste0(dataPath, 'data.rda'))

# mcmc params
imps = 50000000
brn = 10000000
ods = 100
latDims = c(2, 4, 6)
latDims = latDims[1]

# Run gbme
for(latDim in latDims){
	matName = paste0(resultsPath, 'gbmeDir/')
	afile=paste0(matName, 'A_', latDim )
	bfile=paste0(matName, 'B_', latDim )
	ofile=paste0(matName, 'OUT_', latDim )
	source(paste0(gbmePath, 'gbme.asym2.R'))
	ufile=paste0(matName, 'U_', latDim)
	vfile=paste0(matName, 'V_', latDim)

	gbme(Y = Y, Xd=Xd, Xs=Xs, Xr=Xr,
	     fam='binomial', k=latDim, directed=TRUE, 
	     efilename=ufile, ffilename = vfile, 
	     owrite=T, awrite=T, bwrite=T, 
	     ofilename=ofile, afilename=afile, bfilename=bfile,
	     NS = imps, odens=ods, N=matrix(1,n,n)) 
}