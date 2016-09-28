rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg(c('amen'))

tmp = as.matrix(read.table(file = paste0(dataPath, "climate0205-collab.csv"), 
	header = TRUE, row.names = 1, sep = ";"))
load(paste0(dataPath, 'data.rda'))
rownames(Y) = colnames(Y) = rownames(tmp)

load(paste0(resultsPath, 'ameFitSR_2.rda'))
circplot(Y=Y, U=ameFit$'U', V=ameFit$'V')