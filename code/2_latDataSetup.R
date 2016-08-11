source('~/Research/netModels/code/paths.R')

load(paste0(resultsPath, 'logitResults.rda'))
n = length(unique(logit.data$rows))
Y = matrix(logit.data$collab, nrow=n)
Xs = Xr = matrix(logit.data$ngo.ofactor, nrow=n, ncol=1, dimnames=list(NULL, 'ngo.ofactor'))
dvars=setdiff(names(logit.data), c('collab','ngo.ofactor','rows','cols'))
Xd = array(NA, dim=c(n, n, length(dvars)), dimnames=list(NULL,NULL,dvars))

for(ii in 1:dim(Xd)[3]){
  var = dimnames(Xd)[[3]][ii]
  toadd = matrix(logit.data[,var], nrow=n)
  Xd[,,var] = toadd
}

save(Y, Xd, Xs, Xr, n, file=paste0(dataPath, 'latFacData.rda'))