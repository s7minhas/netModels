source('~/Research/netModels/code/helpers/paths.R')
load(paste0(dataPath, 'dvForCrossval.rda'))
source(paste0(funcPath, 'functions.R'))

loadPkg(c('doParallel', 'foreach'))
cores = min(8, length(nw.collabMiss))

##########
subK = 16
nw.collabMiss = nw.collabMiss[1:subK]
yMiss = yMiss[1:subK]
yAct = yAct[1:subK]
##########

##########
# ergm model
cl=makeCluster(cores) ; registerDoParallel(cl)
modsErgm = foreach(ii=1:length(nw.collabMiss), .packages=c('ergm','sna') ) %dopar% {

    dv = nw.collabMiss[[ii]]
    mod <- ergm(dv ~ 
        edges + 
        edgecov(collab.t) + 
        nodeifactor("orgtype", base = -1) + 
        nodeofactor("orgtype", base = -2) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp) + 
        odegreepopularity + 
        twopath + 
        gwidegree(2, fixed = TRUE) + 
        gwesp(1, fixed = TRUE) + 
        gwodegree(0.5, fixed = TRUE),
        eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm(seed = seed) )
    sim = simulate.ergm(mod,nsim=1000)
    probs = Reduce('+', lapply(sim, as.sociomatrix) ) / length(sim)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,sim=sim,pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsErgm, file=paste0(resultsPath, 'ergmOutPerfResults.rda'))
rm(list='modsErgm')
##########

#########
latentnet mod
cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
modsLs = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet') ) %dopar% {
    fergmm = TRUE ; source(paste0(gPath, 'code/replicationSetup.R'))

    dv = nw.collabMiss[[ii]]
    mod <- ergmm(dv ~ 
        euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
        edgecov(gov.ifactor) + 
        edgecov(ngo.ofactor) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp), 
        seed = seed, 
        control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
    probs = predict(mod)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,pred=pred)  
    return(summ)
}
stopCluster(cl)
save(modsLs, file=paste0(resultsPath, 'euclLatSpaceOutPerfResults.rda'))
rm(list='modsLs')

cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
modsLsBil = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet','sna') ) %dopar% {
    fergmm = TRUE ; source(paste0(gPath, 'code/replicationSetup.R'))
    
    dv = nw.collabMiss[[ii]]
    mod <- ergmm(dv ~ 
        bilinear(d = 2, G = 0) +  # 2 dimensions and 0 clusters
        edgecov(gov.ifactor) + 
        edgecov(ngo.ofactor) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp), 
        seed = seed, 
        control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
    probs = predict(mod)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,pred=pred)  
    return(summ)
}
stopCluster(cl)
save(modsLsBil, file=paste0(resultsPath, 'bilLatSpaceOutPerfResults.rda'))
rm(list='modsLsBil')
##########

##########
# mrqap
cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
modsQap = foreach(ii=1:length(nw.collabMiss), .packages=c('sna') ) %dopar% {

    dv = nw.collabMiss[[ii]]
    mod <- netlogit(dv, covariates, reps = 200, nullhyp = "qap")
    beta = mod$coefficients
    outDesignArr = cbind(1, do.call('cbind', lapply(covariates, function(x){ x[which(rposmat==ii)] }) ))
    yhat = outDesignArr %*% beta
    prob = 1/(1+exp(-yhat))
    pred=data.frame(prob=prob, actual=yAct[[ii]])
    summ=list(mod=mod, pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsQap, file=paste0(resultsPath, 'qapOutPerfResults.rda'))
rm(list='modsQap')
##########

##########
# logit mod
cl=makeCluster(length(yMiss)) ; registerDoParallel(cl)
modsLogit = foreach(ii=1:length(yMiss) ) %dopar% {

    dv = yMiss[[ii]]
    logit.data$dv = c(dv)
    mod <- glm(dv ~ priv.ngo + allopp + prefdist + forum + infrep + 
        influence.icov + influence.absdiff + gov.ifactor + ngo.ofactor + type.nodematch + collab.t, 
        data = logit.data, family = "binomial")
    beta = mod$coefficients
    outDesignArr = cbind(1, logit.data[which(rposmat==ii),names(mod$'coefficients')[2:length(mod$'coefficients')]])
    yhat = data.matrix(outDesignArr) %*% beta
    prob = 1/(1+exp(-yhat))
    pred=data.frame(prob=prob, actual=yAct[[ii]])
    summ=list(mod=mod, pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsLogit, file=paste0(resultsPath, 'logitOutPerfResults.rda'))
rm(list='modsLogit')
##########

##########
# amen
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
load(paste0(dataPath, 'data.rda'))
imps = 100000
brn = 50000
ods = 10

latDims = 2
cl=makeCluster(length(yMiss)) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=latDims, 
        nscan=imps, seed=seed, burn=brn, odens=ods, 
        rvar=TRUE, cvar=TRUE,  # row and column effects, standard in srm
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = 1/(1+exp(-preds))
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsAme, file=paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda'))
rm(list='modsAme')

##########
# other ame parameterizations
latDims = 1
cl=makeCluster(length(yMiss)) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=latDims, 
        nscan=imps, seed=seed, burn=brn, odens=ods, 
        rvar=TRUE, cvar=TRUE,  # row and column effects, standard in srm
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = 1/(1+exp(-preds))
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsAme, file=paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda'))
rm(list='modsAme')

latDims = 3
cl=makeCluster(length(yMiss)) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=latDims, 
        nscan=imps, seed=seed, burn=brn, odens=ods, 
        rvar=TRUE, cvar=TRUE,  # row and column effects, standard in srm
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = 1/(1+exp(-preds))
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsAme, file=paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda'))
rm(list='modsAme')

latDims = 4
cl=makeCluster(length(yMiss)) ; registerDoParallel(cl)
modsAme = foreach(ii=1:length(yMiss), .packages=c('amen') ) %dopar% {

    dv = yMiss[[ii]]
    ameFit = ameTest(Y=dv, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=latDims, 
        nscan=imps, seed=seed, burn=brn, odens=ods, 
        rvar=TRUE, cvar=TRUE,  # row and column effects, standard in srm
        plot=FALSE, print=FALSE) 
    preds = ameFit$'EZ'[which(rposmat==ii)]
    prob = 1/(1+exp(-preds))
    pred = data.frame(prob=prob, actual=yAct[[ii]])
    summ = list(mod=ameFit, pred=pred)
    return(summ)
}
stopCluster(cl)
save(modsAme, file=paste0(resultsPath, 'ameFitSR_', latDims, '_outPerfResults.rda'))
rm(list='modsAme')
##########

##########
# other latentnet parameterizations

# latentnet eucl mod with s + r
cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
modsLsSR = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet') ) %dopar% {
    fergmm = TRUE ; source(paste0(gPath, 'code/replicationSetup.R'))

    dv = nw.collabMiss[[ii]]
    mod <- ergmm(dv ~ 
        euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
        rsender(var=1, var.df=3) + 
        rreceiver(var=1, var.df=3) +         
        edgecov(gov.ifactor) + 
        edgecov(ngo.ofactor) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp), 
        seed = seed, 
        control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
    probs = predict(mod)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,pred=pred)  
    return(summ)
}
stopCluster(cl)
save(modsLsSR, file=paste0(resultsPath, 'euclSenRecLatSpaceOutPerfResults.rda'))
rm(list='modsLsSR')

cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
modsLsBilSR = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet','sna') ) %dopar% {
    fergmm = TRUE ; source(paste0(gPath, 'code/replicationSetup.R'))
    
    dv = nw.collabMiss[[ii]]
    mod <- ergmm(dv ~ 
        bilinear(d = 2, G = 0) +  # 2 dimensions and 0 clusters
        rsender(var=1, var.df=3) + 
        rreceiver(var=1, var.df=3) +                 
        edgecov(gov.ifactor) + 
        edgecov(ngo.ofactor) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp), 
        seed = seed, 
        control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
    probs = predict(mod)
    oProbs = probs[which(rposmat==ii)]
    pred = data.frame(prob=oProbs, actual=yAct[[ii]])
    summ=list(mod=mod,pred=pred)  
    return(summ)
}
stopCluster(cl)
save(modsLsBilSR, file=paste0(resultsPath, 'bilSenRecLatSpaceOutPerfResults.rda'))
rm(list='modsLsBilSR')
##########