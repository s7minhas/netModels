##############################
rm(list=ls())

if(Sys.info()['user']=='s7m'){
    fPath = '~/Research/netModels/code/helpers/'
    dPath = '~/Dropbox/Research/netModels/'
    simResPath = paste0(dPath, 'simulations/')
    source(paste0(fPath, 'functions.R'))
    source(paste0(fPath, 'paths.R'))
}

toLoad = c(
    'devtools', 
    'foreach', 'doParallel',
    'magrittr', 'dplyr', 'ggplot2'
    )
loadPkg(toLoad)
# devtools::install_github('s7minhas/amen', ref='dev')
loadPkg(c('latentnet', 'amen'))
##############################
seed=6886 ; n = 30 ; mu = -2 ; beta=1 ; gamma=1 ; model = 'lsm'
##############################
# simRun = function(seed, n, mu, beta, gamma, model='lfm'){
    set.seed(seed) 
    xw = matrix(rnorm(n*2),n,2)
    X = tcrossprod(xw[,1])  ; W = tcrossprod(xw[,2])
    XW = array(dim=c(n,n,2)) ; XW[,,1] = X ; XW[,,2] = W

    # create DV
    Y<- 1*(mu + beta*X + gamma*W + matrix(rnorm(n*n),n,n) >0)

    # run models
    # # run AME models
    # if(model == 'lfm'){
    #     fit1 = ame(Y,X,R=1,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
    #     print=FALSE,plot=FALSE)
    #     fitO = ame(Y,XW,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
    #     print=FALSE,plot=FALSE)
    #     # gather together results
    #     beta = list(ame=fit1$BETA, oracle=fitO$BETA)
    #     return(out) }

    # run LDM models
    # if(model == 'lsm'){
    yNet <- network(Y, directed = TRUE, )

    fit1 = ergmm(Y ~ edgecov(X) + euclidean(d=2, G=0),
        seed=seed,
        control = control.ergmm(
            # sample.size = 10000, burnin = 500, interval = 25) ) 
            sample.size = 10, burnin = 5, interval = 1) ) 
    # gather together results
    beta = summary(model.ls)$'pmean'$'coef.table'
    }
}
##############################

##############################
# params
imps = 50
intEff=-2 ; x1Eff=1 ; x2Eff=1

#
writeLines(c('n=50 using 8 cores\n'), paste0(simResPath,'ameSim50Log.txt'))
cl=makeCluster(8) ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
out=simRun(seed=imp, n=50, mu=intEff, beta=x1Eff, gamma=x2Eff)
sink(paste0(simResPath,'ameSim50Log.txt'), append=TRUE)
cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50.rda') )

#
writeLines(c('n=100 using 8 cores\n'), paste0(simResPath,'ameSim100Log.txt'))
cl=makeCluster(8) ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
out=simRun(seed=imp, n=100, mu=intEff, beta=x1Eff, gamma=x2Eff)
sink(paste0(simResPath,'ameSim100Log.txt'), append=TRUE)
cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100.rda') )


save(ameSim50, ameSim100, file=paste0(simResPath, 'ameSim.rda'))
##############################

library(latentnet)
set.seed(seed)

lsmSim100 = foreach(imp = 1:imps, .packages=c('latentnet')) %dopar% {
}


model.ls <- ergmm(nw.collab ~ 
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
    control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
)
