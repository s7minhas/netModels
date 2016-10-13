source('~/Research/netModels/code/helpers/paths.R')
load(paste0(dataPath, 'dvForCrossval.rda'))
source(paste0(funcPath, 'functions.R'))
loadPkg(c('doParallel', 'foreach'))

##########
subK = 12
nw.collabMiss = nw.collabMiss[1:subK]
yMiss = yMiss[1:subK]
yAct = yAct[1:subK]
##########

##########
# ergm model
cores = min(8, length(nw.collabMiss))
cl=makeCluster(cores) ; registerDoParallel(cl)
modsErgm = foreach(ii=1:length(nw.collabMiss), .packages=c('ergm','sna') ) %dopar% {
    load(paste0(dataPath, 'dvForCrossval.rda'))

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
        eval.loglik = TRUE, check.degeneracy = TRUE, 
        # control = control.ergm(seed = seed, MCMC.samplesize = 5000, MCMC.interval = 5000) )
        # control = control.ergm(seed = seed, MCMC.samplesize = 1000, MCMC.interval = 1000) )    
        control = control.ergm(seed = seed) )
    return(mod)
}
stopCluster(cl)
save(modsErgm, file=paste0(resultsPath, 'ergmOutPerfResults.rda'))
rm(list='modsErgm')
##########

# ##########
# # latentnet mod
# cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
# modsLs = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet','sna') ) %dopar% {
#     load(paste0(dataPath, 'dvForCrossval.rda'))
    
#     dv = nw.collabMiss[[ii]]
#     mod <- ergmm(dv ~ 
#         euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
#         edgecov(gov.ifactor) + 
#         edgecov(ngo.ofactor) + 
#         nodematch("orgtype") + 
#         edgecov(priv.ngo) + 
#         edgecov(forum) + 
#         edgecov(infrep) + 
#         nodeicov("influence") + 
#         absdiff("influence") + 
#         edgecov(prefdist) + 
#         edgecov(allopp), 
#         seed = seed, 
#         control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
#     return(mod)
# }
# stopCluster(cl)
# save(modsLs, file=paste0(resultsPath, 'euclLatSpaceOutPerfResults.rda'))
# rm(list='modsLs')

# cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
# modsLsBil = foreach(ii=1:length(nw.collabMiss), .packages=c('latentnet','sna') ) %dopar% {
#     load(paste0(dataPath, 'dvForCrossval.rda'))
    
#     dv = nw.collabMiss[[ii]]
#     mod <- ergmm(dv ~ 
#         bilinear(d = 2, G = 0) +  # 2 dimensions and 0 clusters
#         edgecov(gov.ifactor) + 
#         edgecov(ngo.ofactor) + 
#         nodematch("orgtype") + 
#         edgecov(priv.ngo) + 
#         edgecov(forum) + 
#         edgecov(infrep) + 
#         nodeicov("influence") + 
#         absdiff("influence") + 
#         edgecov(prefdist) + 
#         edgecov(allopp), 
#         seed = seed, 
#         control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100) )
#     return(mod)
# }
# stopCluster(cl)
# save(modsLsBil, file=paste0(resultsPath, 'bilLatSpaceOutPerfResults.rda'))
# rm(list='modsLsBil')
# ##########

# ##########
# # mrqap
# cl=makeCluster(length(nw.collabMiss)) ; registerDoParallel(cl)
# modsQap = foreach(ii=1:length(nw.collabMiss), .packages=c('sna') ) %dopar% {
#     load(paste0(dataPath, 'dvForCrossval.rda'))
    
#     dv = nw.collabMiss[[ii]]
#     mod <- netlogit(dv, covariates, reps = 200, nullhyp = "qap")
#     return(mod)
# }
# stopCluster(cl)
# save(modsQap, file=paste0(resultsPath, 'qapOutPerfResults.rda'))
# rm(list='modsQap')
# ##########

# ##########
# # logit mod
# cl=makeCluster(length(yMiss)) ; registerDoParallel(cl)
# modsLogit = foreach(ii=1:length(yMiss) ) %dopar% {
#     load(paste0(dataPath, 'dvForCrossval.rda'))

#     dv = yMiss[[ii]]
#     logit.data$dv = c(dv)
#     mod <- glm(dv ~ priv.ngo + allopp + prefdist + forum + infrep + 
#         influence.icov + influence.absdiff + gov.ifactor + ngo.ofactor + type.nodematch + collab.t, 
#         data = logit.data, family = "binomial")
#     return(mod)
# }
# stopCluster(cl)
# save(modsLogit, file=paste0(resultsPath, 'logitOutPerfResults.rda'))
# rm(list='modsLogit')
# ##########