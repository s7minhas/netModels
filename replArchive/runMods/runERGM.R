source('replicationSetup.R')

########## ERGM ##########

# ERGM: collaboration
model.ergm <- ergm(nw.collab ~ 
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
    control = control.ergm(seed = seed, MCMC.samplesize = 5000, 
    MCMC.interval = 5000)
)

# For tie predictions
ergmSims = simulate.ergm(model.ergm,nsim=1000)

# coef results
ergmCoef = summary(model.ergm)$'coefs'

# ERGM goodness of fit
gof.ergm <- gof(model.ergm, MCMC.burnin = 30000, MCMC.interval = 10000, 
    statistics = c(dsp, esp, geodesic, ideg, odeg, istar))

save(model.ergm, ergmSims, ergmCoef, gof.ergm, file=paste0(resultsPath, 'ergmResults.rda'))