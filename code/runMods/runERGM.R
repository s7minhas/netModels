source('~/Research/netModels/code/1_replicationSetup.R')

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
summary(model.ergm)
save(model.ergm, file=paste0(resultsPath, 'ergmResults.rda'))
load(paste0(resultsPath, 'ergmResults.rda'))

# MCMC diagnostics plot (not included in the paper)
pdf(paste0(graphicsPath, "ergm-mcmc-diagnostics.pdf"))
mcmc.diagnostics(model.ergm)
dev.off()

# ERGM goodness of fit
gof.ergm <- gof(model.ergm, MCMC.burnin = 30000, MCMC.interval = 10000, 
    statistics = c(dsp, esp, geodesic, ideg, odeg, istar))
pdf(paste0(graphicsPath, "gof-ergm.pdf"), width = 9, height = 6)
plot(gof.ergm)
dev.off()