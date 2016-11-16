source('replicationSetup.R')

########## LATENT SPACE MODEL ##########

# Latent space model: collaboration
set.seed(seed)
model.lsBil <- ergmm(nw.collab ~ 
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
    control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
)

# goodness of fit assessment for the latent space model
gof.lsBil <- gof.ergmm(model.lsBil, 
    GOF = ~ dspartners + espartners + distance + idegree + odegree, 
    control = control.gof.ergmm(seed = seed))

# sender/receiver random effects
set.seed(seed)
model.lsBilSR <- ergmm(nw.collab ~ 
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
    control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
)

# goodness of fit assessment for the latent space model
gof.lsBilSR <- gof.ergmm(model.lsBilSR, 
    GOF = ~ dspartners + espartners + distance + idegree + odegree, 
    control = control.gof.ergmm(seed = seed))

save(
    model.lsBil, gof.lsBil, 
    model.lsBilSR, gof.lsBilSR,     
    file=paste0(resultsPath, 'bilLatSpaceResults.rda')
    )