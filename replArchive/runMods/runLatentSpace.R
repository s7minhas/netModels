source('replicationSetup.R')

########## LATENT SPACE MODEL ##########

# Latent space model: collaboration
set.seed(seed)
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

# goodness of fit assessment for the latent space model
gof.ls <- gof.ergmm(model.ls, 
    GOF = ~ dspartners + espartners + distance + idegree + odegree, 
    control = control.gof.ergmm(seed = seed))

# sender/receiver random effects
model.lsSR <- ergmm(nw.collab ~ 
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
    control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
)

# goodness of fit assessment for the latent space model
gof.lsSR <- gof.ergmm(model.lsSR, 
    GOF = ~ dspartners + espartners + distance + idegree + odegree, 
    control = control.gof.ergmm(seed = seed))

save(
    model.ls, gof.ls, 
    model.lsSR, gof.lsSR, 
    file=paste0(resultsPath, 'euclLatSpaceResults.rda')
    )