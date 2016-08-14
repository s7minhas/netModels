source('~/Research/netModels/code/replicationSetup.R')

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
gof.ls <- gof.ergmm(model.ls, GOF = ~ dspartners + espartners + distance + 
    idegree + odegree, control = control.gof.ergmm(seed = seed))

pdf(paste0(graphicsPath, "gof-ls.pdf"), width = 9, height = 6)
par(mfrow = c(2, 3))
plot(gof.ls, main = "Latent space model: goodness of fit")
set.seed(seed)
plot(model.ls, labels = TRUE, print.formula = FALSE, 
    main = "MKL Latent positions")
dev.off()

save(model.ls, gof.ls, file=paste0(resultsPath, 'euclLatSpaceResults.rda'))