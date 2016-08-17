source('~/Research/netModels/code/replicationSetup.R')

########## LATENT SPACE MODEL ##########

# Latent space model: collaboration
set.seed(seed)
# model.ls <- ergmm(nw.collab ~ 
#     euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
#     edgecov(gov.ifactor) + 
#     edgecov(ngo.ofactor) + 
#     nodematch("orgtype") + 
#     edgecov(priv.ngo) + 
#     edgecov(forum) + 
#     edgecov(infrep) + 
#     nodeicov("influence") + 
#     absdiff("influence") + 
#     edgecov(prefdist) + 
#     edgecov(allopp), 
#     seed = seed, 
#     control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
# )

# # goodness of fit assessment for the latent space model
# gof.ls <- gof.ergmm(model.ls, GOF = ~ dspartners + espartners + distance + 
#     idegree + odegree, control = control.gof.ergmm(seed = seed))

# pdf(paste0(graphicsPath, "gof-ls.pdf"), width = 9, height = 6)
# par(mfrow = c(2, 3))
# plot(gof.ls, main = "Latent space model: goodness of fit")
# set.seed(seed)
# plot(model.ls, labels = TRUE, print.formula = FALSE, 
#     main = "MKL Latent positions")
# dev.off()

# Extend chain due to warning meassage about too few acceptances
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
loadPkg('snowFT')
model.ls2 <- ergmm(nw.collab ~ 
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
    prior = model.ls$'prior', 
    control = control.ergmm(sample.size = 30000, burnin = 30000000, interval = 100, threads=3)
)

# goodness of fit assessment for the latent space model
gof.ls2 <- gof.ergmm(model.ls2, GOF = ~ dspartners + espartners + distance + 
    idegree + odegree, control = control.gof.ergmm(seed = seed))

pdf(paste0(graphicsPath, "gof-ls2.pdf"), width = 9, height = 6)
par(mfrow = c(2, 3))
plot(gof.ls2, main = "Latent space model: goodness of fit")
set.seed(seed)
plot(model.ls2, labels = TRUE, print.formula = FALSE, 
    main = "MKL Latent positions")
dev.off()

save(model.ls, gof.ls, model.ls2, gof.ls2, file=paste0(resultsPath, 'euclLatSpaceResults.rda'))