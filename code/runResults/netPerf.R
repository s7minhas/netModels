rm(list=ls())
source('~/Research/netModels/code/replicationSetup.R')

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# generate gof stats

# dyad-wise shared partners
dspAct = btergm::dsp(collab)/sum( btergm::dsp(collab) )

# edge-wise shared partners
espAct = btergm::esp(collab) / sum( btergm::esp(collab) )

# geodesic distances
geoAct = btergm::geodesic(collab) / sum( btergm::geodesic(collab) )

# indegree
idegAct = btergm::ideg(collab) / sum( btergm::ideg(collab) )

# outdegree
odegAct = btergm::odeg(collab) / sum( btergm::odeg(collab) )

# 
################################################

################################################
# Logit Mod
load(paste0(resultsPath, 'logitResults.rda'))
set.seed(seed)
logitGof = btergm::gof(collab, covariates, coef(model.logit), 
	statistics = c(dsp, esp, geodesic, ideg, odeg, istar))



# ERGM
load(paste0(resultsPath, 'ergmResults.rda'))
set.seed(seed)
ergmGof = gof(model.ergm, MCMC.burnin = 30000, MCMC.interval = 10000, nsim=100, 
	statistics = c(dsp), seed=seed)
	# statistics = c(dsp, esp, geodesic, ideg, odeg, istar), seed=seed)

# For tie predictions
set.seed(seed)
ergmSims = simulate.ergm(model.ergm,nsim=100, seed=seed)
# tmp = lapply(ergmSims, function(x){ dsp(as.sociomatrix(x))/sum( dsp(as.sociomatrix(x)) )  } )
tmp = lapply(ergmSims, function(x){ dsp(as.sociomatrix(x))  } )
tmp2 = do.call('cbind',tmp)

ugh = unlist( lapply(tmp, function(x) {x[1]}) )
cbind(
	c(mu=mean(ugh), med=median(ugh), min=min(ugh), max=max(ugh) ),
	matrix(ergmGof[[1]]$'stats'[1,2:5])
	)

# Latent space - eucl
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
lsEuclGof = gof.ergmm(model.ls, 
	GOF = ~ dspartners + espartners + distance + idegree + odegree, 
	control = control.gof.ergmm(seed = seed))

# QAP
load(paste0(resultsPath, 'qapResults.rda'))
qapGof = gof(collab, covariates, coef(model.qap), 
	statistics = c(dsp, esp, geodesic, ideg, odeg, istar))

# Amen
load(paste0(resultsPath, 'ameFit_2.rda'))
################################################