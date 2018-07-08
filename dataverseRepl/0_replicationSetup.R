########## LOAD PACKAGES AND SET RANDOM SEED ##########
rm(list=ls())

seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))

# version numbers provided for original paper and version for current model run
loadPkg(
  c(
    'network',      # needed to handle network data; version 1.13.0 # 1.13.0
    'sna',          # descriptive network analysis; version 2.3.2 # 2.4
    'ergm',         # ERGM estimation; tested with version 3.5.1 # 3.8.0
    'latentnet',    # latent space models; tested with version 2.7.1 # 2.7.1
    'texreg',       # generate regression tables; tested with version 1.36.4 # 1.36.23
    'btergm',       # extensions of ERGMs; tested with version 1.7.0 # 1.9.0
    'lme4',         # Random effects logit model; tested with version 1.1.10 # 1.1-13
    'sandwich',     # Huber-White correction; tested with version 2.3.4 # 2.4-0
    'lmtest',       # Robust significance test; tested with version 0.9.34 # 0.9-35
    'gee',          # GEE models; tested with version 4.13.19 # 4.13-19
    'latticeExtra'  # nicer output of MCMC diagnostics; version 0.6.26 # 0.6-28
    )
  )

########## LOAD DATA ##########

# policy forum affiliation data
# 1 = affiliation; 0 = no affiliation
# committee names are in the column labels; actors in the row labels
forum <- as.matrix(read.table(file = paste0(dataPath, "climate0205-committee.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# influence reputation data
# square matrix with influence attribution
# 1 = influential; 0 = not influential
# cells contain the ratings of row actors about column actors
infrep <- as.matrix(read.table(file = paste0(dataPath, "climate0205-rep.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# collaboration; directed network
collab <- as.matrix(read.table(file = paste0(dataPath, "climate0205-collab.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# type of organization; vector with five character types
types <- as.character(read.table(file = paste0(dataPath, "climate0205-type.csv"), 
    header = TRUE, row.names = 1, sep = ";")[, 2])

# alliance-opposition perception; -1 = row actor perceives column actor as 
# an opponent; 1 = row actor perceives column actor as an ally; 0 = neutral
allopp <- as.matrix(read.table(file = paste0(dataPath, "climate0205-allop.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# preference dissimilarity; Manhattan distance over four important policy issues
prefdist <- as.matrix(read.table(file = paste0(dataPath, "climate0205-prefdist.csv"), 
    header = TRUE, row.names = 1, sep = ";"))


########## PREPARE DATA ##########

# apply some changes to the data to make them network-compatible
forum <- forum %*% t(forum)  # compute one-mode projection over forums
diag(forum) <- 0  # the diagonal has no meaning

nw.collab <- network(collab)  # create network object
set.vertex.attribute(nw.collab, "orgtype", types)  # store attributes in network
set.vertex.attribute(nw.collab, "betweenness", betweenness(nw.collab))
set.vertex.attribute(nw.collab, "influence", degree(infrep, cmode = "indegree"))

# endogenous terms as edge covariates
collab.t <- t(collab)  # reciprocal relation
infrep.t <- t(infrep)

# matrix: row actor = environmental group, column = business, or vice-versa
priv.ngo <- matrix(0, nrow = nrow(collab), ncol = ncol(collab))
for (i in 1:nrow(priv.ngo)) {
  for (j in 1:ncol(priv.ngo)) {
    if ((types[i] == "private" && types[j] == "ngo") || 
        (types[i] == "ngo" && types[j] == "private")) {
      priv.ngo[i, j] <- 1
      priv.ngo[j, i] <- 1
    }
  }
}

# create nodal covariate matrices
gov.ifactor <- matrix(rep(1 * (types == "gov"), length(types)), byrow = TRUE, 
    nrow = length(types))
ngo.ofactor <- matrix(rep(1 * (types == "ngo"), length(types)), byrow = FALSE, 
    nrow = length(types))

type.nodematch <- matrix(0, nrow = nrow(collab), ncol = ncol(collab))
for (i in 1:nrow(type.nodematch)) {
  for (j in 1:ncol(type.nodematch)) {
    if (types[i] == types[j]) {
      type.nodematch[i, j] <- 1
    }
  }
}

influence <- degree(infrep, cmode = "indegree")
influence.icov <- matrix(rep(influence, length(influence)), byrow = TRUE, 
    nrow = length(influence))
influence.absdiff <- matrix(0, nrow = nrow(collab), ncol = ncol(collab))
for (i in 1:nrow(influence.absdiff)) {
  for (j in 1:ncol(influence.absdiff)) {
    influence.absdiff[i, j] <- abs(influence[i] - influence[j])
  }
}

# logistic regression without any correction; row random effects
rows <- rep(1:nrow(collab), ncol(collab))
cols <- c(sapply(1:ncol(collab), function(x) rep(x, nrow(collab))))

logit.data <- data.frame(collab = c(collab), prefdist = c(prefdist), 
    ngo.ofactor = c(ngo.ofactor), gov.ifactor = c(gov.ifactor), 
    forum = c(forum), infrep = c(infrep), collab.t = c(collab.t), 
    type.nodematch = c(type.nodematch), priv.ngo = c(priv.ngo), 
    influence.icov = c(influence.icov), influence.absdiff = 
    c(influence.absdiff), allopp = c(allopp), rows = rows, cols = cols)

# covar list
covariates <- list(priv.ngo, allopp, prefdist, forum, infrep, 
    influence.icov, influence.absdiff, gov.ifactor, ngo.ofactor, 
    type.nodematch, collab.t)

#### create data for amen
if( !file.exists(paste0(dataPath, 'data.rda')) ){
  # Build data for amen
  n = length(unique(logit.data$rows))
  Y = matrix(logit.data$collab, nrow=n)
  Xs = matrix(logit.data$ngo.ofactor, nrow=n, ncol=1, dimnames=list(NULL, 'ngo.ofactor'))
  Xr = data.matrix( unique(logit.data[,c('gov.ifactor','influence.icov','cols')])[,-3] ) ; rownames(Xr) = NULL
  dvars=setdiff(names(logit.data), c('collab','ngo.ofactor','gov.ifactor','influence.icov','collab.t','rows','cols'))
  Xd = array(NA, dim=c(n, n, length(dvars)), dimnames=list(NULL,NULL,dvars))

  for(p in 1:dim(Xd)[3]){
    var = dimnames(Xd)[[3]][p]
    toadd = matrix(logit.data[,var], nrow=n)
    Xd[,,var] = toadd }

  save(
    Y, Xd, Xs, Xr, n, 
    logit.data, 
    nw.collab, covariates, 
    file=paste0(dataPath, 'data.rda'))  
}

#### create data with missingness for cross val perf test
if( !file.exists(paste0(dataPath, 'dvForCrossval.rda')) ){
  load(paste0(dataPath, 'data.rda'))
  k=45
  set.seed(6886) ; rpos = sample(1:k, length(collab), replace=TRUE)
  rposmat = matrix(rpos, nrow=nrow(Y), ncol=ncol(Y))
  diag(rposmat) = NA

  yMiss = lapply(1:k, function(x){ tmp=Y ; tmp[which(rposmat==x)]=NA ; return(tmp) })
  nw.collabMiss = lapply(1:k, function(x){
    tmp=collab ; tmp[which(rposmat==x)]=NA
    nwTmp = network(tmp)
    set.vertex.attribute(nwTmp, "orgtype", types)  # store attributes in network
    set.vertex.attribute(nwTmp, "betweenness", betweenness(nw.collab))
    set.vertex.attribute(nwTmp, "influence", degree(infrep, cmode = "indegree"))   
    return(nwTmp)
  })
  yAct = lapply(1:k, function(x){ Y[which(rposmat==x)] })

  save(
    yMiss, nw.collabMiss, yAct, rposmat, # dv info
    collab.t, priv.ngo, forum, infrep, prefdist, allopp, # ergm covars
    gov.ifactor, ngo.ofactor, # lsm covars
    covariates, # qap covars
    logit.data, # glm covars
    seed, # other
    file=paste0(dataPath, 'dvForCrossval.rda')) 
}