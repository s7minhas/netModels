# Last change: 2016-03-25
# 
# Replication data for:
# 
# Cranmer, Skyler J., Philip Leifeld, Scott McClurg, and Meredith Rolfe (2016): 
# Navigating the Range of Statistical Tools for Inferential Network Analysis. 
# American Journal of Political Science.
# 
# Note: The original dataset was collected by Karin Ingold and is publicly 
# available for download at http://dx.doi.org/10.7910/DVN/27427.
# 
# The following analyses were carried out using R version 3.2.4 (2016-03-16).
# 

########## LOAD PACKAGES AND SET RANDOM SEED ##########
rm(list=ls())
library("network")      # needed to handle network data; version 1.13.0
library("sna")          # descriptive network analysis; version 2.3.2
library("ergm")         # ERGM estimation; tested with version 3.5.1
library("latentnet")    # latent space models; tested with version 2.7.1
library("texreg")       # generate regression tables; tested with version 1.36.4
library("btergm")       # extensions of ERGMs; tested with version 1.7.0
library("lme4")         # Random effects logit model; tested with version 1.1.10
library("sandwich")     # Huber-White correction; tested with version 2.3.4
library("lmtest")       # Robust significance test; tested with version 0.9.34
library("gee")          # GEE models; tested with version 4.13.19
library("latticeExtra") # nicer output of MCMC diagnostics; version 0.6.26

seed <- 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
setwd(dataPath)

########## LOAD DATA ##########

# policy forum affiliation data
# 1 = affiliation; 0 = no affiliation
# committee names are in the column labels; actors in the row labels
forum <- as.matrix(read.table(file = "climate0205-committee.csv", 
    header = TRUE, row.names = 1, sep = ";"))

# influence reputation data
# square matrix with influence attribution
# 1 = influential; 0 = not influential
# cells contain the ratings of row actors about column actors
infrep <- as.matrix(read.table(file = "climate0205-rep.csv", 
    header = TRUE, row.names = 1, sep = ";"))

# collaboration; directed network
collab <- as.matrix(read.table(file = "climate0205-collab.csv", 
    header = TRUE, row.names = 1, sep = ";"))

# type of organization; vector with five character types
types <- as.character(read.table(file="climate0205-type.csv", 
    header = TRUE, row.names = 1, sep = ";")[, 2])

# alliance-opposition perception; -1 = row actor perceives column actor as 
# an opponent; 1 = row actor perceives column actor as an ally; 0 = neutral
allopp <- as.matrix(read.table(file = "climate0205-allop.csv", 
    header = TRUE, row.names = 1, sep = ";"))

# preference dissimilarity; Manhattan distance over four important policy issues
prefdist <- as.matrix(read.table(file = "climate0205-prefdist.csv", 
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