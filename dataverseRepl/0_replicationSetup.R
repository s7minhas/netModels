#set up workspace ################### 
if( !exists('pergmm')  ){ rm(list=ls()) } 

seed <- 12345
set.seed(seed)
# # example linux path
# mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/Research/netModels/dataverseRepl/' 

# other necessary libraries
oPkgs = c(
  'network', 'sna', 'ergm', 'latentnet', 'btergm',
  #'Cairo',
  'reshape2','plyr','ggplot2','latex2exp',
  'xtable','ROCR','caTools',
  'RColorBrewer', 'png', 'grid')
for(pkg in oPkgs){
  if(!pkg %in% installed.packages())
    install.packages(pkg, repos="https://cloud.r-project.org") }

# match versions as closely as possible to cranmer et al. 2017
iPkgs = apply(installed.packages()[,c('Package','Version')], 1, function(x){paste0(x[1],x[2])})
if(!('network1.13.0' %in% iPkgs)){
    install.packages(paste0(mainPath, 'pkgs/network_1.13.0.tar.gz'), repos = NULL, type="source") }
if(!('sna2.4' %in% iPkgs)){
    install.packages(paste0(mainPath, 'pkgs/sna_2.4.tar.gz'), repos = NULL, type="source") }    
if(!('ergm3.8.0' %in% iPkgs)){
    install.packages(paste0(mainPath, 'pkgs/ergm_3.8.0.tar.gz'), repos = NULL, type="source") }
if(!('latentnet2.7.1' %in% iPkgs)){
    install.packages(paste0(mainPath, 'pkgs/latentnet_2.7.1.tar.gz'), repos = NULL, type="source") }        
if(!('btergm1.9.0' %in% iPkgs)){
    install.packages(paste0(mainPath, 'pkgs/btergm_1.9.0.tar.gz'), repos = NULL, type="source") }

# static version of amen
if(!'devtools' %in% installed.packages()){
  install.packages('devtools', repos="https://cloud.r-project.org") }
if(!'amen' %in% installed.packages()[,1]){
    devtools::install_github('s7minhas/amen', ref='pa2018_version') }

# load libraries
library(network) # version 1.13.0
library(sna) # version 2.4
library(ergm) # version 3.8
library(latentnet) # version 2.7.1
library(btergm) # version 1.9.0
library(amen) # version 1.4
library(reshape2) # version 1.4.3
library(plyr) # version 1.8.4
library(ggplot2) # version 3.0.0
library(latex2exp) # version 0.4.0
# library(Cairo) # version 1.5-9
library(xtable) # version 1.8-2
library(ROCR) # version 1.0-7
library(caTools) # version 1.17.1
library(RColorBrewer) # version 1.1-2
library(png) # version 0.1-7
library(grid) # version 3.5.0

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
#################### 

#load data ################### 
##*** note that lines 71 to 151 comes primarily from cranmer et al. 2017

# policy forum affiliation data
# 1 = affiliation; 0 = no affiliation
# committee names are in the column labels; actors in the row labels
forum <- as.matrix(read.table(file = paste0(mainPath, "data/climate0205-committee.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# influence reputation data
# square matrix with influence attribution
# 1 = influential; 0 = not influential
# cells contain the ratings of row actors about column actors
infrep <- as.matrix(read.table(file = paste0(mainPath, "data/climate0205-rep.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# collaboration; directed network
collab <- as.matrix(read.table(file = paste0(mainPath, "data/climate0205-collab.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# type of organization; vector with five character types
types <- as.character(read.table(file = paste0(mainPath, "data/climate0205-type.csv"), 
    header = TRUE, row.names = 1, sep = ";")[, 2])

# alliance-opposition perception; -1 = row actor perceives column actor as 
# an opponent; 1 = row actor perceives column actor as an ally; 0 = neutral
allopp <- as.matrix(read.table(file = paste0(mainPath, "data/climate0205-allop.csv"), 
    header = TRUE, row.names = 1, sep = ";"))

# preference dissimilarity; Manhattan distance over four important policy issues
prefdist <- as.matrix(read.table(file = paste0(mainPath, "data/climate0205-prefdist.csv"), 
    header = TRUE, row.names = 1, sep = ";"))
#################### 

#prep data for ergm/latentnet ################### 
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
################### 

#data for logit ################## 
# logistic regression without any correction; row random effects
rows <- rep(1:nrow(collab), ncol(collab))
cols <- c(sapply(1:ncol(collab), function(x) rep(x, nrow(collab))))

logit.data <- data.frame(collab = c(collab), prefdist = c(prefdist), 
    ngo.ofactor = c(ngo.ofactor), gov.ifactor = c(gov.ifactor), 
    forum = c(forum), infrep = c(infrep), collab.t = c(collab.t), 
    type.nodematch = c(type.nodematch), priv.ngo = c(priv.ngo), 
    influence.icov = c(influence.icov), influence.absdiff = 
    c(influence.absdiff), allopp = c(allopp), rows = rows, cols = cols)
####################

#covar list for qap ################### 
covariates <- list(priv.ngo, allopp, prefdist, forum, infrep, 
    influence.icov, influence.absdiff, gov.ifactor, ngo.ofactor, 
    type.nodematch, collab.t)
#################### 

#create data for amen ################### 
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
####################   

#create data with missingness for cross val perf test ################### 
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
#################### 

#save all data ################### 
if( !exists('pergmm')  ){
  save(
    nw.collab, collab.t, priv.ngo, forum, infrep, prefdist, allopp, # dv and covars for ergm/latentnet
    gov.ifactor, ngo.ofactor, # lsm covars  
    Y, Xd, Xs, Xr, n, # data for amen
    logit.data, # data for logit
    covariates, # qap covars
    yMiss, nw.collabMiss, yAct, rposmat, # dv info
    file=paste0(mainPath, 'data/data.rda')) 
}
#################### 