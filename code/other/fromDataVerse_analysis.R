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
summary(model.ls)

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

########## LOGISTIC REGRESSION ##########

# logistic regression without any correction; row random effects
rows <- rep(1:nrow(collab), ncol(collab))
cols <- c(sapply(1:ncol(collab), function(x) rep(x, nrow(collab))))

logit.data <- data.frame(collab = c(collab), prefdist = c(prefdist), 
    ngo.ofactor = c(ngo.ofactor), gov.ifactor = c(gov.ifactor), 
    forum = c(forum), infrep = c(infrep), collab.t = c(collab.t), 
    type.nodematch = c(type.nodematch), priv.ngo = c(priv.ngo), 
    influence.icov = c(influence.icov), influence.absdiff = 
    c(influence.absdiff), allopp = c(allopp), rows = rows, cols = cols)

model.logit <- glm(collab ~ priv.ngo + allopp + prefdist + forum + infrep + 
    influence.icov + influence.absdiff + gov.ifactor + ngo.ofactor + type.nodematch + collab.t, data = logit.data, family = "binomial")

# goodness of fit for the logit model
covariates <- list(priv.ngo, allopp, prefdist, forum, infrep, 
    influence.icov, influence.absdiff, gov.ifactor, ngo.ofactor, 
    type.nodematch, collab.t)

gof.logit <- gof(collab, covariates, coef(model.logit), statistics = c(dsp, 
    esp, geodesic, ideg, odeg, istar))-
pdf("gof-logit.pdf", width = 9, height = 6)
plot(gof.logit)
dev.off()

########## QAP NETLOGIT ##########

model.qap <- netlogit(nw.collab, covariates, reps = 200, nullhyp = "qap")
summary(model.qap)

gof.qap <- gof(collab, covariates, coef(model.qap), statistics = c(dsp, 
    esp, geodesic, ideg, odeg, istar))

pdf(paste0(graphicsPath, "gof-qap.pdf"), width = 9, height = 6)
plot(gof.qap)
dev.off()

########## TABLE OUTPUT ##########

tr.ls <- extract(model.ls)
tr.ls@gof <- tr.ls@gof[1]
tr.ls@gof.names <- "BIC"
tr.ls@gof.decimal <- TRUE
tr.ls@coef.names <- c(
    "Intercept/Edges", 
    "Alter = Government actor", 
    "Ego = Environmental NGO", 
    "Same actor type", 
    "Business vs. NGO", 
    "Joint forum participation", 
    "Influence attribution", 
    "Alter's influence indegree", 
    "Influence absolute diff.", 
    "Preference dissimilarity", 
    "Opposition/alliance"
)

tr.logit <- extract(model.logit, include.aic = FALSE, include.loglik = FALSE, 
    include.deviance = FALSE, include.nobs = FALSE)
tr.logit@coef.names <- c(
    "Intercept/Edges", 
    "Business vs. NGO", 
    "Opposition/alliance", 
    "Preference dissimilarity", 
    "Joint forum participation", 
    "Influence attribution", 
    "Alter's influence indegree", 
    "Influence absolute diff.", 
    "Alter = Government actor", 
    "Ego = Environmental NGO", 
    "Same actor type", 
    "Mutuality"
)

tr.ergm <- extract(model.ergm, include.aic = FALSE, include.loglik = FALSE)
tr.ergm@coef.names <- c(
    "Intercept/Edges", 
    "Mutuality", 
    "Alter = Government actor", 
    "Ego = Environmental NGO", 
    "Same actor type", 
    "Business vs. NGO", 
    "Joint forum participation", 
    "Influence attribution", 
    "Alter's influence indegree", 
    "Influence absolute diff.", 
    "Preference dissimilarity", 
    "Opposition/alliance", 
    "Outdegree popularity", 
    "Twopaths", 
    "GWIdegree (2.0)", 
    "GWESP (1.0)", 
    "GWOdegree (0.5)"
)

tr.qap <- extract(model.qap, include.aic = FALSE, include.deviance = FALSE, 
    include.nobs = FALSE)
tr.qap@coef.names <- c(
    "Intercept/Edges", 
    "Business vs. NGO", 
    "Opposition/alliance", 
    "Preference dissimilarity", 
    "Joint forum participation", 
    "Influence attribution", 
    "Alter's influence indegree", 
    "Influence absolute diff.", 
    "Alter = Government actor", 
    "Ego = Environmental NGO", 
    "Same actor type", 
    "Mutuality"
)

# ASCII plain text table output
screenreg(
    list(tr.logit, tr.qap, tr.ls, tr.ergm), 
    single.row = FALSE, 
    custom.model.names = c("Logit", "MRQAP", "LSM", "ERGM"), 
    file = "table.txt", 
    stars = 0.05, 
    custom.note = "* p < 0.05 (or 0 outside the 95% confidence interval).", 
    groups = list("Conflicting policy preferences" = 2:4, "Transaction costs" = 
        5, "Influence" = 6:9, "Functional requirements" = 10:11, 
        "Endogenous dependencies" = 12:17)
)

# LaTeX table output
texreg(
    list(tr.logit, tr.qap, tr.ls, tr.ergm), 
    single.row = FALSE, 
    custom.model.names = c("Logit", "MRQAP", "LSM", "ERGM"), 
    file = "table.tex", 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    caption = paste("Comparison of Logit, QAP, LSM, and ERGM coefficients and", 
        "uncertainty measures."), 
    stars = 0.05, 
    custom.note = "$^*p<0.05$ (or 0 outside the 95\\% confidence interval).", 
    groups = list("Conflicting policy preferences" = 2:4, "Transaction costs" = 
        5, "Influence" = 6:9, "Functional requirements" = 10:11, 
        "Endogenous dependencies" = 12:17), 
    scalebox = 0.86
)


########## ONLINE APPENDIX ##########

# logit model with Huber-White-corrected standard errors
hc <- vcovHC(model.logit)  # Huber-White sandwich estimator
ct <- coeftest(model.logit, vcov = hc)  # use corrected vcov matrix
se <- ct[, 2]
pval <- ct[, 4]
tr.robust <- extract(model.logit, include.aic = FALSE, 
    include.loglik = FALSE, include.deviance = FALSE, 
    include.nobs = FALSE)  # create a texreg object and replace SEs
tr.robust@se <- se
tr.robust@pvalues <- pval
tr.robust@coef.names <- tr.logit@coef.names

# linear mixed effects model (= with random effects)
model.glmer <- glmer(collab ~ collab.t + gov.ifactor + ngo.ofactor + 
    type.nodematch + priv.ngo + forum + infrep + influence.icov + 
    influence.absdiff + prefdist + allopp + (1 | rows), data = logit.data, 
    family = binomial)

relgrad <- with(model.glmer@optinfo$derivs, solve(Hessian,gradient))
max(abs(relgrad))  # check convergence (should be < 0.001)

tr.glmer <- extract(model.glmer, include.aic = FALSE, include.loglik = FALSE, 
    include.nobs = FALSE, include.variance = FALSE, include.groups = FALSE)
tr.glmer@coef.names <- c(
    "Intercept/Edges", 
    "Mutuality", 
    "Alter = Government actor", 
    "Ego = Environmental NGO", 
    "Same actor type", 
    "Business vs. NGO", 
    "Joint forum participation", 
    "Influence attribution", 
    "Alter's influence indegree", 
    "Influence absolute diff.", 
    "Preference dissimilarity", 
    "Opposition/alliance"
)

# GEE model
model.gee <- gee(collab ~ collab.t + gov.ifactor + ngo.ofactor + 
    type.nodematch + priv.ngo + forum + infrep + influence.icov + 
    influence.absdiff + prefdist + allopp, id = rows, data = logit.data, 
    family = binomial)

tr.gee <- extract(model.gee, include.dispersion = FALSE, include.nobs = FALSE)
tr.gee@coef.names <- c(
    "Intercept/Edges", 
    "Mutuality", 
    "Alter = Government actor", 
    "Ego = Environmental NGO", 
    "Same actor type", 
    "Business vs. NGO", 
    "Joint forum participation", 
    "Influence attribution", 
    "Alter's influence indegree", 
    "Influence absolute diff.", 
    "Preference dissimilarity", 
    "Opposition/alliance"
)

# ASCII plain text table output
screenreg(
    list(tr.logit, tr.robust, tr.gee, tr.glmer), 
    single.row = FALSE, 
    custom.model.names = c("Logit", "Robust", "GEE", "GLMM"), 
    file = "table-appendix.txt", 
    stars = 0.05, 
    custom.note = "* p < 0.05.", 
    groups = list("Conflicting policy preferences" = 2:4, "Transaction costs" = 
        5, "Influence" = 6:9, "Functional requirements" = 10:11, 
        "Endogenous dependencies" = 12)
)

# LaTeX table output
texreg(
    list(tr.logit, tr.robust, tr.gee, tr.glmer), 
    single.row = FALSE, 
    custom.model.names = c("Logit", "Robust", "GEE", "GLMM"), 
    file = "table-appendix.tex", 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    stars = 0.05, 
    custom.note = "$^*p<0.05$.", 
    groups = list("Conflicting policy preferences" = 2:4, "Transaction costs" = 
        5, "Influence" = 6:9, "Functional requirements" = 10:11, 
        "Endogenous dependencies" = 12)
)

