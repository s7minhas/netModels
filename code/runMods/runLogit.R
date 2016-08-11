source('~/Research/netModels/code/1_replicationSetup.R')

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
pdf(paste0(graphicsPath, "gof-logit.pdf"), width = 9, height = 6)
plot(gof.logit)
dev.off()

save(model.logit, logit.data, file=paste0(resultsPath, 'logitResults.rda'))