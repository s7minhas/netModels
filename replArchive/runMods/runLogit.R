source('replicationSetup.R')

########## LOGISTIC REGRESSION ##########

model.logit <- glm(collab ~ priv.ngo + allopp + prefdist + forum + infrep + 
    influence.icov + influence.absdiff + gov.ifactor + ngo.ofactor + type.nodematch + collab.t, 
    data = logit.data, family = "binomial")

gof.logit <- gof(collab, covariates, coef(model.logit), statistics = c(dsp, 
    esp, geodesic, ideg, odeg, istar))

save(model.logit, logit.data, gof.logit, file=paste0(resultsPath, 'logitResults.rda'))