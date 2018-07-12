source('~/Research/netModels/code/replicationSetup.R')

########## LOGISTIC REGRESSION ##########

model.logit <- glm(collab ~ priv.ngo + allopp + prefdist + forum + infrep + 
    influence.icov + influence.absdiff + gov.ifactor + ngo.ofactor + type.nodematch + collab.t, 
    data = logit.data, family = "binomial")

gof.logit <- gof(collab, covariates, coef(model.logit), statistics = c(dsp, 
    esp, geodesic, ideg, odeg, istar))

pdf(paste0(graphicsPath, "gof-logit.pdf"), width = 9, height = 6)
plot(gof.logit)
dev.off()

save(model.logit, logit.data, gof.logit, file=paste0(resultsPath, 'logitResults.rda'))