source('~/Research/netModels/code/1_replicationSetup.R')

########## QAP NETLOGIT ##########

model.qap <- netlogit(nw.collab, covariates, reps = 200, nullhyp = "qap")
summary(model.qap)

gof.qap <- gof(collab, covariates, coef(model.qap), statistics = c(dsp, 
    esp, geodesic, ideg, odeg, istar))

pdf(paste0(graphicsPath, "gof-qap.pdf"), width = 9, height = 6)
plot(gof.qap)
dev.off()

save(model.qap, file=paste0(resultsPath, 'qapResults.rda'))