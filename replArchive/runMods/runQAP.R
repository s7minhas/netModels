source('replicationSetup.R')

########## QAP NETLOGIT ##########

model.qap <- netlogit(nw.collab, covariates, reps = 200, nullhyp = "qap")

gof.qap <- gof(collab, covariates, coef(model.qap), statistics = c(dsp, 
    esp, geodesic, ideg, odeg, istar))

save(model.qap, gof.qap, file=paste0(resultsPath, 'qapResults.rda'))