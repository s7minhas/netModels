rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R')) ; source(paste0(funcPath, 'coefHelpers.R'))
loadPkg(c('latentnet','sna'))

################################################
# load model results
load(paste0(resultsPath, 'ameFitSR_0.rda'))
ameCoef0 = getAmeCoef(ameFit) ; rownames(ameCoef0) = gsub('.col','',rownames(ameCoef0))
ameTab0 = lazyCleanTable( ameCoef0[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(resultsPath, 'ameFitSR_1.rda'))
ameCoef1 = getAmeCoef(ameFit) ; rownames(ameCoef1) = gsub('.col','',rownames(ameCoef1))
ameTab1 = lazyCleanTable( ameCoef1[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(resultsPath, 'ameFitSR_2.rda'))
ameCoef2 = getAmeCoef(ameFit) ; rownames(ameCoef2) = gsub('.col','',rownames(ameCoef2))
ameTab2 = lazyCleanTable( ameCoef2[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(resultsPath, 'ameFitSR_3.rda'))
ameCoef3 = getAmeCoef(ameFit) ; rownames(ameCoef3) = gsub('.col','',rownames(ameCoef3))
ameTab3 = lazyCleanTable( ameCoef3[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

load(paste0(resultsPath, 'ameFitSR_4.rda'))
ameCoef4 = getAmeCoef(ameFit) ; rownames(ameCoef4) = gsub('.col','',rownames(ameCoef4))
ameTab4 = lazyCleanTable( ameCoef4[,c('pmean','lo95','hi95','p-val')], sigCol=4 )
################################################

################################################
# summarize in q & d table
coefDfs = list(
	'AME (k=0)'=ameTab0,  'AME (k=1)'=ameTab1, 'AME (k=2)'=ameTab2,
	'AME (k=3)'=ameTab3, 'AME (k=4)'=ameTab4 )

# table
frameRows = rep(varKey[,2], each=2)
frameRows = append(frameRows, '\\textbf{Conflicting policy preferences}', which(frameRows=='Intercept/Edges')[2])
frameRows = append(frameRows, '\\textbf{Transaction costs}', which(frameRows=='Preference dissimilarity')[2])
frameRows = append(frameRows, '\\textbf{Influence}', which(frameRows=='Joint forum participation')[2])
frameRows = append(frameRows, '\\textbf{Functional requirements}', which(frameRows=='Alter = Government actor')[2])
frameRows = append(frameRows, '\\textbf{Endogenous dependencies}', which(frameRows=='Same actor type')[2])
frame = matrix('', nrow=length(frameRows), ncol=length(coefDfs)+1, 
	dimnames=list( frameRows, c(' ', names(coefDfs)) ))

# helpful ids
varPos = which(frameRows %in% varKey[,2])
estRows = varPos[seq(1,length(varPos),2)]; errorRows = varPos[seq(2,length(varPos),2)]
varLab = frameRows; varLab[errorRows] = ''; frame[,1] = varLab; rownames(frame)[estRows] = ''

# Add logit results
frame = insertCoefInfo(frame, model='AME (k=0)', error='int')
frame = insertCoefInfo(frame, model='AME (k=1)', error='int')
frame = insertCoefInfo(frame, model='AME (k=2)', error='int')
frame = insertCoefInfo(frame, model='AME (k=3)', error='int')
frame = insertCoefInfo(frame, model='AME (k=4)', error='int')

# cleanup
rownames(frame) = NULL
frame[estRows[-1],1] = paste0('$\\;\\;\\;\\;$ ', frame[estRows[-1],1])
frame = frame[-(grep('Endogenous dependencies', frame[,1]):nrow(frame)),]

# print
print.xtable(
	xtable(frame, align='llccccc',
		caption='* p $<$ 0.05 (or 0 outside the 95\\% confidence interval).',
		label='tab:regTable_latSpace'), 
	include.rownames=FALSE,
	sanitize.text.function=identity,
	hline.after=c(0,0,nrow(frame),nrow(frame)),
	size='tiny', file=paste0(graphicsPath, 'regTable_ameSR.tex')
)
################################################