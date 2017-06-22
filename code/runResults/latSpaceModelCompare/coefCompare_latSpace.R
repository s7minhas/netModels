rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R')) ; source(paste0(funcPath, 'coefHelpers.R'))
loadPkg(c('latentnet','sna'))

################################################
# load model results
load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
lsEuclCoef = summary(model.ls)$'pmean'$'coef.table'
lsEuclTab = lazyCleanTable( lsEuclCoef, sigCol=4 )

lsEuclCoefSR = summary(model.lsSR)$'pmean'$'coef.table'
lsEuclTabSR = lazyCleanTable( lsEuclCoefSR, sigCol=4 )

load(paste0(resultsPath, 'bilLatSpaceResults.rda'))
lsBilCoef = summary(model.lsBil)$'pmean'$'coef.table'
lsBilTab = lazyCleanTable( lsBilCoef, sigCol=4 )

lsBilCoefSR = summary(model.lsBilSR)$'pmean'$'coef.table'
lsBilTabSR = lazyCleanTable( lsBilCoefSR, sigCol=4 )

load(paste0(resultsPath, 'ameFitSR_2.rda'))
ameCoef = getAmeCoef(ameFit) ; rownames(ameCoef) = gsub('.col','',rownames(ameCoef))
ameTab = lazyCleanTable( ameCoef[,c('pmean','lo95','hi95','p-val')], sigCol=4 )
################################################

################################################
# summarize in q & d table
coefDfs = list(LSM=lsEuclTab, 'LSM (Bilinear)'=lsBilTab, 
	'LSM (SR)'=lsEuclTabSR, 'LSM (Bilinear + SR)'=lsBilTabSR, 
	AME=ameTab)

graphicsPath='~/Research/netModels/paper/toSubmit/'
coefDfs = coefDfs[c(1,3,5)]

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
frame = insertCoefInfo(frame, model='LSM', error='int')
# frame = insertCoefInfo(frame, model='LSM (Bilinear)', error='int')
frame = insertCoefInfo(frame, model='LSM (SR)', error='int')
# frame = insertCoefInfo(frame, model='LSM (Bilinear + SR)', error='int')
frame = insertCoefInfo(frame, model='AME', error='int')

# cleanup
rownames(frame) = NULL
frame[estRows[-1],1] = paste0('$\\;\\;\\;\\;$ ', frame[estRows[-1],1])
frame = frame[-(grep('Endogenous dependencies', frame[,1]):nrow(frame)),]

# print
print.xtable(
	xtable(frame, align='llccc',
		caption='* p $<$ 0.05 (or 0 outside the 95\\% confidence interval).',
		label='tab:regTable_latSpace'), 
	include.rownames=FALSE,
	sanitize.text.function=identity,
	hline.after=c(0,0,nrow(frame),nrow(frame)),
	size='tiny', file=paste0(graphicsPath, 'regTable_latSpaceSmall.tex')
)
################################################