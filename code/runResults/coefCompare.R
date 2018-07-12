rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R')) ; source(paste0(funcPath, 'coefHelpers.R'))
loadPkg(c('ergm','latentnet','sna'))

################################################
# load model results
load(paste0(resultsPath, 'logitResults.rda'))
logitCoef = summary(model.logit)$'coefficients'
logitTab = lazyCleanTable( logitCoef[,c(1:2,4)], sigCol=3 )

load(paste0(resultsPath, 'ergmResults.rda'))
ergmTab = lazyCleanTable( ergmCoef[,c(1:2,4)], sigCol=3 )

load(paste0(resultsPath, 'euclLatSpaceResults.rda'))
lsEuclCoef = summary(model.ls)$'pmean'$'coef.table'
lsEuclTab = lazyCleanTable( lsEuclCoef, sigCol=4 )

load(paste0(resultsPath, 'qapResults.rda'))
qapCoef = matrix( with(model.qap, c(coefficients, pgreqabs)),
	nrow=length(model.qap$coefficients), ncol=2, 
	dimnames=list( 
		c( '(Intercept)', 'priv.ngo', 'allopp', 'prefdist', 'forum', 'infrep',
			'influence.icov', 'influence.absdiff', 'gov.ifactor',
			'ngo.ofactor', 'type.nodematch', 'collab.t'),
		c('est', 'pval')) )
qapTab = lazyCleanTable( qapCoef, 2 )

load(paste0(resultsPath, 'ameFitSR_2.rda'))
ameCoef = getAmeCoef(ameFit) ; rownames(ameCoef) = gsub('.col','',rownames(ameCoef))
ameTab = lazyCleanTable( ameCoef[,c('pmean','lo95','hi95','p-val')], sigCol=4 )
################################################

################################################
# summarize in q & d table
coefDfs = list(Logit=logitTab, MRQAP=qapTab, LSM=lsEuclTab, ERGM=ergmTab, 'AME'=ameTab)

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
frame = insertCoefInfo(frame, model='Logit')
frame = insertCoefInfo(frame, model='MRQAP', error='none')
frame = insertCoefInfo(frame, model='LSM', error='int')
frame = insertCoefInfo(frame, model='ERGM')
frame = insertCoefInfo(frame, model='AME', error='int')

# cleanup
rownames(frame) = NULL
frame[estRows[-1],1] = paste0('$\\;\\;\\;\\;$ ', frame[estRows[-1],1])

# remove mrqap
frame = frame[,-3]

# print
print.xtable(
	xtable(frame, align='llcccc',
		caption='* p $<$ 0.05 (or 0 outside the 95\\% confidence interval).',
		label='tab:regTable'), 
	include.rownames=FALSE,
	sanitize.text.function=identity,
	hline.after=c(0,0,nrow(frame),nrow(frame)),
	size='normalsize', file=paste0(graphicsPath, 'regTable.tex')
)
################################################