# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

# some necessary libs
loadPkg(c(
	'reshape2',
	'ggplot2', 'latex2exp', 'Cairo',	# plotting
	'xtable' # tables
	))

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# var key
varKey = matrix(
	c(
		'(Intercept)', 'Intercept/Edges',
		"priv.ngo", "Business vs. NGO",
		"allopp", "Opposition/alliance",
		"prefdist", "Preference dissimilarity",
		"forum", "Joint forum participation",
		"infrep", "Influence attribution",
		"influence.icov", "Alter's influence indegree",
		"influence.absdiff", "Influence absolute diff.",
		"gov.ifactor", "Alter = Government actor",
		"ngo.ofactor", "Ego = Environmental NGO",
		"type.nodematch", "Same actor type",
		"collab.t", "Mutuality",
		'odegreepopularity', 'Outdegree popularity',
		'twopath', 'Twopaths',
		'gwidegree', 'GWIdegree (2.0)',
		'gwesp.fixed.1', 'GWESP (1.0)',
		'gwodegree', 'GWOdegree (0.5)'
	), ncol = 2, byrow=TRUE)

# Extract coefs from ame object
getAmeCoef = function(fit, addTypeLabel=FALSE){
	tmp<-cbind(
		apply(fit$BETA,2,mean), apply(fit$BETA,2,sd),
		apply(fit$BETA,2,mean)/apply(fit$BETA,2,sd),
		2*(1-pnorm( abs(apply(fit$BETA,2,mean)/apply(fit$BETA,2,sd)))),
		t( apply(fit$BETA, 2, function(x){ quantile(x, c(0.025,0.5,0.95,0.975))  }) )
		)
	colnames(tmp)<-c("pmean","psd","z-stat","p-val", 'lo95','lo90','hi90','hi95') 
	if(!addTypeLabel){
		rownames(tmp) = gsub('.dyad', '', rownames(tmp))
		rownames(tmp) = gsub('.row', '', rownames(tmp))
	}
	rownames(tmp)[rownames(tmp)=='intercept'] = '(Intercept)'
	return(tmp)
}

# Extract variance parameters
getAmeVar = function(fit){
	tmp<-cbind(apply(fit$VC,2,mean), apply(fit$VC,2,sd) )
	colnames(tmp)<-c("pmean","psd") 
	return(tmp)
}

# clean coef output
lazyCleanTable = function(coefMatrix, sigCol=3, estCol=1, digits=2, pval=0.05, convChar=TRUE){

	# stdz var names
	coefMatrix=data.matrix(coefMatrix)
	rownames(coefMatrix) = gsub('edgecov.','',rownames(coefMatrix))
	rownames(coefMatrix) = gsub('nodeicov.','',rownames(coefMatrix))
	rownames(coefMatrix)[rownames(coefMatrix)=='edges'] = '(Intercept)'
	rownames(coefMatrix)[rownames(coefMatrix)=='nodeifactor.orgtype.gov'] = 'gov.ifactor'
	rownames(coefMatrix)[rownames(coefMatrix)=='nodeofactor.orgtype.ngo'] = 'ngo.ofactor'
	rownames(coefMatrix)[rownames(coefMatrix)=='nodematch.orgtype'] = 'type.nodematch'
	rownames(coefMatrix)[rownames(coefMatrix)=='absdiff.influence'] = 'influence.absdiff'
	rownames(coefMatrix)[rownames(coefMatrix)=='influence'] = 'influence.icov'
	
	# add missing vars
	vmiss = setdiff(varKey[,1] ,rownames(coefMatrix))
	toadd = matrix(NA, nrow=length(vmiss), ncol=ncol(coefMatrix), dimnames=list(vmiss, colnames(coefMatrix)))
	coefMatrix = rbind(coefMatrix, toadd)	

	# reorder & relabel
	coefMatrix = coefMatrix[varKey[,1],]
	rownames(coefMatrix) = varKey[,2]

	# round and NAs to blank
	if(convChar){
		coefMatrixChar = format(round(coefMatrix, digits), nsmall=digits)
		coefMatrixChar[grepl('NA',coefMatrixChar)] = ''
		coefMatrixChar = trim(coefMatrixChar)

		sigCoefs = which(coefMatrix[,sigCol]<pval)
		coefMatrixChar[sigCoefs,estCol] = paste0(coefMatrixChar[sigCoefs,estCol], '$^{\\ast}$')

		return(coefMatrixChar)
	}

	return(coefMatrix)
}

# coef stuff
insertCoefInfo = function(coefTable, model, error='se', data=coefDfs, estRows=varLab, seRows=rownames(frame)){
	
	# IDs
	estIds = match( rownames(data[[model]]), estRows)
	seIds = match( rownames(data[[model]]), seRows )
	
	# Insert est
	frame[estIds, model] = data[[model]][,1]
	
	# Insert error
	if(error!='none'){
		if(error=='se'){
			frame[seIds, model] = ifelse(data[[model]][,2]=='','',paste0('(',data[[model]][,2],')')) }
		if(error=='int'){
			frame[seIds, model] = ifelse(data[[model]][,2]=='','',paste0('[',data[[model]][,2],'; ',data[[model]][,3],']')) }
	}
	return(frame)
}