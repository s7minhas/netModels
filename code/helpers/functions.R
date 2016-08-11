# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

# var key
varKey = matrix(
	c(
		"priv.ngo", "business v ngo",
		"allopp", "opposition/alliance",
		"prefdist", "pref dissim",
		"forum", "joint forum",
		"infrep", "influence attribution",
		"influence.icov", "alter's influence indegree",
		"influence.absdiff", "influence abs diff",
		"gov.ifactor", "alter = gov actor",
		"ngo.ofactor", "ego = environ ngo",
		"type.nodematch", "same actor type",
		"collab.t", "mutuality"
	), ncol = 2, byrow=TRUE)

# Extract coefs from ame object
getAmeCoef = function(fit, addTypeLabel=FALSE){
	tmp<-cbind(apply(fit$BETA,2,mean), apply(fit$BETA,2,sd) ,
	   apply(fit$BETA,2,mean)/apply(fit$BETA,2,sd) , 
	   2*(1-pnorm( abs(apply(fit$BETA,2,mean)/apply(fit$BETA,2,sd)))))
	colnames(tmp)<-c("pmean","psd","z-stat","p-val") 
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