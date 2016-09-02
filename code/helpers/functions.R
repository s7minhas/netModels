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

########################################################################
# Helpers to make regression table
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
########################################################################

########################################################################
# Helpers to make gof plots
# many depends on btergm
loadPkg('btergm')

# gof names
gofNames = c("Dyad-wise shared partners", "Edge-wise shared partners", "Geodesic distances", "Indegree", "Outdegree", "Incoming k-star")
gofMethods = c(btergm::dsp, btergm::esp, btergm::geodesic, btergm::ideg, btergm::odeg, btergm::istar)

# generate gof stats
getNetStats = function(mat, methodNames=gofNames, methods=gofMethods){
	netSumm = lapply(methods, function(m){
		netCalc = m(mat) ; denom = sum(netCalc) ; res = netCalc/denom
		return(res) })
	names(netSumm) = methodNames
	return(netSumm) }

# get gof dist from lseucl
getLsGof = function(gofObject, lsVars=c('.dspart', '.espart', '.dist','.ideg','.odeg'), netNames=gofNames[1:5]){
	lsDist = lapply(lsVars, function(v){
		tmp=data.matrix( gofObject[[paste0('psim',v)]] )
		colnames(tmp) = rownames(gofObject[[paste0('pval', v)]])
		colnames(tmp) = gsub('dsp|esp', '', colnames(tmp))
		return(tmp) })
	names(lsDist) = netNames
	return(lsDist)
}

# extract gof from ame filepath
getAmeGOF = function(fName, netNames=gofNames, netMethods=gofMethods){
	load(paste0(resultsPath, fName))
	yDraws = ameFit$'ysList'[ sample(1000:length(ameFit$'ysList'), 100, replace=FALSE) ]
	yAmeSim = lapply(yDraws, function(z){ getNetStats(z, netNames, netMethods) } )
	ameDist = lapply(names(yAmeSim[[1]]), function(v){
		t( do.call('cbind', lapply(yAmeSim, function(x){ x[[v]] })) ) })
	names(ameDist) = names(yAmeSim[[1]])
	return(ameDist) }

# organize dist data
getGofData = function(distList, actList){
	gofData = melt(distList)[,-1]
	names(gofData) = c('obs', 'value', 'var', 'model')
	gofData$obs2 = factor(gofData$obs, levels=char(sort(num(unique(gofData$obs)))))
	gofData$key = paste(gofData$obs2, gofData$var, sep='_')
	gofData$id = paste(gofData$key, gofData$model, sep='_')
	toKeep = unique(char(gofData$key[gofData$value!=0]))
	gofData = gofData[which(gofData$key %in% toKeep),]

	# add in actual values
	fuc = melt(actList, id=c('obs','value'))
	fuc$key = paste(fuc$obs, fuc$L1, sep='_')
	gofData$actual = fuc$value[match(gofData$key, fuc$key)]

	# add in means
	gofData$mu = ave(gofData$value, gofData$id, FUN=mean)
	gofData$lo95 = ave(gofData$value, gofData$id, FUN=function(x){quantile(x, 0.025)})
	gofData$hi95 = ave(gofData$value, gofData$id, FUN=function(x){quantile(x, 0.975)})
	gofData$lo90 = ave(gofData$value, gofData$id, FUN=function(x){quantile(x, 0.05)})
	gofData$hi90 = ave(gofData$value, gofData$id, FUN=function(x){quantile(x, 0.95)})
	return(gofData)
}

# plot
ggGof = function(
	data, pRows=3, pCols=2, modSpace=.8, actWidth=.75, bColPal='Set1', 
	save=TRUE, fPath=NULL, fWidth=12, fHeight=6
	){
	tmp=ggplot(data, aes(obs2, value, color=model)) + 
		# geom_point(size=.1, alpha=.25, position=position_jitterdodge(dodge.width=modSpace)) +
		# geom_boxplot(outlier.shape=NA, position=position_dodge(width=modSpace), width=.1) +		
		geom_linerange(aes(ymin=lo95, ymax=hi95), size=.2, position=position_dodge(width=modSpace)) +
		geom_linerange(aes(ymin=lo90, ymax=hi90), size=.8, position=position_dodge(width=modSpace)) +		
		geom_errorbar(aes(ymin=actual, ymax=actual, x=obs2), color='grey50', width=actWidth, size=1.5) +
		geom_point(aes(y=mu, shape=model), size=3, position=position_dodge(width=modSpace)) + 
		xlab('') + ylab('Frequency') + ylim(0,NA) + 
		facet_wrap(~var, scales='free', nrow=pRows, ncol=pCols) + 
		scale_color_brewer(palette=bColPal) +
		theme(
			axis.ticks=element_blank(),
			panel.border=element_blank(),
			legend.position='bottom', legend.title=element_blank(), 
			legend.background=element_blank(), legend.key = element_rect(colour = NA)
			)
	if(!save){ return( tmp ) }
	if(save){
		tmp=tmp + theme(
			legend.text=element_text(family="Source Sans Pro Light", size=18),
			axis.text.x=element_text(family="Source Sans Pro Light", size=12),
			axis.text.y=element_text(family="Source Sans Pro Light",size=12),
			axis.title.y=element_text(family='Source Sans Pro Semibold', size=16),
			strip.text.x = element_text(family="Source Sans Pro Semibold", size=18)
			)
		ggsave(tmp, file=fPath, width=fWidth, height=fHeight, device=cairo_pdf)
	}
}	

# AME net perf
getAmeNetPerf = function(fName){
	load(paste0(resultsPath, fName))

	# perf built into amen
	GOF = ameFit$'GOF'
	GOF[1,] = gofstats(Y)	
	pdf(file=paste0(graphicsPath, gsub('.rda', '', fName), '_GoF.pdf'))
	par(mfrow=c(2,2))
	for (k in 1:4) {
		hist(GOF[-1, k], xlim = range(GOF[, k]), 
		  main = "", prob = TRUE, xlab = colnames(GOF)[k], 
		  col = "lightblue", ylab = "", yaxt = "n")
		abline(v = GOF[1, k], col = "red")
		abline(v=quantile(GOF[-1,k], c(0.025)), col='black')
		abline(v=quantile(GOF[-1,k], c(0.975)), col='black')
	}
	dev.off()
}

# net perf ame coefplot
getNetPerfCoef = function(
	perfList, perfNetKey, actVals, noModels = length(perfList),
	summ = function(x){ c( mu=mean(x), med=median(x), quantile(x, c(0.025,0.05,0.95,0.975)) ) },
	summNames = c('mu','med','lo95','lo90','hi90','hi95'),
	bColPal='Set1',
	pRows=NULL, pCols=NULL,
	save=TRUE,
	fPath=NULL, fWidth=9, fHeight=4
	){
	perfStats=data.frame( do.call('rbind', 
		lapply(perfList, function(x){ t( apply(x, 2, summ) ) })), row.names=NULL )
	colnames(perfStats) = summNames
	perfStats$var = rep( colnames(perfList[[1]] ), noModels )
	perfStats$model = factor(rep(names(perfList) , each=4), levels=names(perfList))
	perfStats$actual = actVals[match(perfStats$var, names(actVals))]
	perfStats$varClean = perfNetKey[,2][match(perfStats$var, perfNetKey[,1])]
	perfStats$varClean = factor(perfStats$varClean, levels=perfNetKey[,2])

	tmp=ggplot(perfStats, aes(x=model, color=factor(model))) +
		geom_hline(aes(yintercept=actual), color='grey40', size=1.5) +
		geom_point(aes(y=mu)) + 
		geom_linerange(aes(ymin=lo95, ymax=hi95), size=.2) +
		geom_linerange(aes(ymin=lo90, ymax=hi90), size=.8) +
		xlab('') + ylab('') +
		facet_wrap(~varClean, scales='free_y', nrow=pRows, ncol=pCols) + 	
		scale_color_brewer(palette=bColPal) + 
		theme(
			legend.position='bottom', legend.title=element_blank(),
			legend.key=element_rect(color=NA),
			axis.ticks=element_blank(), panel.border=element_blank(),
			axis.text.x=element_blank()		
			)
	if(!save){ return(tmp) }
	if(save){
		tmp = tmp + theme(
			legend.text=element_text(family="Source Sans Pro Light"),
			axis.title.y=element_text(family='Source Sans Pro Semibold'),
			strip.text.x = element_text(family="Source Sans Pro Semibold")
			)
		ggsave(tmp, file=fPath, height=fHeight, width=fWidth, device=cairo_pdf)
	}
}

# net perf ame dist, requires plyr
loadPkg('plyr')
getNetPerfDist = function(
	perfList, perfNetKey,actVals,
	bColPal='Set1',
	pRows=NULL, pCols=NULL,
	save=TRUE,
	fPath=NULL, fWidth=9, fHeight=4	
	){

	# org data
	ggData = do.call('rbind', lapply(1:length(perfList), function(ii){
		cbind(melt(perfList[[ii]]), model=names(perfList)[ii]) }) )
	ggData$key = paste(ggData$Var2, ggData$model, sep='_')

	ggDensity = ddply(ggData, .(key), .fun=function(x){ 
		tmp = density(x$value); x1 = tmp$x; y1 = tmp$y 
		q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975) 
		q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95) 
		data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

	# clean up and add labels
	ggDensity$var = unlist(lapply(strsplit(ggDensity$key, '_'), function(x){ x[1] }))
	ggDensity$model = unlist(lapply(strsplit(ggDensity$key, '_'), function(x){ x[2] }))
	ggDensity$model = factor(ggDensity$model, levels=names(perfList))
	ggDensity$actual = actVals[match(ggDensity$var, names(actVals))]
	ggDensity$varClean = perfNetKey[,2][match(ggDensity$var, perfNetKey[,1])]
	ggDensity$varClean = factor(ggDensity$varClean, levels=perfNetKey[,2])

	# plot
	tmp=ggplot(data=ggDensity) + 
		geom_line(aes(x=x,y=y,color=model)) + 
		geom_ribbon(data=subset(ggDensity,q95), aes(x=x,ymax=y,fill=model),ymin=0,alpha=0.3) + 
	  	geom_ribbon(data=subset(ggDensity,q90), aes(x=x,ymax=y,fill=model),ymin=0,alpha=0.7) + 
		geom_vline(aes(xintercept=actual),linetype='solid',size=1.5, color='black') +   	
	  	scale_color_brewer(palette=bColPal) +
	  	scale_fill_brewer(palette=bColPal) +
	  	xlab('') + ylab('') +
	  	facet_wrap(~varClean, scales='free', nrow=pRows, ncol=pCols) + 	
	  	theme(
	  		panel.border=element_blank(),
	  		axis.ticks=element_blank(),
	  		legend.position='bottom', legend.title=element_blank(),
	  		legend.background=element_blank(), legend.key = element_rect(colour = NA),
			axis.text.y=element_blank()
			)
	if(!save){ return(tmp) }
	if(save){
		tmp = tmp + theme(
				legend.text=element_text(family="Source Sans Pro Light"),
				axis.text.x=element_text(family="Source Sans Pro Light"),
				axis.title.y=element_text(family='Source Sans Pro Semibold'),
				strip.text.x = element_text(family="Source Sans Pro Semibold")
		  		)
		ggsave(tmp, file=fPath, height=fHeight, width=fWidth, device=cairo_pdf)
	}
}
########################################################################