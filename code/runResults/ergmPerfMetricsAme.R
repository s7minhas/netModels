rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg( c( 'devtools', 'plyr' ) )
devtools::install_github('s7minhas/amen') ; library(amen)

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# actual values
actVals = gofstats(Y)
################################################

################################################
# ERGM
load(paste0(resultsPath, 'ergmResults.rda'))
ergmPerf = do.call('rbind', lapply(ergmSims, function(x){
	gofstats( as.sociomatrix( x ) ) }))

load(paste0(resultsPath, 'ameFitSR_2.rda'))
amePerf = ameFit$'GOF'[-1,]
################################################

################################################
# summary stats
summ = function(x){ c( mu=mean(x), med=median(x), quantile(x, c(0,0.025,0.975,1))  )  }
actVals
t( apply(ergmPerf, 2, summ) )
t( apply(amePerf, 2, summ) )
varKey
perfNetKey = cbind(v=names(actVals), 
	c=c('Sender variation', 'Receiver Variation', 'Dyadic dependency', 'Triadic dependency') )
################################################

################################################
# plot
ggData = rbind(cbind(melt(ergmPerf),model='ERGM'), cbind(melt(amePerf), model='AME'))
ggData$key = paste(ggData$Var2, ggData$model, sep='_')

ggDensity = ddply(ggData, .(key), .fun=function(x){ 
	tmp = density(x$value); x1 = tmp$x; y1 = tmp$y 
	q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975) 
	q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95) 
	data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

# clean up and add labels
ggDensity$var = unlist(lapply(strsplit(ggDensity$key, '_'), function(x){ x[1] }))
ggDensity$model = unlist(lapply(strsplit(ggDensity$key, '_'), function(x){ x[2] }))
ggDensity$actual = actVals[match(ggDensity$var, names(actVals))]
ggDensity$varClean = perfNetKey[,2][match(ggDensity$var, perfNetKey[,1])]
ggDensity$varClean = factor(ggDensity$varClean, levels=perfNetKey[,2])

# plot
ggPalCol = 'Set1'
tmp=ggplot(data=ggDensity) + 
	geom_line(aes(x=x,y=y,color=model)) + 
	geom_ribbon(data=subset(ggDensity,q95), aes(x=x,ymax=y,fill=model),ymin=0,alpha=0.3) + 
  	geom_ribbon(data=subset(ggDensity,q90), aes(x=x,ymax=y,fill=model),ymin=0,alpha=0.7) + 
	geom_vline(aes(xintercept=actual),linetype='solid',size=1.5, color='black') +   	
  	scale_color_brewer(palette=ggPalCol) +
  	scale_fill_brewer(palette=ggPalCol) +
  	xlab('') + ylab('') +
  	facet_wrap(~varClean, nrow=2, scales='free') +
  	theme(
  		panel.border=element_blank(),
  		axis.ticks=element_blank(),
  		legend.position='bottom', legend.title=element_blank(),
  		legend.background=element_blank(), legend.key = element_rect(colour = NA),
		legend.text=element_text(family="Source Sans Pro Light"),
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_blank(),
		axis.title.y=element_text(family='Source Sans Pro Semibold'),
		strip.text.x = element_text(family="Source Sans Pro Semibold")
  		)
ggsave(tmp, file=paste0(graphicsPath, 'ergmAmePerf.pdf'), height=5, width=8, device=cairo_pdf)
################################################