rm(list=ls())
seed = 12345
set.seed(seed)
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
library(amen)

# ggtheme for charts
convTheme = theme( panel.border=element_blank(), axis.ticks=element_blank(), 
	axis.text.x=element_text(family="Source Sans Pro Light", size=4.5), 
	axis.text.y=element_text(family="Source Sans Pro Light", size=4.5), 
	strip.text.x = element_text(size = 7, color='white',family="Source Sans Pro Semibold") )

# ame results
load(paste0(resultsPath, 'ameFit_2.rda'))

# conv check
convData = data.frame( ameFit$'BETA', stringsAsFactors=FALSE)
names(convData)[1] = '(Intercept)'; names(convData) = gsub('.dyad|.row|.col','',names(convData))
names(convData) = varKey[,2][match(names(convData), varKey[,1])]
convData$iter = as.numeric( rownames(convData) )
ggConvData = melt(convData, id='iter')
levels(ggConvData$variable) = varKey[,2]

# data for plotting
toKeep = unique(ggConvData$variable)[2:6] ; fName = 'ameConv1.pdf'
# toKeep = unique(ggConvData$variable)[7:11] ; fName = 'ameConv2.pdf'
ggConvData = ggConvData[which(ggConvData$variable %in% toKeep),]

ggConv = ggplot(ggConvData, aes(x=iter, y=value)) + 
	geom_line(color='grey40') + ylab('') + xlab('') + 
	facet_wrap(~variable, scales='free_y', ncol=1) +	
	theme_light(base_family="Source Sans Pro") + convTheme

loadPkg('plyr')
ggMu = ddply(ggConvData, .(variable), summarise, mu=mean(value))
ggDens = ddply(ggConvData,.(variable),.fun = function(x){
  tmp = density(x$value) ; x1 = tmp$x; y1 = tmp$y
  q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
  data.frame(x=x1,y=y1,q95=q95) })

ggDist = ggplot(ggDens, aes(x=x)) + 
	facet_wrap(~ variable, scales='free', ncol=1) + 	
	geom_line(aes(y=y), color='grey40') + ylab('') + xlab('') +
	geom_ribbon(data=subset(ggDens, q95), aes(ymax=y),ymin=0, alpha=.5, fill='grey40') + 
	geom_vline(data=ggMu, aes(xintercept=mu), linetype='solid', size=1, color='black') + 
	theme_light(base_family="Source Sans Pro") + convTheme

loadPkg('gridExtra')
ggsave(grid.arrange(ggConv, ggDist, ncol=2), file=paste0(graphicsPath,fName), 
	width=4, height=6, device=cairo_pdf)
system(paste('pdfcrop', paste0(graphicsPath,fName), paste0(graphicsPath,fName)))