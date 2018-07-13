#set up workspace ################### 
rm(list=ls())

seed <- 12345
set.seed(seed)
# # example linux path
# mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/Research/netModels/dataverseRepl/' 
graphicsPath = paste0(mainPath, 'appendix_results_floats/')

# install gridExta if not there
if(!'gridExtra' %in% installed.packages()){
	install.packages('gridExtra', repos="https://cloud.r-project.org") }

# load libraries
pkgs = c(
    'amen',
    'reshape2','plyr','ggplot2','latex2exp','gridExtra',
    'xtable','ROCR','caTools','RColorBrewer','png','grid')
shh=lapply(pkgs, library, character.only=TRUE)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
source(paste0(mainPath, 'coefHelpers.R'))
####################

####################
# ggtheme for charts
convTheme = theme(
  panel.border=element_blank(), axis.ticks=element_blank(), 
	axis.text.x=element_text( size=4.5), 
	axis.text.y=element_text( size=4.5), 
	strip.text.x = element_text(size=7, color='black' ) 
	)
####################

####################
# construct trace plots
# load ame results
load(paste0(mainPath, 'results/ameEst.rda'))

# conv check
shhh = lapply(1:2, function(ii){
convData = data.frame( ameFit$'BETA', stringsAsFactors=FALSE)
names(convData)[1] = '(Intercept)'; names(convData) = gsub('.dyad|.row|.col','',names(convData))
names(convData) = varKey[,2][match(names(convData), varKey[,1])]
convData$iter = as.numeric( rownames(convData) )
ggConvData = melt(convData, id='iter')
levels(ggConvData$variable) = varKey[,2]

# data for plotting
if(ii==1){toKeep = unique(ggConvData$variable)[2:6] ; fName = 'FigureA1a.pdf'}
if(ii==2){toKeep = unique(ggConvData$variable)[7:11] ; fName = 'FigureA1b.pdf'}
ggConvData = ggConvData[which(ggConvData$variable %in% toKeep),]

ggConv = ggplot(ggConvData, aes(x=iter, y=value)) + 
	geom_line(color='grey40') + ylab('') + xlab('') + 
	facet_wrap(~variable, scales='free_y', ncol=1) + 
	convTheme

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
	convTheme

ggsave(
	grid.arrange(ggConv, ggDist, ncol=2), 
	file=paste0(graphicsPath,fName), 
	width=4, height=6 )
})
####################