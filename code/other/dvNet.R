rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg(c('igraph'))

# collaboration; directed network
collab <- as.matrix(read.table(file = paste0(dataPath, "climate0205-collab.csv"), 
    header = TRUE, row.names = 1, sep = ";"))
gCollab = graph_from_adjacency_matrix(collab, mode='directed', diag=FALSE)

# add g attribs
gCollab$vSize = degree(gCollab, mode='in')
pal = colorRampPalette(c('#649173','#DBD5A4'))
pal = colorRampPalette(c('#E6DADA','#274046'))
pal = colorRampPalette(c('white','black'))
gCollab$vColor = pal(7)[as.numeric(cut(degree(gCollab, mode='out'), breaks=7))]

# plot g
set.seed(6886)
fName = paste0(graphicsPath, 'dvNet.pdf')
pdf(file=fName, width=7.5, height=6)
plot(
	gCollab,
	layout=layout_components,
	vertex.label=NA, 
	vertex.color=gCollab$vColor,
	vertex.size=gCollab$vSize,
	edge.arrow.size=.3,
	asp=FALSE		
	)
dev.off()
system( paste0('pdfcrop ', fName, ' ', fName) )

# Load some covariate info
load(paste0(dataPath, 'data.rda'))
covars = cbind(ngo=Xs[,'ngo.ofactor'], gov=Xr[,'gov.ifactor'])

# Add g attribs to distinguish different types of nodes
gCollab$shapeType = rep('circle', nrow(covars))
gCollab$shapeType[which(covars[,'ngo']==1)] = 'square'
gCollab$shapeType[which(covars[,'gov']==1)] = 'rectangle'

set.seed(6886)
plot(
	gCollab,
	layout=layout_components,
	vertex.label=NA, 
	vertex.color=gCollab$vColor,
	vertex.size=gCollab$vSize,
	vertex.shape=gCollab$shapeType,
	edge.arrow.size=.3,
	asp=FALSE		
	)

#################
# outdegree 
diag(collab) = NA
sumNA = function(x){base::sum(x, na.rm=TRUE) }

ggOutNgoData = data.frame( rbind(
	cbind( lab='ngoOne', val=apply(collab[which(covars[,'ngo']==1),], 1, sumNA) ),	
	cbind( 'ngoZero', apply(collab[which(covars[,'ngo']==0),], 1, sumNA) )
	), stringsAsFactors=FALSE, row.names=NULL )
ggOutNgoData$val = num(ggOutNgoData$val)
ggOut = ggplot(ggOutNgoData, aes(val, fill=lab)) + geom_bar() 

# indegree
ggInNgoData = data.frame( rbind(
	cbind( lab='ngoOne', val=apply(collab[,which(covars[,'ngo']==1)], 2, sumNA) ),	
	cbind( 'ngoZero', apply(collab[,which(covars[,'ngo']==0)], 2, sumNA) )
	), stringsAsFactors=FALSE, row.names=NULL )
ggInNgoData$val = num(ggInNgoData$val)
ggIn = ggplot(ggInNgoData, aes(val, fill=lab)) + geom_bar()

loadPkg('gridExtra')
grid.arrange(ggIn, ggOut, ncol=2)
#################

#################
# outdegree
mean( apply(collab, 1, sumNA) ) ; sd( apply(collab, 1, sumNA) )
mean(
	apply(collab[which(covars[,'gov']==1),], 1, sumNA)
	)
mean(
	apply(collab[which(covars[,'gov']==0),], 1, sumNA)
	)

# indegree
mean( apply(collab, 2, sumNA) ) ; sd( apply(collab, 2, sumNA) )
mean(
	apply(collab[,which(covars[,'gov']==1)], 2, sumNA)
	)
mean(
	apply(collab[,which(covars[,'gov']==0)], 2, sumNA)
	)
#################