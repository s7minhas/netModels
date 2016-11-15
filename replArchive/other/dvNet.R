rm(list=ls())
source('../helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg(c('igraph','RColorBrewer'))

# collaboration; directed network
collab <- as.matrix(read.table(file = paste0(dataPath, "climate0205-collab.csv"), 
    header = TRUE, row.names = 1, sep = ";"))
gCollab = graph_from_adjacency_matrix(collab, mode='directed', diag=FALSE)
actorType = as.matrix(read.table(file = paste0(dataPath, "climate0205-type.csv"), 
    header = TRUE, row.names = 1, sep = ";"))[,2]
actorType2 = paste0(toupper(substr(actorType, 1, 1)), substr(actorType, 2, nchar(actorType)))
actorType2[actorType2=='Ngo']='NGO' ; actorType2[actorType2=='Gov']='State'
actorType2[actorType2=='Private']='Business'
typeKey = data.frame(type=unique(actorType2), stringsAsFactors=FALSE)
typeKey$col = brewer.pal(nrow(typeKey)*2, 'Paired')[seq(2,nrow(typeKey)*2,2)]

# add g attribs
gCollab$vSizeOut = degree(gCollab, mode='out')
gCollab$vSizeIn = degree(gCollab, mode='in')
gCollab$type = actorType2
gCollab$typeCol = typeKey$col[match(gCollab$type, typeKey$type)]

# define layout
set.seed(6886) ; nodeLayout = layout_with_fr(gCollab)

# plot g
# pwidth=7.5, pheight=6
dvPlot = function(vertSize, textLab, 
	saveNet=TRUE, fName=NULL, inclLegend=TRUE,
	pwidth=7.5, pheight=7.5){
	if(saveNet){ pdf(file=fName, width=pwidth, height=pheight) }
	plot( gCollab,
		layout=nodeLayout,
		vertex.label=NA, 
		vertex.color=gCollab$typeCol,
		vertex.size=vertSize,
		edge.arrow.size=.4,
		asp=FALSE )
	text(x=.25, y=.8, textLab, cex=1.5)
	if(inclLegend){ legend(x=-1.2, y=.1, typeKey$type, pch=21, col="black", pt.bg=typeKey$col, 
		pt.cex=2, cex=1.25, bty="n", ncol=1) }
	if(saveNet){ dev.off() ; system( paste0('pdfcrop ', fName, ' ', fName) ) }
}

dvPlot(vertSize=gCollab$vSizeOut, 
	textLab='Node Size Function of Out-Degree',fName=paste0(graphicsPath,'dvNet_outDegree.pdf'))
dvPlot(vertSize=gCollab$vSizeIn, inclLegend=FALSE,
	textLab='Node Size Function of In-Degree',fName=paste0(graphicsPath,'dvNet_inDegree.pdf'))

#################
# descriptives by actorType
actorTypeOutDegree = lapply(unique(actorType), function(x){
	matSub = collab[names(actorType[actorType==x]),]
	return( rowSums(matSub) ) }) ; names(actorTypeOutDegree) = unique(actorType)
sort(unlist(lapply(actorTypeOutDegree, mean)))

actorTypeInDegree = lapply(unique(actorType), function(x){
	matSub = collab[,names(actorType[actorType==x])]
	return( colSums(matSub) ) }) ; names(actorTypeInDegree) = unique(actorType)
sort(unlist(lapply(actorTypeInDegree, mean)))
#################

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