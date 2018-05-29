rm(list=ls())
source('helpers/paths.R')
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
	if(saveNet){ dev.off() }
}

dvPlot(vertSize=gCollab$vSizeOut, 
	textLab='Node Size Function of Out-Degree',fName=paste0(graphicsPath,'dvNet_outDegree.pdf'))
dvPlot(vertSize=gCollab$vSizeIn, inclLegend=FALSE,
	textLab='Node Size Function of In-Degree',fName=paste0(graphicsPath,'dvNet_inDegree.pdf'))
#################