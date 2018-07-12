rm(list=ls())
library("igraph")
library('RColorBrewer')
source('~/Research/netModels/code/helpers/paths.R')

#### Homophily
# load homophnet
load(paste0(dataPath, 'homophNet.rda'))
set.seed(1234)
fName = paste0(graphicsPath, 'homophNet.pdf') ; pdf(file=fName, width=7.5, height=3)
plot(net, edge.arrow.size=0,vertex.label=NA,vertex.color='grey55')
dev.off() ; system( paste0('pdfcrop ', fName, ' ', fName) )

# stoch equiv, params rougly based on manger et al. 2012
adjMat = matrix(0, nrow=16, ncol=16)
high = 1:4
med = 5:11
low = 12:16

adjMat[high,high] = rbinom(length(adjMat[high,high]), 1, .8) # high-high
adjMat[med,med] = rbinom(length(adjMat[med,med]), 1, .4) # med-med
adjMat[low,low] = rbinom(length(adjMat[low,low]), 1, .005) # low-low

adjMat[high,med] = rbinom(length(adjMat[high,med]), 1, .5) # high - med
# adjMat[med,high] = rbinom(length(adjMat[med,high]), 1, .9) # med - high

adjMat[low,high] = rbinom(length(adjMat[low,high]), 1, .001) # low - high
# adjMat[high,low] = rbinom(length(adjMat[high,low]), 1, .002) # high - low

adjMat[med,low] = rbinom(length(adjMat[med,low]), 1, .1) # med - low
# adjMat[low,med] = rbinom(length(adjMat[low,med]), 1, .3) # low - med

# oops undirected
adjMat[lower.tri(adjMat)] = 0
adjMat[lower.tri(adjMat)] = t(adjMat)[lower.tri(adjMat)]
g = graph_from_adjacency_matrix(adjMat, mode='undirected', weighted=NULL, diag=FALSE)

# layout
hl=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[high,high], mode='undirected', weighted=NULL, diag=FALSE)), .3, .7, .8, 1)
ml=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[med,med], mode='undirected', weighted=NULL, diag=FALSE)), 0, .3, 0, .4)
ll=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[low,low], mode='undirected', weighted=NULL, diag=FALSE)), .7, 1, 0, .4)
l = rbind(hl,ml,ll)

# color by class
V(g)$color = c(rep('grey90', length(high)), rep('grey60', length(med)), rep('grey10', length(low)))

# plot save
fName = paste0(graphicsPath, 'stochEquivNet.pdf') ; pdf(file=fName, width=7.5, height=3)
plot(g, vertex.label=NA, vertex.color=V(g)$color, layout=l)
dev.off() ; system( paste0('pdfcrop ', fName, ' ', fName) )