#set up workspace ################### 
rm(list=ls())

seed <- 12345
set.seed(seed)
# # example linux path
# mainPath = '/home/minhas/dataverseRepl/'
# example mac path
mainPath = '~/Research/netModels/dataverseRepl/' 
resultsPath = paste0(mainPath, 'results/')
dataPath = paste0(mainPath, 'data/')
graphicsPath = paste0(mainPath, 'appendix_results_floats/')

# load libraries
pkgs = c(
    'amen',
    'reshape2','plyr','ggplot2','latex2exp',
    'xtable','ROCR','caTools','RColorBrewer','png','grid')
shh=lapply(pkgs, library, character.only=TRUE)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# load data
load(paste0(mainPath, 'data/data.rda'))
####################

####################
# load actor type info and assign cols [same as dvNet]
actorType = as.matrix(read.table(file = paste0(dataPath, "climate0205-type.csv"), 
    header = TRUE, row.names = 1, sep = ";"))[,2]
actorType2 = paste0(toupper(substr(actorType, 1, 1)), substr(actorType, 2, nchar(actorType)))
actorType2[actorType2=='Ngo']='NGO' ; actorType2[actorType2=='Gov']='State'
actorType2[actorType2=='Private']='Business'
typeKey = data.frame(type=unique(actorType2), stringsAsFactors=FALSE)
typeKey$ucol = brewer.pal(nrow(typeKey)*2, 'Paired')[seq(1,nrow(typeKey)*2,2)]
typeKey$vcol = brewer.pal(nrow(typeKey)*2, 'Paired')[seq(2,nrow(typeKey)*2,2)]

tmp = as.matrix(read.table(file = paste0(dataPath, "climate0205-collab.csv"), 
	header = TRUE, row.names = 1, sep = ";"))
load(paste0(dataPath, 'data.rda'))
rownames(Y) = colnames(Y) = rownames(tmp)
load(paste0(resultsPath, 'ameEst.rda'))

# Multiplicative effects
# laziness lets just use base plot
ucols = typeKey$ucol[match(actorType2, typeKey$type)]
vcols = typeKey$vcol[match(actorType2, typeKey$type)]

fName = paste0(graphicsPath, 'FigureA2.pdf')
pdf(file=fName, width=8, height=8)
circplot(Y=Y, U=ameFit$'U', V=ameFit$'V', 
	rcol=ucols, ccol=vcols, lcol='grey80',
	plotnames=FALSE, pch=19, pscale=3)
legend(x=.45, y=1.2, typeKey$type, pch=21, col="black", pt.bg=typeKey$ucol, 
		pt.cex=2, cex=1.25, bty="n", ncol=1)
legend(x=-1.2, y=-.6, typeKey$type, pch=21, col="black", pt.bg=typeKey$vcol, 
		pt.cex=2, cex=1.25, bty="n", ncol=1)
dev.off()
####################