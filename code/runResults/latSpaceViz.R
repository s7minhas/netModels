rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg(c('amen', 'RColorBrewer'))

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
load(paste0(resultsPath, 'ameFitSR_2.rda'))

# Multiplicative effects
# laziness lets just use base plot
ucols = typeKey$ucol[match(actorType2, typeKey$type)]
vcols = typeKey$vcol[match(actorType2, typeKey$type)]

fName = paste0(graphicsPath, 'ameFitSR_2_UV.pdf')
pdf(file=fName, width=8, height=8)
circplot(Y=Y, U=ameFit$'U', V=ameFit$'V', 
	rcol=ucols, ccol=vcols, lcol='grey80',
	plotnames=FALSE, pch=19, pscale=3)
# text(x=.55, y=1.25, TeX('$\\hat{U}$'), cex=1.5, family='serif')
legend(x=.45, y=1.2, typeKey$type, pch=21, col="black", pt.bg=typeKey$ucol, 
		pt.cex=2, cex=1.25, bty="n", ncol=1)
# text(x=-1.1, y=-.55, TeX('$\\hat{V}$'), cex=1.5, family='')
legend(x=-1.2, y=-.6, typeKey$type, pch=21, col="black", pt.bg=typeKey$vcol, 
		pt.cex=2, cex=1.25, bty="n", ncol=1)
dev.off()
system( paste0('pdfcrop ', fName, ' ', fName) )