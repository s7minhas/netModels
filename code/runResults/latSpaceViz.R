rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
loadPkg(c('amen'))

tmp = as.matrix(read.table(file = paste0(dataPath, "climate0205-collab.csv"), 
	header = TRUE, row.names = 1, sep = ";"))
load(paste0(dataPath, 'data.rda'))
rownames(Y) = colnames(Y) = rownames(tmp)
load(paste0(resultsPath, 'ameFitSR_2.rda'))

# First + Second order effects
outEffect = data.frame(act=rowSums(Y), mod=ameFit$APM, 
	id=rownames(Y), stringsAsFactors=FALSE, row.names=NULL)
inEffect = data.frame(act=colSums(Y), mod=ameFit$BPM, 
	id=rownames(Y), stringsAsFactors=FALSE, row.names=NULL)

# plot leftover effects against degree stats
facet_labeller = function(string){ TeX(string) }
firstEffPlot = function(effect, actName, modName){
	ggEff = melt(effect, id='id')
	ggEff$id = factor(ggEff$id, levels=effect$id[order(effect$mod,decreasing=TRUE)])
	ggEff$variable=char(ggEff$variable)
	ggEff$variable[ggEff$variable=='act'] = actName
	ggEff$variable[ggEff$variable=='mod'] = modName
	effPlot=ggplot(ggEff, aes(x=id, xend=id, y=0, yend=value)) +
		geom_segment(size=1) + 
		geom_point(aes(x=id,y=value)) +
		ylab('') + xlab('') + 
		facet_wrap(~variable, nrow=2, scales='free_y', 
			labeller=as_labeller(facet_labeller, default = label_parsed)) + 
		theme(
			axis.text.x = element_text(angle=45, hjust=1, size=8), 
			# ,family="Source Sans Pro Light"
			# axis.text.y = element_text(family="Source Sans Pro Light"),
			axis.ticks=element_blank(), panel.border=element_blank(),
			legend.position='none'
			)
	return(effPlot)
}

firstEffPlot(outEffect, 'Out-Degree', 'Additive Sender Effect ($a_{i}$)')
firstEffPlot(inEffect, 'In-Degree', 'Additive Receiver Effect ($b_{j}$)')

# Multiplicative effects
circplot(Y=Y, U=ameFit$'U', V=ameFit$'V')
circplot(Y=Y, U=ameFit$'U')
circplot(Y=Y, U=ameFit$'V')

U = ameFit$U
V = ameFit$V

if (is.null(V)) {
    V <- U
    ccol <- rcol
    vscale <- 1
}
mu <- sqrt(apply(U^2, 1, sum))
mv <- sqrt(apply(V^2, 1, sum))
u <- diag(1/mu) %*% U
vscale = 0.8
v <- diag(1/mv) %*% V * vscale

jitter = 0.1 * (nrow(Y)/(1 + nrow(Y)))
ju <- 1 + jitter * (rank(mu)/(nrow(Y) + 1) - 0.5)
u <- u * ju
v <- v * ju
rsum <- apply(abs(Y), 1, sum, na.rm = TRUE)
csum <- apply(abs(Y), 2, sum, na.rm = TRUE)

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
bty = "n"
plot(u * 1.2, type = "n", xaxt = "n", yaxt = "n", xlab = "", 
    ylab = "", bty = bty)

links <- which(Y != 0, arr.ind = TRUE)
lcol = "gray"
lty = 3
segments(u[links[, 1], 1], u[links[, 1], 2], v[links[, 2], 
    1], v[links[, 2], 2], col = lcol, lty = lty)

pscale = 1.75
row.names = rownames(Y) ; rcol = "brown"
col.names = colnames(Y) ; ccol = "blue"
text(u[rsum > 0, ], row.names[rsum > 0], cex = pscale * 
    (mu[rsum > 0])^0.3, col = rcol)
text(v[csum > 0, ], col.names[csum > 0], cex = pscale * 
    (mv[csum > 0])^0.3, col = ccol)