loadPkg(c('ROCR', 'caTools','RColorBrewer'))

# Roc curve, depends ROCR
roc = function(prediction, actual){
      pred = prediction(prediction, actual)
      perf = performance(pred,"tpr","fpr")
      rocData = data.frame(attributes(perf)$x.values[[1]], attributes(perf)$y.values[[1]])
      names(rocData) = c('FPR', 'TPR')
      return(rocData)
}

# Auc, depends ROCR
getAUC = function(prediction, actual){
	pred = prediction(prediction, actual)	
	attributes(performance(pred,"auc"))$y.values[[1]]
}

# Plot roc curves, depends RColorBrewer
# plot_type is "roc" or "pr"
rocPlot = function(rocData, type='roc', legPos=c(.56,.25), colorPal = 'Set1', colorManual=NULL, linetypes, legText=6, legSpace=3){

	if(type=='roc'){ 
    tmp=ggplot(rocData, aes(x=FPR, y=TPR, color=model, linetype=model)) + 
      geom_abline(intercept=0, slope=1, color='darkgrey') + 
      ylab('True Positive Rate (Sensitivity)') + xlab('False Positive Rate (1-Specificity)')

  }

  if(type=='pr'){ 
    tmp=ggplot(rocData, aes(x=rec, y=prec, color=model, linetype=model)) + 
      ylab('Precision') + xlab('Recall (True Positive Rate)')
  }

  if(is.null(colorManual)){
    tmp = tmp + scale_color_brewer(palette=colorPal)
  } else {
    tmp = tmp + scale_color_manual(values=colorManual)
  }

	tmp=tmp + 
    geom_line(lwd=1) +
    ylim(0,1) + 
    scale_linetype_manual(values=linetypes) + 
    theme_light(base_family="Source Sans Pro") + 
    theme(
  		legend.position=legPos, legend.title=element_blank(),
      legend.background=element_blank(), 
      legend.text.align = 0, legend.text=element_text(size=legText),
      legend.key=element_rect(colour = NA, fill = NA), legend.key.size=unit(legSpace,'lines'),
      axis.text.x=element_text(family="Source Sans Pro Light"),
      axis.text.y=element_text(family="Source Sans Pro Light"),    
      axis.ticks=element_blank(),    
  		panel.border=element_blank()
		)
	return(tmp)
}

# gg separation plot
# thanks to http://www.peterhaschke.com/r/2013/04/22/SeparationPlot.html
ggSep = function(actual, proba, color, lty, fPath, save=TRUE){
  color = c('white',color)
  sepData = data.frame(actual, proba)
  sepData = sepData[order(sepData$proba),]
  tmp=ggplot(sepData) + 
    geom_rect(aes(xmin = 0, xmax = seq(length.out = length(actual)), ymin = 0, ymax = 1), fill = "transparent") +
    geom_linerange(aes(color = factor(actual), ymin = 0, ymax = 1, x = seq(length.out = length(actual))), alpha = 0.5) +
    geom_line(aes(y = proba, x = seq(length.out = length(actual)), linetype=lty), lwd = 4) + 
    scale_linetype_manual(values=lty) +
    scale_color_manual(values=color) + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0), breaks=seq(0,1,.25)) + 
    theme(
      legend.position='none', 
      panel.grid=element_blank(), panel.border=element_rect(colour = "grey13"),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      axis.title=element_blank()
      )
  if(save){ ggsave(tmp, file=fPath, width=12, height=2) } else { return(tmp) }
}

####################################################################
# From: https://github.com/andybega/auc-pr/blob/master/auc-pr.r
# Author: Andy Beger
#   Functions for Precision-recall plot and AUC-PR

#' Area under the ROC curve
auc_roc <- function(obs, pred) {
  pred <- prediction(pred, obs)
  auc  <- performance(pred, "auc")@y.values[[1]]
  return(auc) }

#' Area under Precision-recall curve
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  xy <- subset(xy, !is.nan(xy$precision))
  res   <- trapz(xy$recall, xy$precision)
  return(res) }

# Function to create raw data needed to plot Precision against recall
# For a vector of observed and predicted, creates x-y coordinates for a ROC or PR curve.
rocdf <- function(pred, obs, data=NULL, type=NULL) {
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data) }
  
  rocr_xy <- switch(type, roc=c("tpr", "fpr"), pr=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(rocr_pr@x.values[[1]], rocr_pr@y.values[[1]])
  colnames(xy) <- switch(type, roc=c("tpr", "fpr"), pr=c("rec", "prec"))
  return(xy) }
####################################################################