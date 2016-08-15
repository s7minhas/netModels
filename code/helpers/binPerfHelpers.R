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
rocPlot = function(rocData, colorPal = 'Set1', type='roc'){
	if(type=='roc'){ tmp=ggplot(rocData, aes(x=FPR, y=TPR, color=model)) + geom_line() }
  if(type=='pr'){ tmp=ggplot(rocData, aes(x=rec, y=prec, color=model)) + geom_line() }
	if(type=='roc'){
    tmp=tmp + geom_abline(intercept=0, slope=1, color='darkgrey')
    tmp=tmp + ylab('True Positive Rate (Sensitivity)') + xlab('False Positive Rate (1-Specificity)')
  }
  if(type=='pr'){ tmp=tmp + ylab('Recall') + xlab('Precision') }
	tmp=tmp + scale_color_brewer(palette=colorPal)
	tmp=tmp + theme(
		legend.position='top', legend.title=element_blank(),
		panel.grid=element_blank(),
		axis.ticks=element_blank()
		)
	return(tmp)
}

####################################################################
####################################################################
####################################################################
# From: https://github.com/andybega/auc-pr/blob/master/auc-pr.r
# Author: Andy Beger
#   Functions for Precision-recall plot and AUC-PR
#

#' Area under the ROC curve
#'
#' improt ROCR
auc_roc <- function(obs, pred) {
  pred <- prediction(pred, obs)
  auc  <- performance(pred, "auc")@y.values[[1]]
  return(auc)
}

#' Area under Precision-recall curve
#'
#' import ROCR
#' import caTools
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  
  # take out division by 0 for lowest threshold
  xy <- subset(xy, !is.nan(xy$precision))
  
  res   <- trapz(xy$recall, xy$precision)
  res
}

# Function to create raw data needed to plot Precision against recall
#
# For a vector of observed and predicted, creates x-y coordinates for a ROC
# or PR curve.
rocdf <- function(pred, obs, data=NULL, type=NULL) {
  # plot_type is "roc" or "pr"
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data)
  }
  
  rocr_xy <- switch(type, roc=c("tpr", "fpr"), pr=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(rocr_pr@x.values[[1]], rocr_pr@y.values[[1]])
  colnames(xy) <- switch(type, roc=c("tpr", "fpr"), pr=c("rec", "prec"))
  return(xy)
}

# To plot, something like:
#xy <- rocdf(pred, obs, type="pr")
#plot(xy[, 1], xy[, 2])
####################################################################
####################################################################
####################################################################