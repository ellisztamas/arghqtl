#' Points for QTL effect sizes.
#' 
#' Add points for QTL effect sizes to an existing qtleffects plot.
#' These can be raw effect sizes or percent variance explained (PVE).
#' For the former, confidence intervals are also plotted.
#' 
#' @param qtleffects A qtleffects object from qtleffects() denoting x-value positions.
#' @param qtlobject A QTL object from r/qtl.
#' @param model_fit A fitQTL object from r/qtl.
#' @param type Variable to be plotted. This should either be 'effect' to plot raw effect
#' sizes, or 'PVE' to plot percentages of variance explained.
#' @param ... further arguments passed to points.
#' 
#' @export
qtleffects_points <-
function(qtleffects, qtl_object, model_fit, type='effect', ...){
  xvals <- numeric(length(qtleffects$chr)) # empty vector to store x-values
  # Get x-values for each point
  row_number <- match(qtl_object$chr, qtleffects$chr_start$Chromosome)
  xvals      <- qtl_object$pos + qtleffects$chr_start$plot_position[row_number]
  # Get y-values and plot
  if(type == 'effect'){
    point_est <- model_fit$ests$ests[-1] # point estimates for effect sizes
    conf_int  <- summary(model_fit)$ests[-1,2] # confidence intervals for each QTL.
    # remove any epistatic QTL
    point_est <- point_est[1:qtl_object$n.qtl]
    conf_int  <- conf_int[1:qtl_object$n.qtl]
    # add confidence intervals
    arrows(xvals, point_est - conf_int,
           xvals, point_est + conf_int,
           length=0, angle = 90, code = 3, ...)
    points(xvals, point_est, ...) # plot effect sizes
  } else if(type == 'PVE') {
    points(xvals, model_fit$result.drop[1:qtl_object$n.qtl, 4], ...) # plot PVE
  } else {
    print("'type' should be either 'effect' to plot raw effect sizes, or 'PVE' to plot percentages of variance explained.")
    return(NULL)
  }
}
