#' Points for QTL effect sizes.
#' 
#' Add points for QTL effect sizes or PVE to an existing qtleffects plot.
#' Optionally, credible intervals for QTL position are also plotted.
#' Ploting can either use qtl and fitqtl objects, or a cluster_object.
#' 
#' @param qtleffects A qtleffects object from qtleffects() denoting x-value positions.
#' @param qtlobject A QTL object from r/qtl. If this is specifed, model_fit should be
#' as well, but cluster_qtl should not.
#' @param model_fit A fitQTL object from r/qtl. If this is specifed, qtl_object should be
#' as well, but cluster_qtl should not.
#' @param cluster_qtl A cluster_qtl object listing information on QTL grouped into clusters.
#' Usually the output of cluster_qtl(). If this is specified, qtl_object and model_fit
#' should not be.
#' @param type Variable to be plotted. This should either be 'effect' to plot raw effect
#' sizes, or 'PVE' to plot percentages of variance explained.
#' @param plot_pos logical; if TRUE, plot confidence intervals for QTL position.
#' @param effect_weight scalar multiplier for the magnitude of effect sizes.
#' @param ... Further arguments passed to points.
#' 
#' @export
qtleffects_points <- function(qtleffects, qtl_object=NULL, model_fit=NULL, cluster_qtl=NULL, type='effect', plot_pos=T, plot_points=T, effect_weight=1, ...){
  if(!plot_pos & !plot_points) {
    print("plot_pos and plot_points both set to FALSE. There is nothing to plot!")
    return(NULL)
  }
  # plot points when a qtl and a fitqtl object have been supplied
  if(class(qtl_object) == "qtl" & class(model_fit) == "fitqtl" & is.null(cluster_qtl)){
    bi <- bayesint_table(qtl_object, model_fit) # Summary table of QTL positions and effects
    # adjust effect sizes for weights
    bi$effect_sizes <- bi$effect_sizes * effect_weight
    bi$effect_SE    <- bi$effect_SE * effect_weight
    
    xvals <- numeric(length(qtleffects$chr)) # empty vector to store x-values
    # Get x-values for each point
    row_number <- match(qtl_object$chr, qtleffects$chr_start$Chromosome)
    xvals      <- qtl_object$pos + qtleffects$chr_start$plot_position[row_number]
    # Get y-values and plot
    if(type == 'effect'){
      # add standard errors for effect sizes
      arrows(xvals, bi$effect_sizes - bi$effect_SE,
             xvals, bi$effect_sizes + bi$effect_SE,
             length=0, angle = 90, code = 3, ...)
      # if desired, add CIs for QTL position.
      if(plot_pos) segments(bi$min_bayesint + qtleffects$chr_start$plot_position[row_number], bi$effect_sizes, 
                            bi$max_bayesint + qtleffects$chr_start$plot_position[row_number], bi$effect_sizes, ...)
      if(plot_points) points(xvals, bi$effect_sizes, ...) # plot effect sizes
    } else if(type == 'PVE') {
      if(plot_pos) segments(bi$min_bayesint + qtleffects$chr_start$plot_position[row_number], bi$PVE, 
                            bi$max_bayesint + qtleffects$chr_start$plot_position[row_number], bi$PVE, ...)
      if(plot_points) points(xvals, model_fit$result.drop[1:qtl_object$n.qtl, 4], ...) # plot PVE
    } else {
      print("'type' should be either 'effect' to plot raw effect sizes, or 'PVE' to plot percentages of variance explained.")
      return(NULL)
    }
  }
  
  # Plot points if a QTL cluster object has been supplied.
  else if (is.null(qtl_object) & is.null(model_fit) &!is.null(cluster_qtl)){
    cluster_qtl$summary[, c('effect_mean', 'effect_min', 'effect_max')] <- cluster_qtl$summary[, c('effect_mean', 'effect_min', 'effect_max')] * effect_weight
    xvals <- numeric(length(qtleffects$chr)) # empty vector to store x-values
    # Get x-values for each point
    row_number <- match(cluster_qtl$summary$chr, qtleffects$chr_start$Chromosome)
    xvals      <- cluster_qtl$summary$pos_mean + qtleffects$chr_start$plot_position[row_number]
    # Get y-values and plot
    if(type == 'effect'){
      # add ranges for effect sizes
      arrows(xvals, cluster_qtl$summary$effect_min,
             xvals, cluster_qtl$summary$effect_max,
             length=0, angle = 90, code = 3, ...)
      # if desired, add CIs for QTL position.
      if(plot_pos) segments(cluster_qtl$summary$pos_min + qtleffects$chr_start$plot_position[row_number], cluster_qtl$summary$effect_mean, 
                            cluster_qtl$summary$pos_max + qtleffects$chr_start$plot_position[row_number], cluster_qtl$summary$effect_mean, ...)
      if(plot_points) points(xvals, cluster_qtl$summary$effect_mean, ...) # plot effect sizes
    }
    if(type=='PVE'){
      # if desired, add CIs for QTL position.
      if(plot_pos) segments(cluster_qtl$summary$pos_min + qtleffects$chr_start$plot_position[row_number], cluster_qtl$summary$effect_mean, 
                            cluster_qtl$summary$pos_max + qtleffects$chr_start$plot_position[row_number], cluster_qtl$summary$effect_mean, ...)
      if(plot_points) points(xvals, cluster_qtl$summary$effect_mean, ...) # plot PVE
    }
  }
  else print("Supply either a qtl object with a fitqtl object, or else a QTL cluster object from cluster_qtl.")
}
