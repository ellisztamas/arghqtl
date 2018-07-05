#' Plot QTL positions.
#' 
#' Plot QTL positions and confidence intervals from onto a plotQTL plot.
#' This can be done with a qtl object with a fitqtl object, or else with a cluster_qtl
#' object. In the former case, maximum-likelihood positions are labelled with an upward
#' arrow if the reference allele increases the phenotype, and vice versa.
#' 
#' QTL with narrow confidence intervals are plotted with filled arrows. If confidence
#' intervals are greater than a threshold number of centimorgans, arrows are plotted empty.
#' This can be disabled by setting fill_cutoff to a larger number than the longest interval.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param qtlobject A QTL object from r/qtl. If this is specifed, model_fit should be
#' as well, but cluster_qtl should not.
#' @param model_fit A fitQTL object from r/qtl. If this is specifed, qtl_object should be
#' as well, but cluster_qtl should not.
#' @param cluster_qtl A cluster_qtl object listing information on QTL grouped into clusters.
#' Usually the output of cluster_qtl(). If this is specified, qtl_object and model_fit
#' should not be.
#' @param lane Integer indicating which lane to plot to.
#' @param fill_cutoff Threshold length of a confidence interval on QTL positions, above which
#' points will be empty, and below which points will be filled.
#' @param col Plotting colour. Defaults to black.
#' @param ... Additional plotting commands passed to points() and segments().
#' 
#' @export
plotQTL_qtl <-
  function(plotQTL, qtl_object=NULL, model_fit=NULL, cluster_qtl=NULL, lane, fill_cutoff=NULL, col=1, ...){
    if(class(qtl_object) == "qtl" & class(model_fit) == "fitqtl" & is.null(cluster_qtl)){
      qtl_positions <- bayesint_table(qtl_object, model_fit)
      xv <- plotQTL$lane_centres[lane,match(qtl_positions$chr, colnames(plotQTL$lane_centres))]
      
      # lengths of each interval
      qtl_positions$interval_length <- qtl_positions$max_bayesint - qtl_positions$min_bayesint
      # add arrows indicating direction of QTL
      qtl_positions$pch[qtl_positions$effect_size >=0] <- 24
      qtl_positions$pch[qtl_positions$effect_size < 0] <- 25
      qtl_positions$pch[qtl_positions$effect_size ==0] <- 18
      
      # If fill_cutoff is NULL, set the maximum length to something longer than the longest interval.
      if(is.null(fill_cutoff)) fill_cutoff <- max(qtl_positions$interval_length)+1
      # create a vector for whether arrows should be filled white or the same colour as the outline
      qtl_positions$bg <- numeric(nrow(qtl_positions))
      qtl_positions$bg[qtl_positions$interval_length <  fill_cutoff] <- col
      qtl_positions$bg[qtl_positions$interval_length >= fill_cutoff] <- 'white'
      # add confidence intervals
      segments(xv, -qtl_positions$max_bayesint, xv, -qtl_positions$min_bayesint, col=col, ...)
      # plot ML positions
      points(xv, -qtl_positions$ML_bayesint, pch=qtl_positions$pch, col=col, bg=qtl_positions$bg, ...)
    }
    
    # Plot point estimates and credible intervals for a cluster object.
    else if (is.null(qtl_object) & is.null(model_fit) &!is.null(cluster_qtl)){
      cluster_qtl <- cluster_qtl$summary
      xv <- plotQTL$lane_centres[lane, match(cluster_qtl$chr, colnames(plotQTL$lane_centres))] # x-axis values.
      
      cluster_qtl$interval_length <- cluster_qtl$pos_max - cluster_qtl$pos_min
      # add arrows indicating direction of QTL
      cluster_qtl$pch[cluster_qtl$effect_mean >= 0] <- 24
      cluster_qtl$pch[cluster_qtl$effect_mean <  0] <- 25
      cluster_qtl$pch[cluster_qtl$effect_mean == 0] <- 18
      
      # If fill_cutoff is NULL, set the maximum length to something longer than the longest interval.
      if(is.null(fill_cutoff)) fill_cutoff <- max(cluster_qtl$interval_length)+1
      # create a vector for whether arrows should be filled white or the same colour as the outline
      cluster_qtl$bg <- numeric(nrow(cluster_qtl))
      cluster_qtl$bg[cluster_qtl$interval_length <  fill_cutoff] <- col
      cluster_qtl$bg[cluster_qtl$interval_length >= fill_cutoff] <- 'white'
      # plot CIs and point estimates
      segments(xv, -cluster_qtl$pos_min, xv, -cluster_qtl$pos_max, col=col)
      points(xv, -cluster_qtl$pos_mean, col=col, pch= cluster_qtl$pch, bg=cluster_qtl$bg)
    }
    else warning("Supply either a qtl object with a fitqtl object, or else a QTL cluster object from cluster_qtl.")
  }
