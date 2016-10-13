#' Plot QTL positions.
#' 
#' Plot QTL positions and confidence intervals from an r/qtl object onto a plotQTL plot.
#' Maximum-likelihood positions are labelled with an upward arrow if the reference allele
#' increases the phenotype, and vice versa.
#' 
#' QTL with narrow confidence intervals are plotted with filled arrows. If confidence
#' intervals are greater than a threshold number of centimorgans, arrows are plotted empty.
#' This can be disabled by setting fill_cutoff to a larger number than the longest interval.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param qtlobject A QTL object from r/qtl.
#' @param model_fit A fitQTL object from r/qtl.
#' @param lane Integer indicating which lane to plot to.
#' @param fill_cutoff Threshold length of a confidence interval on QTL positions, above which
#' points will be empty, and below which points will be filled.
#' @param col Plotting colour.
#' @param ... Additional plotting commands.
#' 
#' @export
plotQTL_qtl <-
function(plotQTL, qtl_object, model_fit, lane, fill_cutoff=0, col=1, ...){
  qtl_positions <- bayesint_table(qtl_object, model_fit)
  xv <- plotQTL$lane_centres[lane,match(qtl_positions$chr, colnames(plotQTL$lane_centres))]
  
  # lengths of each interval
  qtl_positions$interval_length <- qtl_positions$max_bayesint - qtl_positions$min_bayesint
  # add arrows indicating direction of QTL
  qtl_positions$pch[qtl_positions$effect_size >=0] <- 24
  qtl_positions$pch[qtl_positions$effect_size < 0] <- 25
  qtl_positions$pch[qtl_positions$effect_size ==0] <- 18
  # create a vector for whether arrows should be filled white or the same colour as the outline
  qtl_positions$bg <- numeric(nrow(qtl_positions))
  qtl_positions$bg[qtl_positions$interval_length <  fill_cutoff] <- col
  qtl_positions$bg[qtl_positions$interval_length >= fill_cutoff] <- 'white'
  
  # add confidence intervals
  segments(xv, -qtl_positions$max_bayesint, xv, -qtl_positions$min_bayesint, col=col, ...)
  # plot ML positions
  points(xv, -qtl_positions$ML_bayesint, pch=qtl_positions$pch, bg=qtl_positions$bg, col=col, ...)
}
