#' Add box to a plotQTL plot.
#' 
#' Add a rectangle to a plotQTL plot, onto which QTL positions can be plotted.
#' For example, boxes could denote regions in which QTL were found in previous studies.
#' By default boxes span all lanes of the chromosome. Boxes can be restricted to particular
#' lanes by specifying the lane margins in left_lane and right_lane.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param boxes A dataframe listing chomosome, lower bound and upper bounds for 
#' the box. This can be generated using cluster_qtl().
#' @param margins Optional vector of two elements giving the left- and right-most
#' margins for the box. If not used, the whole width of the chromosome will be
#' used.
#' @param ... Additional parameters passed to graphical functions.
#'
#' @export
plotQTL_boxes <- function(plotQTL, boxes, margins='full', ...){
  if(margins == 'full'){
    margins <- c(1, nrow(plotQTL$lane_margins))
  } else if(length(margins) != 2){
    stop("If margins are specified this should be a vector of two elements.")
  }
  boxes$track <- match(boxes$chr, colnames(plotQTL$lane_margins)) # vector denoting which track to plot to.
  boxes <- boxes[boxes$chr %in% 1:plotQTL$ntracks,]
  
  rect(plotQTL$lane_margins[margins[1],boxes$track],
       -boxes$upper,
       plotQTL$lane_margins[margins[2],boxes$track],
       -boxes$lower, ...)
}
