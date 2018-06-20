#' Set up an empty plot for QTL positions
#' 
#' Creates an empty genome mapping for plotting positions and confidence intervals
#' of QTL inferred using r/qtl
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param by Intervals between y-axis tick labels. Defaults to 20.
#' @param maxy upper limit of the y-axis.
#' @return An empty plot to which QTL tracks can be added.
#' 
#' @export
plotQTL_base <-
function(plotQTL, by=20, maxy= 11, miny=11, ...){
  # set the plot up.
  plot(c(-plotQTL$left_gap, plotQTL$maxx), c(-max(plotQTL$track_lengths)-miny, maxy),
       col="white", frame.plot=F, axes=F, xlab="", ylab="")
  title(ylab = "Marker distance (cM)", line = 1.5, ...) # label x-axis
  # set up y-axis
  maxy           <- -max(unlist(plotQTL$map)) # length of the longest chromosome.
  tick_positions <- seq(0, maxy, by = -by)    # positions for the y-axis tick labels.
  tick_labels    <- seq(0, -maxy,by = by)     # y-axis labels
  axis(2, pos = -plotQTL$left_gap, at=tick_positions, labels = tick_labels, ...) # draw a y-axis.
  
  # Label chromosome numbers.
  xv <- plotQTL$lane_margins[1,]+5
  yv <- rep(-max(unlist(plotQTL$map)) - 5, plotQTL$ntracks)
  text(xv, yv, plotQTL$chr, adj=0.5, ...)
}
