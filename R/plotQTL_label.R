#' Plot a label for QTL on the left of a track.
#' 
#' Plot labels for QTL or QTL regions on the left of a track in a plotQTL plot.
#' If map positions are supplied, a bar can be drawn to indicate the extent of a
#' QTL region.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param chr Chromosome.
#' @param label Text to plot
#' @param underlined Logical statement indicating whether the label should be
#' underlined.
#' @param plot_bar Logical statement indicating whether a vertical bar should be
#' plotted with the label.
#' @param pos Y-axis position for the label. If this is NULL, the midpoint of max
#' and min is used.
#' @param min Starting map position of the bar, if this is to be plotted.
#' @param max End map position of the bar, if this is to be plotted.
#' @param bar_offset Distance from the chromosome track to plot the bar.
#' @param text_offset Distance from the chromosome track to plot the label.
#' @param ... Further arguments to be passed to text
#' 
#' @export
plotQTL_label <-
function(plotQTL, chr, label, underlined=FALSE, plot_bar=FALSE, pos=NULL, min=NULL, max=NULL, bar_offset=2, text_offset=6, ...){
  track <- match(chr, plotQTL$chr)
  
  #if(plot_bar == TRUE){
    midpoint <-  -mean(c(max, min))
    segments(plotQTL$lane_margins[1,track] - bar_offset, -min,
             plotQTL$lane_margins[1,track] - bar_offset, -max, ...)
  #}
  text(plotQTL$lane_margins[1,track] - text_offset, midpoint, label, ...)
}
