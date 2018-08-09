#' Plot marker positions in a plotQTL plot.
#' 
#' Plots a solid axis with marker locations to the left of each chromosome track,
#' and dashed grey dividers between lanes.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param marker_tick_width Width of marker labels.
#' @param col Colour for lane margins.
#' @param lty Line style.
#' 
#' @export
plotQTL_ladder <-
function(plotQTL, marker_tick_width=0.5, col='gray50', lty='dashed'){
  # positions for the chromosome vertical lines
  chr_xv  <- plotQTL$lane_margins[1,]
  # Vertical lines for the main chromosome lines, with ticks for each marker.
  segments(chr_xv, 0, chr_xv, -plotQTL$track_lengths)
  for(i in 1:plotQTL$ntracks){
    segments(chr_xv[i],                   -plotQTL$map[[i]],
             chr_xv[i]-marker_tick_width, -plotQTL$map[[i]], lwd=0.5)
  }
  # plot thin horizontal dividers between plotted chromosomes
  xv <- as.vector(plotQTL$lane_margins)
  lane_index   <- sort(rep(1:nrow(plotQTL$lane_centres), nrow(plotQTL$lane_margins)))
  lane_lengths <- rep(plotQTL$track_lengths, nrow(plotQTL$lane_margins))[lane_index]
  segments(xv, 0, xv, -lane_lengths, lty=lty, col=col)
}
