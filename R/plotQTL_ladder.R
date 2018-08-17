#' Plot marker positions in a plotQTL plot.
#' 
#' Plots a solid axis with marker locations to the left of each chromosome track,
#' and dashed grey dividers between lanes.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param marker_tick_width Width of marker labels.
#' @param col Colour for lane margins.
#' @param lty Line style.
#' @param ... Further graphical parameters passed to \code{segments}.
#' 
#' @export
plotQTL_ladder <-
function(plotQTL, marker_tick_width=0.5, col='gray50', lty='dashed', ...){
  # positions for the chromosome vertical lines
  chr_xv  <- plotQTL$lane_margins[1,]
  # Vertical lines for the main chromosome lines, with ticks for each marker.
  segments(chr_xv, 0, chr_xv, -plotQTL$track_lengths)
  for(i in 1:plotQTL$ntracks){ # tick markers
    segments(chr_xv[i],                   -plotQTL$map[[i]],
             chr_xv[i]-marker_tick_width, -plotQTL$map[[i]], lwd=0.5)
  }
  
  
  # plot thin horizontal dividers between plotted chromosomes
  xv <- as.vector(plotQTL$lane_margins[-1,])
  yv <- -rep(plotQTL$track_lengths, each=plotQTL$nlanes)
  segments(x0 = xv,
           y1 = 0,
           x1 = xv,
           y0 = yv,
           lty = lty, col = col, ...)
}
