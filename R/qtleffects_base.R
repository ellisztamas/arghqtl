#' Base plot for qtleffects
#'
#' Creates an empty plot for plotting QTL effect sizes from a qtleffects object.
#'
#' @param qtleffects A qtleffects object from qtleffects() denoting x-value positions.
#' @param y A vector of two elements denoting minimum and maximum y-vales. Equivalent to ylim argument.
#' @param ylabel Axis label for the y-axis. If NULL, this deaults to 'Effect size'.
#' @param tick.length Length of ticks for marker locations. If NULL, defaults to 3% of the total.
#' y-axis length. Negative values will anchor ticks above the minimum y value.
#' @param tick.lwd Line widths for marker locations.
#' @param box If TRUE, a border will be drawn around the plot.
#'
#' @export
qtleffects_base <-
function(qtleffects, y, ylabel=NULL, tick.length=NULL, tick.lwd=0.5, box=TRUE){
  if(length(y)!=2){
    print("y must be a vector with two elements denoting maximum and minumum y-values.")
    return(NULL)
  }
  if(is.null(ylabel)) ylabel <- "Effect size"
  if(is.null(tick.length)) tick.length <- abs(y[1] - y[2]) * 0.03
  # Create an empty plot with no axes
  plot(c(1, length(qtleffects$chr)*100), y, type='n', xlab = 'Chromosome', ylab=ylabel, axes=F, xlim = c(0, length(qtleffects$chr)*100)+50) 
  # add axes back in.
  axis(2)
  axis(1, at=100*(1:length(qtleffects$chr)), labels = qtleffects$chr)
  # Plot marker labels
  segments(qtleffects$marker_positions$plot_position, min(y),
           qtleffects$marker_positions$plot_position, min(y)-tick.length, 
           lwd = tick.lwd)
  if(box == TRUE) box() # add the plot border
}
