#' Add underlined text to a plot.
#' 
#' @param x X-axis position for text.
#' @param y Y-axis position for text.
#' @param label Text to be plotted.
#' @param ... Further arguments to be passed to text().
#' 
#' @export
underlined <-
function(x, y, label, cex=1, ...){ 
  text(x, y, label, cex=cex, ...) 
  sw <- strwidth(label) * cex
  sh <- strheight(label) * cex
  lines(as.vector(x) + c(-sw/2, sw/2), rep(y - 1.5*sh/2, 2), ...) 
}
