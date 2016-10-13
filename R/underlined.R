#' Add underlined text to a plot.
#' 
#' @param x X-axis position for text.
#' @param y Y-axis position for text.
#' @param label Text to be plotted.
#' @param ... Further arguments to be passed to text().
#' 
#' @export
underlined <-
function(x, y, label, ...){ 
  text(x, y, label, ...) 
  sw <- strwidth(label) 
  sh <- strheight(label) 
  lines(x + c(-sw/2, sw/2), rep(y - 1.5*sh/2, 2), ...) 
}
