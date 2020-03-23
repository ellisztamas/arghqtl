#' Underlined text
#' 
#' Add underlined text to a plot.
#' 
#' This is a workaround to get unerlined text on a plot. This is edited from a
#' post by Uwe Ligges at
#' \url{https://tolstoy.newcastle.edu.au/R/help/02a/0471.html}.
#' 
#' It will not work if the text is at an angle (i.e. \code{srt != 0}), or if
#' the axis is on a log scale.
#' 
#' @param x X-axis position for text.
#' @param y Y-axis position for text.
#' @param label Text to be plotted.
#' @param cex A numerical value giving the amount by which plotting text and
#' symbols should be magnified relative to the default. See `?par`.
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
