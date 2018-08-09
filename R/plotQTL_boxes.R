#' Add box to a plotQTL plot.
#' 
#' Add a rectangle to a plotQTL plot, onto which QTL positions can be plotted.
#' For example, boxes could denote regions in which QTL were found in previous studies.
#' By default boxes span all lanes of the chromosome. Boxes can be restricted to particular
#' lanes by specifying the lane margins in left_lane and right_lane.
#' 
#' @param plotQTL A plotQTL object from plotQTL().
#' @param track Integer indicating which chromosome track in the plotQTL object to plot to.
#' @param upper Marker position for the top of the box.
#' @param lower Marker position for the top of the box.
#' @param left_lane Integer indicating the left-hand lane margin of the box.
#' @param right_lane Integer indicating the right-hand lane margin of the box.
#' @param lwd Width of the border.
#' @param border Colour of the border.
#' @param col Colour to fill the box.
#' 
#' @export
plotQTL_box <-
function(plotQTL, track, upper, lower, left_lane = NULL, right_lane=NULL, lwd=0.5, border="gray50", col="gray76", ...){
  # set the left hand boundary
  if(is.null(left_lane)){ # default to left-most lane
    x1 <- plotQTL$lane_margins[1,   track]
  } else if(is.numeric(left_lane)){
    x1 <- plotQTL$lane_margins[left_lane, track] # if left lane has been assigned, use this
  } else return(print("left_lane should be NULL or an integer."))
  
  if(is.null(right_lane)){
    x2 <- plotQTL$lane_margins[plotQTL$nlanes+1, track] # default to right most lane
  } else if(is.numeric(right_lane)){
    x2 <- plotQTL$lane_margins[right_lane, track] # assign a specific lane
  } else return(print("right_lane should be NULL or an integer."))
  # draw the box
  rect(x1, lower, x2, upper, col = col, border = border, lwd=lwd, ...)
}
