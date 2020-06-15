#' Create an object needed to create QTL plots.
#'
#' Creates a list of information needed to plot QTL across a genome, for all or
#' only a subset of chromsomes.
#' 
#' For each chromosome specified a 'track' is created, which is further divided
#' into lanes for plotting QTL from different years or experiments. QTL
#' positions and their confidence intervals are plotted to the centre of these
#' lanes, and dividing lanes plotted between each lane. These lane centres and
#' margin positions can be used to add extra information, such as lane labels
#' above the chromosome.
#' 
#' The gap between chromosomes can be adjusted to include further embellishments,
#' and the plot  will rescale automatically.
#'
#' @param chr a numeric vector listing which chromosomes are to be plotted.
#' @param marker_locations A data.frame listing marker positions on each chromosome.
#' Should include a column labelled 'Position' and another labelled 'Chromosome'.
#' @param nlanes the number of lanes to plot for each chromosome.
#' @param left_gap,right_gap Additional space on the left- and right-hand sides
#' of each chromosome for additional labels.
#' 
#' @return A list of two tables detailing:
#' \enumerate{
#' \item  Chromosomes to be plotted
#' \item Number of chromosome tracks to be included
#' \item Number of lanes on each track
#' \item Lists of marker positions for each track
#' \item Lengths of each track
#' \item Gap to the left of each track
#' \item Gap to the right of each track
#' \item Table of x-axis poisitions of the centres of each lane.
#' \item Table of x-axis poisitions of the margins of each lane.
#' \item Optional list of labels for each lane.
#' }
#'
#' @export
plotQTL <-
function(chr=1:5, marker_locations, nlanes=2, left_gap=2, right_gap=2){
  nchr    <- length(chr) # number of chromosomes to plot 
  maxx    <- nchr * (10+left_gap+right_gap) - left_gap # right hand boundary of the plot
  chr_xv  <- 0:(nchr-1) * (10+left_gap+right_gap) # positions for the chromosome vertical lines
  
  # x-positions for the centre of each lane
  lane_xv     <- sapply(1:nchr, function(i) seq(chr_xv[i], chr_xv[i]+10, by=10/nlanes))
  lane_centre <- sapply(1:nchr, function(i) (seq(chr_xv[i], chr_xv[i]+10, by=10/nlanes)-(5/nlanes))[-1])
  # label row and column names
  rownames(lane_xv)     <- paste("margin", 1:(nlanes+1), sep="_")
  rownames(lane_centre) <- paste("lane", 1:nlanes, sep="_")
  colnames(lane_xv)     <- chr
  colnames(lane_centre) <- chr
  
  # create a list of marker positions for each track
  map <- split(marker_locations[,"Position"], marker_locations[,"Chromosome"])
  
  # return a plotQTL object
  return(list(chr           = chr,
              ntracks       = length(chr),
              nlanes        = nlanes,
              map           = map,
              maxx          = maxx,
              track_lengths = unlist(lapply(map, max)),
              left_gap      = left_gap,
              right_gap     = right_gap,
              lane_centres  = lane_centre,
              lane_margins  = lane_xv))
}
