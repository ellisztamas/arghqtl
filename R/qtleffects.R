#' Create qtleffects object
#'
#' Creates an object needed to plot effect sizes of QTL from r/qtl.
#'
#' @param chr a numeric vector listing which chromosomes are to be plotted.
#' @param marker_locations A data.frame listing marker positions on each chromosome.
#' Should include a column labelled 'Position' and another labelled 'Chromosome'.
#' @param nlanes the number of lanes to plot for each chromosome. Defaults to two for Sweden vs. Italy.
#' left_gap, right_gap left- and right-hand margin around each chromosome.
#' @return A qtleffects object listing chromosomes to be plotted, x-values for marker positions,
#' and x-values for the start of each chromosome.
#'
#' @export
qtleffects <-
function(chr, marker_locations){
  # subset marker locations to pull out data only for chromosomes in chr
  marker_locations <- marker_locations[marker_locations$Chromosome %in% chr,]
  
  plot_markers <- vector('list', length(chr)) # empty list
  for(i in 1:length(chr)){
    this_chr               <- marker_locations[marker_locations$Chromosome == chr[i],] # subset data for this chromosome
    this_chr$chr_id        <- i # assign an index
    midpoint               <- max(this_chr$Position) /2
    this_chr$plot_position <- this_chr$Position - midpoint + i*100 # offset marker positions
    plot_markers[[i]]      <- this_chr # send to plot_markers
  }
  plot_markers <- do.call('rbind', plot_markers) # convert list to data.frame
  chr_start    <- plot_markers[match(1:length(chr), plot_markers$chr_id),] # summary of the starting position for each chromosome
  return(list(chr=chr, marker_positions = plot_markers, chr_start = chr_start))
}
