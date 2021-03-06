#' Group QTL from different experiments into blocks.
#' 
#' Loci in a list of QTL objects are grouped by position.
#' Loci are declared to be colocalised if the confidence intervals on locus
#' position overlap. When the position of a QTL is poorly defined and confidence
#' intervals are large, the locus colocalises with many other loci, probably
#' erroneously. To combat this, set a maximum length using the argument
#' threshold; QTL with confidence intervals longer than this will be excluded
#' from clustering.
#' 
#' @param chr A numeric vector listing which chromosomes are to be plotted.
#' @param qtl_list A list of QTL objects from r/qtl (i.e list(object1, object2, ...)).
#' @param model_fit_list A list of model fit objects (i.e. objects outputted from `fit.qtl`).
#' @param threshold Maximum length in centimorgans above which QTL are considered to be
#' poorly defined and excluded from clustering.
#' @param hard_boundary Logical. Determines whether to separate pairs QTL that
#' overlap at only a single marker, i.e. the end of the credible interval of the
#' first QTL falls at the same marker where the credible interval begins. If
#' TRUE, these QTL are treated as distinct. Defaults to TRUE.
#' @param qtl_labels Prefix for labels for QTL. Usually a string. Defaults to "QTL".
#' 
#' @return Two data frames:
#' \enumerate{
#' \item{1. A summary table of each cluster, indicating the
#' mean and the maximum extent of 95% credible intervals, effect sizes, and
#' percentage of total phenotypic variance explained for all QTL in the cluster.
#' See \code{?cluster_qtl_summary}.}
#' \item{2. Full details of all the QTL, indicating which clutser they belong to. }
#' }
#' @export
cluster_qtl <- function(chr, qtl_list, model_fit_list, threshold = NULL, hard_boundary = TRUE, qtl_labels = "QTL"){
  if(!is.logical(hard_boundary)){
    stop("hard_boundary should be TRUE or FALSE.")
  }
  # Get ML positions for each QTL
  ax <- vector('list', length(qtl_list))
  names(ax) <- names(qtl_list)
  for(l in names(qtl_list)){
    ax[[l]] <- cbind(
      experiment=l,
      bayesint_table(qtl_list[[l]], model_fit_list[[l]])
      )
  }
  ax <- do.call('rbind', ax)
  ax <- ax[order(ax$ML_bayesint),] # sort by position
  
  clusters <- list() # empty list
  counter <- 1       # initialise counter
  
  for(c in chr){ #loop over chromosomes
    this_chr <- ax[ax$chr == c,] # pull out this chromosome only
    if(is.numeric(threshold)) this_chr <- this_chr[this_chr$length <= threshold,] # ignore QTL which have poorly defined intervals
    
    while(nrow(this_chr)  != 0){
      clusters[[counter]] <- this_chr[1,] # initialise first cluster with the first entry
      this_chr <- this_chr[-1,]           # remove that entry from the dataset.
      
      size_change <- 1 # change in size of this_chr. We've just removed the first entry, so this is 1.
      while(size_change != 0){ # repeat as long as the change in this_chr is more than one row.
        # max and min positions of the current cluster
        mv        <- c(min(clusters[[counter]]$min_bayesint), max(clusters[[counter]]$max_bayesint))
        # Which entries overlap with the LH, RH or both boundary of the cluster?
        if(hard_boundary == TRUE){
          lh_border <- (this_chr$min_bayesint < mv[1]) * (this_chr$max_bayesint > mv[1])
          rh_border <- (this_chr$min_bayesint < mv[2]) * (this_chr$max_bayesint > mv[2])
          middle    <- (this_chr$min_bayesint >= mv[1]) * (this_chr$max_bayesint <= mv[2])
        } else {
          lh_border <- (this_chr$min_bayesint <= mv[1]) * (this_chr$max_bayesint >= mv[1])
          rh_border <- (this_chr$min_bayesint <= mv[2]) * (this_chr$max_bayesint >= mv[2])
          middle    <- (this_chr$min_bayesint >= mv[1]) * (this_chr$max_bayesint <= mv[2])
        }
        #Send entries matching any of these criteria to the cluster
        clusters[[counter]] <- rbind(clusters[[counter]], this_chr[as.logical(lh_border + rh_border + middle),])
        # remove entries from the dataset.
        this_chr      <- this_chr[!as.logical(lh_border + rh_border + middle),]
        size_change   <- sum(lh_border + rh_border + middle) # record how many entries were removed.
      }
      clusters[[counter]]$QTL_id <- paste(qtl_labels, sprintf("%03d", counter), sep="_") # add a label for this cluster
      counter <- counter + 1
    }
  }
  qtl_clusters <- do.call('rbind', clusters) # create a data.frame from the list.
  # sort by chromosome and position
  qtl_clusters <- do.call('rbind', as.list(by(qtl_clusters, qtl_clusters$chr, function(qtl_clusters) qtl_clusters[order(qtl_clusters$ML_bayesint),])))
  rownames(qtl_clusters) <- 1:nrow(qtl_clusters)
  
  smmry <- cluster_qtl_summary(qtl_clusters)
  return(list(summary = smmry,
              full.list = qtl_clusters,
              boxes=make_box(smmry)))
}



