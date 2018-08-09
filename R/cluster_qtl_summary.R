#' Make a summary table of clusters of QTL.
#' 
#' Creates a data.frame summarising information about groups of QTL clustered
#' using cluster_qtl(). Clusters will be labelled with the chromosome and index,
#' with an optional prefix.
#' 
#' @param qtl_clusters Data.frame containing information on clustered QTL,
#' usually from the output of cluster_qtl.
#' 
#' @return Data.frame listing information for each cluster. Information
#' includes the chromosome number, number of QTL in the cluster, then the range
#' and weighted mean values for position (cM), effect size and precentage of the
#' variation explained.
#' 
#' @export
cluster_qtl_summary <- function(qtl_clusters){
  # If a whole object from cluster_qtl has been supplied, just use the full.list.
  if(is.list(qtl_clusters) & all(names(qtl_clusters) == c("summary", "full.list"))){
    qtl_clusters <- qtl_clusters$full.list
  }
  # summarise information about the QTL clusters.
  summ <- data.frame(QTL_id      = unique(qtl_clusters$QTL_id),
                     chr         = tapply(qtl_clusters$chr, qtl_clusters$QTL_id, mean),
                     n.qtl       = as.integer(table(qtl_clusters$QTL_id)),
                     pos_mean    = as.numeric(by(qtl_clusters, qtl_clusters$QTL_id, function(x) weighted.mean(x$ML_bayesint, x$LOD))),
                     pos_min     = tapply(qtl_clusters$ML_bayesint, qtl_clusters$QTL_id, min),
                     pos_max     = tapply(qtl_clusters$ML_bayesint, qtl_clusters$QTL_id, max),
                     effect_mean = as.numeric(by(qtl_clusters, qtl_clusters$QTL_id, function(x) weighted.mean(x$effect_sizes, x$LOD))),
                     effect_min  = tapply(qtl_clusters$effect_sizes, qtl_clusters$QTL_id, min),
                     effect_max  = tapply(qtl_clusters$effect_sizes, qtl_clusters$QTL_id, max),
                     PVE_mean    = as.numeric(by(qtl_clusters, qtl_clusters$QTL_id, function(x) weighted.mean(x$PVE, x$LOD))),
                     PVE_min     = tapply(qtl_clusters$PVE, qtl_clusters$QTL_id, min),
                     PVE_max     = tapply(qtl_clusters$PVE, qtl_clusters$QTL_id, max))
  summ <- do.call('rbind', as.list(by(summ, summ$chr, function(summ) summ[order(summ$pos_mean),]))) # coerce to data.frame
  rownames(summ) <- paste(summ$chr,":", unlist(sapply(table(summ$chr), seq)), sep="") # add labels for each QTL.
  return(summ)
}