#' Convert the output of cluster_qtl_summary to a list of box coordinates
#' @param dat Output of cluster_qtl_summary.
#' @return Dataframe listing chromosome name, and upper and lower bounds of the
#' box.
make_box <- function(dat){
  qtlboxes <- by(dat, dat$QTL_id, function(x) c(unique(x$chr), min(x$pos_min), max(x$pos_max)))
  qtlboxes <- as.data.frame(do.call('rbind', qtlboxes))
  colnames(qtlboxes) <- c("chr", "lower", "upper")
  return(qtlboxes)
}
