#' Create a table of maximum likelihood and Bayesian credible interval positions for a QTL object.
#' @param qtlobject A QTL object from r/qtl.
#' @param model_fit A fitQTL object from r/qtl.
#' @return A data frame listing QTL name, the maximum likelihood position of the QTL and the upper and lower
#' confidence intervals.
#' 
#' @export
bayesint_table <- function(qtl_object, model_fit){
  if(class(qtl_object) != 'qtl'){
    warning("qtl_object should be of class 'qtl'.")
    return(NULL)
  }
  if(class(model_fit) != 'fitqtl'){
    warning("model_fit should be of class 'model_fit'.")
    return(NULL)
  }
  library(qtl)
  nqtl <- qtl_object$n.qtl
  # create data frame summarising qtl model.
  qtl_intervals <- as.data.frame(matrix(0, ncol=5, nrow=nqtl))
  colnames(qtl_intervals)<-c("name", "chr", "ML_bayesint","min_bayesint","max_bayesint")
  # fill in the data
  qtl_intervals$name <- qtl_object$name
  qtl_intervals$chr  <- qtl_object$chr
  for(q in 1:nqtl) qtl_intervals[q, 3:5] <- bayesint(qtl_object, qtl.index = q)[[2]][c(2,1,3)] # Get positions and CIs
  for(c in 2:5)    qtl_intervals[ , c  ] <- as.numeric(qtl_intervals[,c]) # coerce to numeric
  qtl_intervals$length                   <- qtl_intervals$max_bayesint - qtl_intervals$min_bayesint # length of the CIs
  
  # Pull out additive effect sizes for each QTL
  effect_sizes <- model_fit$ests$est[-1]
  effect_sizes <- effect_sizes[1:qtl_object$n.qtl]
  qtl_intervals$effect_sizes <- effect_sizes
  # Pull our SEs for each effect size
  effect_SE <- summary(model_fit)$ests[-1,2]
  effect_SE <- effect_SE[1:qtl_object$n.qtl]
  qtl_intervals$effect_SE <- effect_SE
  
  # If there is only one QTL, get LOD and PVE for the model.
  if(qtl_object$n.qtl == 1){
    qtl_intervals$PVE <- model_fit$result.full[1,5]
    qtl_intervals$LOD <- model_fit$result.full[1,4]
  }
  # If there are multiple QTL get LOD and PVE for each locus.
  if(qtl_object$n.qtl > 1){
    # Pull out additive effect sizes for each QTL
    pve               <- model_fit$result.drop[,4]
    pve               <- pve[1:qtl_object$n.qtl]
    qtl_intervals$PVE <- pve
    # Pull out LOD scores for each QTL
    lod_scores        <- model_fit$result.drop[,3]
    lod_scores        <- lod_scores[1:qtl_object$n.qtl]
    qtl_intervals$LOD <- lod_scores
  }
  #qtl_intervals[,7:9] <- round(qtl_intervals[,7:9], 2)
  return(qtl_intervals)
}
