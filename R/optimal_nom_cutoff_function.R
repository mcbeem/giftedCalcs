#' @examples 
#' optimal_nom_cutoff(relyt=.95, test.cutoff=.9, valid=.91)

#' @export
#' 
optimal_nom_cutoff <- function(relyt=.9, test.cutoff, valid=1e-7, mu=0) {
  
  nom.cutoff <- pnorm(seq(-3, 3, by=.05), 0, 1)
  
  perf_by_cutoff <- cbind(nom.cutoff,
                matrix(unlist(sapply(nom.cutoff, marginal_psychometrics, valid=valid, relyt=relyt, 
                                     test.cutoff=test.cutoff)), nrow=length(nom.cutoff), byrow=T)[,1:2])
  
  # find the optimal nomination cutoff for cost effectiveness
  rad <- -45 * (pi/180)
  efficiency = perf_by_cutoff[,1:2] %*% matrix(c(cos(rad), sin(rad), -sin(rad), cos(rad)), 2,2)
  bestindex <- which(efficiency[,2] == max(efficiency[,2]))
  
  # convert to data frame and add names
  perf_by_cutoff <- data.frame(perf_by_cutoff)
  names(perf_by_cutoff) <- c("nom.cutoff", "sensitivity", "IIR")
  
  # add proportional testing cost
  perf_by_cutoff$cost <- 1-perf_by_cutoff$nom.cutoff
  
  # select the optimal values and reset the row number
  bestvalues <- perf_by_cutoff[bestindex, ]
  rownames(bestvalues) <- NULL
  
  output <- list(
    perf_by_cutoff = perf_by_cutoff,
    optimal = bestvalues
  )
  
  class(output) = c("perf_by_cutoff", "list")
  attr(output, "hidden") <- c("perf_by_cutoff")
  
  return(output)
}
