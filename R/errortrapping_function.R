#' Function for catching out-of-range parameter values
#'
#' \code{errortrapping} catches improper input values. 
#'
#' Internal function used to validate parameters supplied to other functions. 
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#' 
#' @export

errortrapping <- function(relyt=.9999999, valid=1E-30,
                          nom.cutoff=.1, test.cutoff=.9, mu=0) {
  
  if (relyt <= 0 | relyt >= 1) {
    stop("\nThe value of relyt must be between zero and one. It is a reliability coefficient similar to r squared. It cannot be exactly zero or exactly one due to computational instability. Use a value like .0000001 instead of zero or .9999999 instead of one.")
  }
  
  if (valid <= 0 | valid >= 1) {
    stop("\nThe value of valid must be between zero and one. It is a validity coefficient. It cannot be exactly zero or exactly one due to computational instability. Use a value like .0000001 instead of zero or .9999999 instead of one.")
  }
  
  if (valid > sqrt(relyt)) {
    stop(paste0("\nThe nomination validity (valid) cannot exceed the square root of the test reliability (relyt). In such a case, the implied correlation between the nomination and test true scores exeeds 1.0. For the current setting of test reliability, the maximum nomination validity is ", round(sqrt(relyt),2), ". ",
                "Please increase test reliability or decrease nomination validity."))
  }
  
  if (nom.cutoff <= 0 | nom.cutoff >= 1) {
    stop("\nThe value of nom.cutoff must be between zero and one. It is the percentile cutoff score for nomination. For example, a value of 0.9 is a cutoff at the 90th percentile. It cannot be exactly zero or one, as these would imply infinite cutoff values. The pnorm() function can be used to convert a known cutoff score into a percentile. For example, a cutoff of 120 can be converted to a percentile by running\n\npnorm(120, mean=100, sd=15)")
  }
  
  if (test.cutoff <= 0 | test.cutoff >= 1) {
    stop("\nThe value of test.cutoff must be between zero and one. It is the percentile cutoff score for the confirmatory test. For example, a value of 0.9 is a cutoff at the 90th percentile. It cannot be exactly zero or one, as these would imply infinite cutoff values. The pnorm() function can be used to convert a known cutoff score into a percentile. For example, a cutoff of 120 can be converted to a percentile by running\n\npnorm(120, mean=100, sd=15)")
  }
  
  if (abs(mu)>1) {
    warning("\nThe population mean true score (mu) is represented as a z-score and is unlikely to be outside the range of -1 to +1. Please verify your value of mu.")
  }
}