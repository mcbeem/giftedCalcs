
#' true_corrs: calculates the true score correlation matrix from an observed score
#'   correlation matrix
#'
#' @param corr an n by n observed correlation matrix
#' @param rely a vector of reliability coefficients
#'
#' @return a correlation matrix of true scores which are disattentuated for
#'   measurement error

true_corrs <- function(corr, rely) {
  l <- nrow(corr)

  mat <- matrix(NA, nrow = l, ncol = l)

  for (i in 1:l) { # i will index rows

    for (j in 1:l) { # j will index cols

      mat[i, j] <- corr[i, j] / sqrt(rely[i] * rely[j])
    }
  }

  diag(mat) <- 1

  return(mat)
}



#' cross_corrs_upper_right: calculates the cross correlation between observed
#'   scores (rows) and their true score counterparts (cols). Creates the submatrix
#'   which occupies the upper right block of the full correlation matrix
#'
#' @param corr a matrix of observed correlations
#' @param rely a vector of reliability coefficients
#'
#' @return a correlation matrix of disattenuated cross-correlations

cross_corrs_upper_right <- function(corr, rely) {
  l <- nrow(corr)

  mat <- matrix(NA, nrow = l, ncol = l)

  for (i in 1:l) { # i will index rows

    for (j in 1:l) { # j will index cols

      mat[i, j] <- corr[i, j] / sqrt(rely[j])
    }
  }

  diag(mat) <- sqrt(rely)

  return(mat)
}

#' cross_corrs_lower_left: calculates the cross correlation between true
#'   scores (rows) and their observed score counterparts (cols). Creates the submatrix
#'   which occupies the lower left block of the full correlation matrix
#'
#' @param corr a matrix of observed correlations
#' @param rely a vector of reliability coefficients
#'
#' @return a correlation matrix of disattenuated cross-correlations

cross_corrs_lower_left <- function(corr, rely) {
  l <- nrow(corr)

  mat <- matrix(NA, nrow = l, ncol = l)

  for (i in 1:l) { # i will index rows

    for (j in 1:l) { # j will index cols

      mat[i, j] <- corr[i, j] / sqrt(rely[i])
    }
  }

  diag(mat) <- sqrt(rely)

  return(mat)
}


#' rowwise_compare: function for comparing two vectors
#'
#' @param datarow numeric vector; the row to be compared
#' @param bounds numveric vector; the values to compare against. same length as datarow
#'
#' @return boolean value; TRUE if all elements of datarow are larger than the corresponding
#'   elements of bounds

rowwise_compare <- function(datarow, bounds) {
  return(all(datarow > bounds))
}


# define a 'not in' function
`%!in%` <- Negate(`%in%`)


#' Marginal psychometrics for multiple criteria identification systems
#'
#' This function calculates marginal psychometrics (currently only sensitivity) for
#' one- and two-stage multiple criteria identification systems. A plot method is
#' available for visualizing the distributions of scores on each assessment for the
#' identified students. This function uses Monte Carlo simulation to approximate the
#' integrals involved in computing the metrics; as such the results will vary somewhat
#' from run to run. Users should determine the necessary sample size (\code{n}) needed
#' to achieve the required level of precision. The default value of \code{n=50000} will
#' likely suffice for most situations. Users may also wish to set a random number seed
#' for reproducibility.
#'
#' @usage marginal_psychometrics_MC(policy, corr, rely, n=50000, nomination=NA,
#'   ignore_nomination=FALSE, labels=NA)
#'
#' @param policy a matrix describing the identification policy. assessments are in
#'   columns, pathways in are in rows. values are percentile cutoffs. multiple
#'   requirements within a row are joined by "and" combination rules, whereas the "or"
#'   rule joins across rows
#' @param corr a correlation matrix
#' @param rely a vector of reliability coefficients
#' @param n scalar, the number of samples to draw. defaults to 50,000
#' @param nomination vector defining which columns of the policy matrix,
#'   row / column of the correlation matrix, and element of the reliability
#'   vector is the nomination. Defaults to NA, which is interpreted as no
#'   nomination stage
#' @param ignore_nomination boolean. Should the nomination be ignored? This allows
#'   for convenient comparison of single- and two-stage versions of a multiple
#'   criteria policy without needing to respecify the other inputs. defaults to FALSE
#' @param labels an optional vector of labels for the assessments; defaults to NA
#'
#' @return a list with the following elements:
#'    \code{$identified}: the proportion of students that are identified
#'    \code{$gifted}: the proportion of students that are gifted
#'    \code{$sensitivity}: the sensitivity
#'    \code{$scores}: a data frame of scores for identified students
#'
#' @examples
#'
#' policy <- matrix(c(
#'   .9, .9, .9, 0,
#'   .9, 0, .9, .9,
#'   .9, 0, 0, .95
#' ), ncol = 4, byrow = TRUE)
#'
#' corr <- matrix(c(
#'   1, .5, .4, .3,
#'   .5, 1, .7, .6,
#'   .4, .7, 1, .5,
#'   .3, .6, .5, 1
#' ), byrow = TRUE, nrow = 4)
#'
#' rely <- c(.8, .9, .8, .85)
#'
#' result <- marginal_psychometrics_MC(
#'   n = 50000, policy = policy, corr = corr,
#'   rely = rely, nomination = 1, labels = c("nom", "IQ", "ach", "creativity"),
#'   ignore_nomination = FALSE
#' )
#'
#' result
#' @export

marginal_psychometrics_MC <- function(policy, corr, rely, n = 50000, nomination = NA,
                                      ignore_nomination = FALSE, labels = NA) {

  # convert policy values from percentiles to scores
  policy <- qnorm(policy)

  # if only one row of policy is passed, make it a matrix
  if ("matrix" %!in% class(policy)) {
    policy <- matrix(policy, nrow = 1)
  }


  # case 2: nomination is provided but should be ignored
  if (all(!is.na(nomination)) & ignore_nomination == TRUE) {
    # drop nomination cols from policy object, rows / cols for corrs object ,and rely entry
    policy <- policy[, -nomination]
    corr <- corr[-nomination, -nomination]
    rely <- rely[-nomination]

    if (all(!is.na(labels))) {
      # if labels were given, drop the nomination label
      labels <- labels[-nomination]
    }

    if ("matrix" %!in% class(policy)) {
      policy <- matrix(policy, nrow = 1)
    }

    if ("matrix" %!in% class(corr)) {
      corr <- matrix(corr, nrow = 1)
    }
  }

  # create full correlation matrix
  # cross-corr upper right
  ccur <- cross_corrs_upper_right(corr, rely)
  # cross-corr lower left
  cclr <- cross_corrs_lower_left(corr, rely)
  # true score correlations
  true <- true_corrs(corr, rely)
  # full correlation matrix
  full_mat <- cbind(rbind(corr, ccur), rbind(cclr, true))

  # define the vectors of lower bounds defining giftedness and identification
  policy_gifted <- cbind(matrix(-Inf, nrow = nrow(policy), ncol = ncol(policy)), policy)
  policy_identified <- cbind(policy, policy)

  # case 1: no nomination provided is the default!
  #  so don't alter the policy_gifted or policy_identified vectors

  # case 3: nomination is provided and it should be used
  if (ignore_nomination == FALSE & all(!is.na(nomination))) {
    # reset nomination criterion
    #  since nomination doesn't define who is gifted, set the appropriate columns
    #    on the true score side of the policy matrix to -Inf
    policy_gifted[, ncol(policy) + nomination] <- -Inf

    # do the same thing in the identification policy object, again only on the true score side
    policy_identified[, ncol(policy) + nomination] <- -Inf
  }

  # if the policy matrix has only one row, it has no or's. So an analytic solution is convenient.
  if (nrow(policy) == 1) {
    gifted <- mnormt::sadmvn(
      lower = policy_gifted,
      upper = rep(Inf, times = length(policy_gifted)),
      mean = rep(0, times = length(policy_gifted)),
      varcov = full_mat
    )[1]

    identified <- mnormt::sadmvn(
      lower = policy_identified,
      upper = rep(Inf, times = length(policy_gifted)),
      mean = rep(0, times = length(policy_gifted)),
      varcov = full_mat
    )[1]

    sensitivity <- identified / gifted

    results <- list(identified = identified, gifted = gifted, sensitivity = sensitivity)
  }

  # if the policy matrix has multiple rows, we can't do an analytic solution :( :( :(
  #  must resort to Monte Carlo

  else {

    # sample data from multivariate normal dist
    data <- mnormt::rmnorm(n = n, mean = rep(0, nrow(full_mat)), varcov = full_mat)

    # initialize sensitivity
    gifted_flag <- rep(0, times = n)
    identified_flag <- rep(0, times = n)

    for (i in 1:nrow(policy)) {
      gifted_flag <- mapply(
        max,
        apply(data, 1, rowwise_compare, policy_gifted[i, ]),
        gifted_flag
      )


      identified_flag <- mapply(
        max,
        apply(data, 1, rowwise_compare, policy_identified[i, ]),
        identified_flag
      )
    }

    sensitivity <- sum(identified_flag) / sum(gifted_flag)

    results <- list(
      identified = sum(identified_flag) / n, gifted = sum(gifted_flag) / n,
      sensitivity = sensitivity
    )
  }


  return(results)
}
