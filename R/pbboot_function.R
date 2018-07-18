#' Bootstrapping with a progress bar
#'
#' This function reproduces the code from the boot package's
#'  boot function, making minor changes in order to add a progress
#'  bar using the pbapply package. This code was adopted from a response
#'  posted to StackOverflow by user psolymos. Documentation from boot::boot follows.
#'
#'  Generate R bootstrap replicates of a statistic applied to data. Both parametric
#'  and nonparametric resampling are possible. For the nonparametric bootstrap, possible
#'  resampling methods are the ordinary bootstrap, the balanced bootstrap, antithetic
#'  resampling, and permutation. For nonparametric multi-sample problems stratified
#'  resampling is used: this is specified by including a vector of strata in the call
#'  to boot. Importance resampling weights may be specified.
#'
#' @param data The data as a vector, matrix or data frame. If it is a matrix
#'   or data frame then each row is considered as one multivariate observation.
#' @param statistic A function which when applied to data returns a vector
#'   containing the statistic(s) of interest. When \code{sim = "parametric"}, the
#'   first argument to \code{statistic} must be the data. For each replicate a
#'   simulated dataset returned by \code{ran.gen} will be passed. In all other cases
#'   statistic must take at least two arguments. The first argument passed will
#'   always be the original data. The second will be a vector of indices, frequencies
#'   or weights which define the bootstrap sample. Further, if predictions are required,
#'   then a third argument is required which would be a vector of the random indices
#'   used to generate the bootstrap predictions. Any further arguments can be passed to
#'   \code{statistic} through the \code{...} argument.
#' @param R The number of bootstrap replicates. Usually this will be a single positive
#'   integer. For importance resampling, some resamples may use one set of weights
#'   and others use a different set of weights. In this case \code{R} would be a vector of
#'   integers where each component gives the number of resamples from each of the rows
#'   of weights.
#' @param sim A character string indicating the type of simulation required. Possible
#'   values are \code{"ordinary"} (the default), \code{"parametric"}, \code{"balanced"},
#'   \code{"permutation"}, or \code{"antithetic"}. Importance resampling is specified by
#'   including importance weights; the type of importance resampling must still be
#'   specified but may only be \code{"ordinary"} or \code{"balanced"} in this case.
#' @param stype A character string indicating what the second argument of \code{statistic}
#'   represents. Possible values of \code{stype} are \code{"i"} (indices - the default),
#'   \code{"f"} (frequencies), or \code{"w"} (weights). Not used for \code{sim = "parametric"}.
#' @param strata An integer vector or factor specifying the strata for multi-sample
#'   problems. This may be specified for any simulation, but is ignored when
#'   \code{sim = "parametric"}. When \code{strata} is supplied for a nonparametric bootstrap,
#'   the simulations are done within the specified strata.
#' @param L Vector of influence values evaluated at the observations. This is used only
#'   when \code{sim} is \code{"antithetic"}. If not supplied, they are calculated through a call to
#'   \code{empinf}. This will use the infinitesimal jackknife provided that \code{stype} is
#'   \code{"w"}, otherwise the usual jackknife is used.
#' @param m The number of predictions which are to be made at each bootstrap replicate.
#'   This is most useful for (generalized) linear models. This can only be used when
#'   \code{sim} is \code{"ordinary"}. \code{m} will usually be a single integer but,
#'   if there are strata, it may be a vector with length equal to the number of strata,
#'   specifying how many of the errors for prediction should come from each strata.
#'   The actual predictions should be returned as the final part of the output of
#'   \code{statistic}, which should also take an argument giving the vector of indices
#'   of the errors to be used for the predictions.
#' @param weights Vector or matrix of importance weights. If a vector then it should
#'   have as many elements as there are observations in data. When simulation from more
#'   than one set of weights is required, \code{weights} should be a matrix where each
#'   row of the matrix is one set of importance weights. If \code{weights} is a matrix then
#'   \code{R} must be a vector of length \code{nrow(weights)}. This parameter is ignored
#'   if \code{sim} is not \code{"ordinary"} or \code{"balanced"}.
#' @param ran.gen This function is used only when \code{sim = "parametric"} when it
#'   describes how random values are to be generated. It should be a function of two
#'   arguments. The first argument should be the observed data and the second argument
#'   consists of any other information needed (e.g. parameter estimates). The second
#'   argument may be a list, allowing any number of items to be passed to \code{ran.gen}.
#'   The returned value should be a simulated data set of the same form as the
#'   observed data which will be passed to \code{statistic} to get a bootstrap replicate.
#'   It is important that the returned value be of the same shape and type as the
#'   original dataset. If \code{ran.gen} is not specified, the default is a function
#'   which returns the original data in which case all simulation should be included
#'   as part of statistic. Use of \code{sim = "parametric"} with a suitable \code{ran.gen}
#'   allows the user to implement any types of nonparametric resampling which are not
#'   supported directly.
#' @param mle The second argument to be passed to \code{ran.gen}. Typically these will be
#'   maximum likelihood estimates of the parameters. For efficiency \code{mle} is often a
#'   list containing all of the objects needed by \code{ran.gen} which can be calculated
#'   using the original data set only.
#' @param simple logical, only allowed to be  \code{TRUE} for  \code{sim = "ordinary"},
#'    \code{stype = "i"},  \code{n = 0} (otherwise ignored with a warning). By default
#'    a  \code{n} by  \code{R} index array is created: this can be large and if
#'     \code{simple = TRUE} this is avoided by sampling separately for each replication,
#'     which is slower but uses less memory.
#' @param ... Other named arguments for \code{statistic} which are passed unchanged each
#'   time it is called. Any such arguments to \code{statistic} should follow the arguments
#'   which \code{statistic} is required to have for the simulation. Beware of partial
#'   matching to arguments of \code{boot} listed above, and that arguments named
#'   \code{X} and \code{FUN} cause conflicts in some versions of boot (but not this one).
#' @param parallel The type of parallel operation to be used (if any). If missing, the
#'   default is taken from the option \code{"boot.parallel"} (and if that is not set, \code{"no"}).
#' @param ncpus integer: number of processes to be used in parallel operation: typically one
#'   would chose this to the number of available CPUs.
#' @param cl An optional parallel or snow cluster for use if \code{parallel = "snow"}. If
#'   not supplied, a cluster on the local machine is created for the duration of the boot call.
#'
#' The statistic to be bootstrapped can be as simple or complicated as desired as long as
#' its arguments correspond to the dataset and (for a nonparametric bootstrap) a vector of
#' indices, frequencies or weights. \code{statistic} is treated as a black box by the boot function
#' and is not checked to ensure that these conditions are met.
#'
#' The first order balanced bootstrap is described in Davison, Hinkley and Schechtman
#' (1986). The antithetic bootstrap is described by Hall (1989) and is experimental,
#' particularly when used with strata. The other non-parametric simulation types
#' are the ordinary bootstrap (possibly with unequal probabilities), and permutation
#' which returns random permutations of cases. All of these methods work independently
#' within strata if that argument is supplied.
#'
#' For the parametric bootstrap it is necessary for the user to specify how the
#' resampling is to be conducted. The best way of accomplishing this is to specify
#' the function \code{ran.gen} which will return a simulated data set from the observed
#' data set and a set of parameter estimates specified in \code{mle}.
#'
#' @return t0	The observed value of \code{statistic} applied to data.
#' @return t A matrix with \code{sum(R)} rows each of which is a bootstrap replicate of
#'   the result of calling \code{statistic}.
#' @return R The value of \code{R} as passed to \code{boot}.
#' @return data The \code{data} as passed to \code{boot}.
#' @return seed The value of \code{.Random.seed} when \code{boot} started work.
#' @return statistic The function \code{statistic} as passed to \code{boot}.
#' @return sim Simulation type used.
#' @return stype Statistic type as passed to \code{boot}.
#' @return call The original call to \code{boot}.
#' @return strata The strata used. This is the vector passed to
#'   \code{boot}, if it was supplied or a vector of ones if there were no strata.
#'   It is not returned if \code{sim} is \code{"parametric"}.
#' @return weights The importance sampling weights as passed to boot or the
#'   empirical distribution function weights if no importance sampling weights
#'   were specified. It is omitted if \code{sim} is not one of \code{"ordinary"}
#'   or \code{"balanced"}.
#' @return pred.i If predictions are required \code{(m > 0)} this is the matrix of indices
#'   at which predictions were calculated as they were passed to statistic.
#'   Omitted if \code{m} is \code{0} or \code{sim} is not \code{"ordinary"}.
#' @return L The influence values used when \code{sim} is \code{"antithetic"}. If no such values
#'   were specified and \code{stype} is not \code{"w"} then \code{L} is returned as
#'   consecutive integers corresponding to the assumption that data is ordered by
#'   influence values. This component is omitted when \code{sim} is not \code{"antithetic"}.
#' @return ran.gen The random generator function used if \code{sim} is \code{"parametric"}. This
#'   component is omitted for any other value of \code{sim}.
#' @return mle The parameter estimates passed to \code{boot} when \code{sim} is
#'   \code{"parametric"}. It is omitted for all other values of \code{sim}.
#'
#' @examples
#'
#'  m.boot <- function(data, indices) {
#'   d <- data[indices]
#'   mean(d, na.rm = TRUE)
#'   }
#'
#'  tot_rep <- 200
#'  v1 <- rnorm(1000)
#'  b <- pbboot(v1, m.boot, R = tot_rep)
#' @export

pbboot <- function (data, statistic, R, sim = "ordinary",
                    stype = c("i", "f", "w"), strata = rep(1, n), L = NULL, m = 0, weights = NULL,
                    ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ...,
                    parallel = c("no", "multicore", "snow"), ncpus = getOption("boot.ncpus",
                                                                               1L), cl = NULL)
{
  call <- match.call()
  stype <- match.arg(stype)
  if (missing(parallel))
    parallel <- getOption("boot.parallel", "no")
  parallel <- match.arg(parallel)
  have_mc <- have_snow <- FALSE
  if (parallel != "no" && ncpus > 1L) {
    if (parallel == "multicore")
      have_mc <- .Platform$OS.type != "windows"
    else if (parallel == "snow")
      have_snow <- TRUE
    if (!have_mc && !have_snow)
      ncpus <- 1L
    loadNamespace("parallel")
  }
  if (simple && (sim != "ordinary" || stype != "i" || sum(m))) {
    warning("'simple=TRUE' is only valid for 'sim=\"ordinary\", stype=\"i\", n=0', so ignored")
    simple <- FALSE
  }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- NROW(data)
  if ((n == 0) || is.null(n))
    stop("no data in call to 'boot'")
  temp.str <- strata
  strata <- tapply(seq_len(n), as.numeric(strata))
  t0 <- if (sim != "parametric") {
    if ((sim == "antithetic") && is.null(L))
      L <- empinf(data = data, statistic = statistic, stype = stype,
                  strata = strata, ...)
    if (sim != "ordinary")
      m <- 0
    else if (any(m < 0))
      stop("negative value of 'm' supplied")
    if ((length(m) != 1L) && (length(m) != length(table(strata))))
      stop("length of 'm' incompatible with 'strata'")
    if ((sim == "ordinary") || (sim == "balanced")) {
      if (isMatrix(weights) && (nrow(weights) != length(R)))
        stop("dimensions of 'R' and 'weights' do not match")
    }
    else weights <- NULL
    if (!is.null(weights))
      weights <- t(apply(matrix(weights, n, length(R),
                                byrow = TRUE), 2L, normalize, strata))
    if (!simple)
      i <- index.array(n, R, sim, strata, m, L, weights)
    original <- if (stype == "f")
      rep(1, n)
    else if (stype == "w") {
      ns <- tabulate(strata)[strata]
      1/ns
    }
    else seq_len(n)
    t0 <- if (sum(m) > 0L)
      statistic(data, original, rep(1, sum(m)), ...)
    else statistic(data, original, ...)
    rm(original)
    t0
  }
  else statistic(data, ...)
  pred.i <- NULL
  fn <- if (sim == "parametric") {
    ran.gen
    data
    mle
    function(r) {
      dd <- ran.gen(data, mle)
      statistic(dd, ...)
    }
  }
  else {
    if (!simple && ncol(i) > n) {
      pred.i <- as.matrix(i[, (n + 1L):ncol(i)])
      i <- i[, seq_len(n)]
    }
    if (stype %in% c("f", "w")) {
      f <- freq.array(i)
      rm(i)
      if (stype == "w")
        f <- f/ns
      if (sum(m) == 0L)
        function(r) statistic(data, f[r, ], ...)
      else function(r) statistic(data, f[r, ], pred.i[r, ], ...)
    }
    else if (sum(m) > 0L)
      function(r) statistic(data, i[r, ], pred.i[r, ], ...)
    else if (simple)
      function(r) statistic(data, index.array(n, 1, sim,
                                              strata, m, L, weights), ...)
    else function(r) statistic(data, i[r, ], ...)
  }
  RR <- sum(R)
  res <- if (ncpus > 1L && (have_mc || have_snow)) {
    if (have_mc) {
      pbapply::pblapply(seq_len(RR), fn, cl = ncpus)
    }
    else if (have_snow) {
      list(...)
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost",
                                             ncpus))
        if (RNGkind()[1L] == "L'Ecuyer-CMRG")
          parallel::clusterSetRNGStream(cl)
        res <- pbapply::pblapply(cl=cl, seq_len(RR), fn) ##change here
        parallel::stopCluster(cl)
        res
      }
      else pbapply::pblapply(cl=cl, seq_len(RR), fn)
    }
  }
  else pbapply::pblapply(seq_len(RR), fn) #### changed !!!
  t.star <- matrix(, RR, length(t0))
  for (r in seq_len(RR)) t.star[r, ] <- res[[r]]
  if (is.null(weights))
    weights <- 1/tabulate(strata)[strata]
  boot.return(sim, t0, t.star, temp.str, R, data, statistic,
              stype, call, seed, L, m, pred.i, weights, ran.gen, mle)
}
## Functions not exported by boot
isMatrix <- boot:::isMatrix
index.array <- boot:::index.array
boot.return <- boot:::boot.return
