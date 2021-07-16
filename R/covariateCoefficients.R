###################################
# covariateCoefficients.R
#
# Contains several functions that
# can generate a vector of covariate
# coefficients
#
# You can create your own by adding
# a function. It can be passed on to the
# simulate function
###################################

#' First covariates have an effect
#'
#' Returns a function that itself return a
#' coefficient vector where on the first
#' s coefficients are non-zero. The values
#' they take are given by the parameter values
#'
#' @param p length of the vector
#' @param s the number of vectors that are non-zero
#' @param values vector of length s that contains the
#'               values of the coefficients
#'
#' @return function that returns a vector of covariates
#'         where only the first s coefficients are non-zero
#' @export
beta_first_s_covariates <- function(p, s, values = rep(1, s), ...) {
  if (p < s) {
    stop("p should be larger or equal to s")
  }

  if (length(values) != s) {
    stop("number of values should match s")
  }

  c(values, rep(0, p - s))
}


#' Non-zero coefficients are spread out 'equally'
#'
#' Returns a function that itself return a
#' coefficient vector where non-zero coefficients
#' are spread out 'equally'. The values
#' they take are given by the parameter values
#'
#' @param p length of the vector
#' @param s the number of vectors that are non-zero
#' @param indices the indices of the coefficients that are non-zero
#'                (Default: equally spread out)
#' @param values vector of length s that contains the
#'               values of the coefficients
#'
#' @return function that returns a vector of covariates
#'         where only the first s coefficients are non-zero
#' @export
beta_spread_covariates <- function(p, s,
                                   indices = floor(seq(1, p, length.out = s)),
                                   values = rep(1,length(indices)), ...) {

  if (p <= s) {
    stop("p should be larger to s")
  }

  if (length(values) != length(indices)) {
    stop("number of values should the number of indices")
  }

  beta <- rep(0, p)
  beta[indices] <- values

  beta
}
