#' Simulate normally distributed dataset
#'
#' Returns a normally distributed dataset with a
#' correlation matrix that is given by the function
#' corr_matrix and the non-zero coefficients as given by
#' the function beta_type
#'
#' @param n number of observations
#' @param p number of parameters
#' @param s number of non-zero coefficients
#' @param corr_matrix function that returns
#'               the correlation matrix
#' @param beta_type function that returns the
#'               beta coefficient vector
#' @param snr signal-to-noise ratio
#'
#' @return A list with X, y
#' @export
simsham <- function(n = 10, p = 20, s = 5,
                    corrmat = corrmat_identity(p),
                    beta = beta_first_s_covariates(p, s),
                    snr = 1) {

  # Generate start data (i.e. designmatrix without a
  # given correlation structure)
  X <- matrix(rnorm(n * p), n, p)

  # add correlation
  obj <- svd(corrmat)
  corrmat_half <- obj$u %*% (sqrt(diag(obj$d))) %*% t(obj$v)
  X <- X %*% corrmat_half

  # Generate response
  vmu <- as.numeric(t(beta) %*% corrmat %*% beta)
  noise <- sqrt(vmu/snr)
  y <- as.numeric(X %*% beta + rnorm(n) * noise)

  list(
    X = X,
    y = y
  )
}
