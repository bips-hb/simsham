###################################
# correlationMatrices.R
#
# Contains several functions that
# can generate different correlation matrices
#
# You can create your own by adding
# a function. It can be passed on to the
# simulate function
###################################

#' Identity Correlation Matrix
#'
#' Returns a function that itself returns a p x p
#' identity matrix
#'
#' @param p matrix size
#'
#' @return function that returns an identity matrix
#' @export
corrmat_identity <- function(p, ...) {
  diag(p)
}

#' Toeplitz Correlation Matrix
#'
#' Returns a function that itself returns a p x p
#' Toeplitz matrix
#'
#' @param p matrix size
#' @param rho controls the correlation strength
#'
#' @return function that returns a Toeplitz matrix
#' @export
#' @export
corrmat_toeplitz <- function(p, rho, ...) {
  if (rho == 0) {
    corrmat_identity(p)
  } else {
    rho^abs(outer(1:p, 1:p, "-"))
  }
}

#' Block Correlation Matrix
#'
#' Returns a function that itself returns a p x p
#' matrix with blocksize blocks.
#' Note that the blocksize should fit the matrix,
#' i.e., mod(p, blocksize) should be 0
#'
#' @param p matrix size
#' @param rho controls the correlation strength
#' @param blocksize the size of the blocks.
#'
#' @return function that returns a block matrix
#' @export
#' @export
corrmat_block <- function(p, rho, blocksize, ...) {
  if (p %% blocksize != 0) {
    stop("mod(p, blocksize) should be zero")
  }

  if (blocksize == 1) {
    corrmat_identity(p)
  }

  # number of blocks in the correlation matrix
  n_blocks = p / blocksize

  # create block matrix
  block <- matrix(rho, nrow = blocksize, ncol = blocksize)

  corrmat <- as.matrix(
    bdiag(rep(list(block), n_blocks))
  )

  diag(corrmat) <- 1

  corrmat
}
