#' A function that estimats B OLS in matrix form
#'
#' @param y A numerical vector of size (nx1).
#' @param X A numerical matrix of size (nxk).
#'
#' @return A numerical vector of size (kx1).
#' @export

estimate_beta <- function(y, X) { 
  beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
  return(beta_hat)
}