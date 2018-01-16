#' Directional Derivative Exact
#'
#' dd_exact computes the exact directional derivative of the multivariate
#' function f, at the point a, in the direction b.
#'
#' @param gradf handle to function that returns the gradient of the function f
#' @param a point at which the directional derivative is evaluated
#' @param b the direction vector
#' @export
dd_exact <- function(gradf, a, b) {
  dfa = jacobian(gradf, a)
  u = b/sqrt(sum(b^2))
  dd_exact = dfa%*%b
  return(dd_exact)
}

