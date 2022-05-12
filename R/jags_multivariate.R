#' Parametrization in `JAGS` for multivariate distribution
#'
#' `jags_multivariate()` prints the parametrization of the main multivariate distributions built in `JAGS`.
#'
#' @param dist Character string indicating the distribution. Argument is "norm" or "multinomial".
#' @seealso \code{\link{jags_param}} for univariate distributions.
#' @export
#' @examples
#' jags_multivariate("multinomial")
jags_multivariate <- function(dist) {

  if (dist == "norm") {
    linea1 <- cat("y[1:N] ~ dmnorm(mu[1:N], Omega[1:N, 1:N])",
                  "y[1:N] ~ dmnorm.vcov(mu[1:N], Sigma[1:N, 1:N])", sep = "\n")
  }

  if (dist == "multinomial") {
    linea1 <- cat("y[1:N] ~ dmulti(pi[1:N], k)",
                  "y[1:N] ~ dsample(pi[1:N], k)", sep = "\n")
  }
  cat(linea1, sep = "\n")
}
