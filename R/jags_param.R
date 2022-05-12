#' Distribution parametrization in `JAGS`
#'
#' `jags_param()` prints the parametrization of different distributions according to `JAGS`.
#'
#' @param dist Character string indicating the distribution.
#' @section Details:
#' \describe{Arguments passed for distribution density may vary from R parametrization, so this function prints
#' the arguments for a wide range of them.
#' }
#' \describe{Distribution that can be specified as the function argument are, "binom", "gamma", "exp", "chisq", "f", "logistic", "lognormal",
#' "norm", "t", "unif", "bern", "categorical", "pois", "Laplace", "negbin".
#' }
#' @export
#' @examples
#' jags_param("f")
jags_param <- function(dist) {
  if (dist == "binom") {
    linea1 <- cat("y[i] ~ dbinom(p, n)",
                  "y[i] ~ dbin(p, n)", sep = "\n")
  }

  if (dist == "gamma") {
    linea1 <- cat("y[i] ~ dgamma(r, lambda)", sep = "\n")
  }

  if (dist == "exp") {
    linea1 <- cat("y[i] ~ dexp(lambda)", sep = "\n")
  }

  if (dist == "chisq") {
    linea1 <- cat("y[i] ~ dchisqr(k)",
                  "y[i] ~ dgamma(1/2, 1/k)", sep = "\n")
  }

  if (dist == "f") {
    linea1 <- cat("y[i] ~ df(n, m)",
                  "y <- (x1/sqrt(n))/(x2/sqrt(m))",
                  "  x1 ~ dchisqr(n)",
                  "  x2 ~ dchisqr(m)", sep = "\n")
  }

  if (dist == "logistic") {
    linea1 <- cat("y[i] ~ dlogis(mu, tau)", sep = "\n")
  }

  if (dist == "lognormal") {
    linea1 <- cat("y[i] ~ dlnorm(mu, tau)", sep = "\n")
  }

  if (dist == "norm") {
    linea1 <- cat("y[i] ~ dnorm(mu, tau)", sep = "\n")
  }

  if (dist == "t") {
    linea1 <- cat("y[i] ~ dt(mu, tau, k)",
                  "y <- x/sqrt(s)",
                  "  x ~ dnorm(mu, tau)",
                  "  s ~ dgamma(k/2, k/2)", sep = "\n")
  }

  if (dist == "unif") {
    linea1 <- cat("y[i] ~ dunif(a, b)", sep = "\n")
  }

  if (dist == "bern") {
    linea1 <- cat("y[i] ~ dbern(p)", sep = "\n")
  }

  if (dist == "categorical") {
    linea1 <- cat("y[i] ~ dcat(pi)", sep = "\n")
  }

  if (dist == "pois") {
    linea1 <- cat("y[i] ~ dpois(lambda)", sep = "\n")
  }

  if (dist == "Laplace") {
    linea1 <- cat("y[i] ~ ddexp(mu, tau)", sep = "\n")
  }

  if (dist == "negbin") {
    linea1 <- cat("y[i] ~ dnegbin(p, r)", sep = "\n")
  }

  cat(linea1, sep = "\n")
}



















