#' Examples of data transformation uses within the `JAGS` code.
#'
#' `jags_data()` prints examples of what can be passed in the data block within the `JAGS` code. How the data is
#' generated can also be specified for  the particular following model, by creating a `data`
#' string before model specification.
#'
#' @param block Character string indicating a data generation method or transformation applied to input data.
#' @section Details:
#' \describe{By specifying `data {...}` before `model {...}`, the data passed onto the `JAGS` model will be transformed or
#' generated accordingly. There are much more options than the ones shown here, this should be used just as an example.
#' }
#' \describe{
#'   \item{Transformation}{Available transformations are "sqrt", "log", "exp", and "sqr". Also "dim" can be used as
#'   an argument. The latter is useful so that the dimension of the response vector doesn't need to be previously
#'   specified.}
#'   \item{Simulated data}{Using "prior" generates true values for the parameters mu and tau from the given prior
#'   specified in the data block. This means the mu and tau are are generated and then passed to the model. Using
#'   "fix" will print a code where mu and tau are the same for both the data and model block. This means the parameters will
#'   be taken as observed values in the model.}
#' }
#' @export
#' @examples
#' jags_data(block = "sqrt")
#' jags_data(block = "fix")
jags_data <- function(block) {

  if (block == "sqrt") {
    linea1 <- cat("data {",
                  "  for (i in 1:N) {",
                  "    y[i] <- sqrt(x[i])",
                  "  }",
                  "}", sep = "\n")
  }

  if (block == "log") {
    linea1 <- cat("data {",
                  "  for (i in 1:N) {",
                  "    y[i] <- log(x[i])",
                  "  }",
                  "}", sep = "\n")
  }

  if (block == "exp") {
    linea1 <- cat("data {",
                  "  for (i in 1:N) {",
                  "    y[i] <- exp(x[i])",
                  "  }",
                  "}", sep = "\n")
  }

  if (block == "sqr") {
    linea1 <- cat("data {",
                  "  for (i in 1:N) {",
                  "    y[i] <- (x[i])^2",
                  "  }",
                  "}", sep = "\n")
  }

  if (block == "generic") {
    linea1 <- cat(c("data {",
                    "  Y <- dim(X)",
                    "}",
                    "model {",
                    "  for (i in 1:Y[1]) {",
                    "    for (j in 1:Y[2]) {",
                    "      Y[i,j] <- dnorm(alpha[i] + beta[j], tau)",
                    "    }",
                    "  }",
                    "  ...",
                    "}"), sep = "\n")
  }

  if (block == "prior") {
    linea1 <- cat(c("data {",
                    "  for (i in 1:N) {",
                    "    y[i] ~ dnorm(mu.true, tau.true)",
                    "  }",
                    "  mu.true ~ dnorm(0,1);",
                    "  tau.true ~ dgamma(1,3);",
                    "}",
                    "model {",
                    "  for (i in 1:N) {",
                    "    y[i] ~ dnorm(mu, tau)",
                    "  }",
                    "  mu ~ dnorm(0, 1.0E-3)",
                    "  tau ~ dgamma(1.0E-3, 1.0E-3)",
                    "}"), sep = "\n")
  }

  if (block == "fix") {
    linea1 <- cat(c("data {",
                    "  for (i in 1:N) {",
                    "    y[i] ~ dnorm(mu, tau)",
                    "  }",
                    "  mu ~ dnorm(0,1);",
                    "  tau ~ dgamma(1,3);",
                    "}",
                    "model {",
                    "  for (i in 1:N) {",
                    "    y[i] ~ dnorm(mu, tau)",
                    "  }",
                    "  mu ~ dnorm(0, 1.0E-3)",
                    "  tau ~ dgamma(1.0E-3, 1.0E-3)",
                    "}"), sep = "\n")
  }

  cat(linea1, sep = "\n")
}


