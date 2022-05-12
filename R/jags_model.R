#' Returns JAGS code for an specific model
#'
#' `jags_model()` prints the code needed for model specification when using `JAGS`. The models and distribution options are quite general,
#' so the user can grasp the structure of the code. However, this models are widely used without added complexity. Note that `JAGS` accepts a
#' wide range of distributions for the likelihood parameter.
#'
#' @param likelihood Likelihood function. It accepts "dnorm", "dbinom", "dpois", and "dbern".
#' @param regression A function accepting "linear", "logit", and "log" as part of the likelihood specification.
#' @param param Logical with `FALSE` as default option. If `TRUE`, the prior distribution is previously specified as a character string using the same
#' parameters as in the `regression` argument. If `FALSE`, the prior distribution is assumed to be normal.
#' @param reg Logical with `FALSE` as default option. It only accepts a `TRUE` value if `regression = "linear"`, and prints the formula for the mean and variance.
#' @section Details:
#' \describe{There is a lot more flexibility for `JAGS` models than the ones specified. However, this is a general
#' representation of the code one would use. Some other distribution examples can be found in `jags_param`.
#' }
#' \describe{Note that it only makes sense to use a linear regression with a normal likelihood, since the othe distributions
#' are discrete distributions.
#' }
#' @seealso \code{\link{jags_param}}
#' @export
#' @examples
#' jags_model("dnorm", "linear", param = FALSE, reg = TRUE)
jags_model <- function(likelihood, regression, param = F, reg = F) {
  linea1 <- cat(c("model {",
                  "  for(i in 1:N) {"), sep = "\n")

  if (likelihood == "dnorm") {
    linea2 <- cat("   y[i] ~ dnorm(prob[i], tau)", sep = "\n")
  }

  if (likelihood == "dbinom") {
    linea2 <- cat("   y[i] ~ dbinom(prob[i], 1)", sep = "\n")
  }

  if (likelihood == "dpois") {
    linea2 <- cat("   y[i] ~ dpois(prob[i])", sep = "\n")
  }

  if (likelihood == "dbern") {
    linea2 <- cat("   y[i] ~ dbern(prob[i])", sep = "\n")
  }

  if (regression == "linear") {
    linea3 <- cat("   prob[i] <- beta0 + beta1*(x[i] - x.bar)",
                  "  }", sep = "\n")
  }

  if (regression == "logit") {
    linea3 <- cat("   logit(prob[i]) <- beta0 + beta1*x[i]",
                  "  }", sep = "\n")
  }

  if (regression == "log") {
    linea3 <- cat("    log(prob[i]) <- beta0 + beta1*x[i]",
                  "  }", sep = "\n")
  }

  if (regression == "linear" & param == T) {
    linea4 <- cat(convert.vecteur(beta0), convert.vecteur(beta1), convert.vecteur(tau), sep = "\n")
  }

  if (regression == "logit" & param == T) {
    linea4 <- cat(convert.vecteur(beta0), convert.vecteur(beta1), sep = "\n")
  }

  if (regression == "log" & param == T) {
    linea4 <- cat(convert.vecteur(beta0), convert.vecteur(beta1), sep = "\n")
  }

  if (regression == "linear" & param == F) {
    beta0 <- "dnorm(0, 0.01)"
    beta1 <- "dnorm(0, 0.01)"
    tau <- "dnorm(0, 0.01)"
    linea4 <- cat(convert.vecteur(beta0), convert.vecteur(beta1), convert.vecteur(tau), sep = "\n")
  }

  if (regression == "logit" & param == F) {
    beta0 <- "dnorm(0, 0.01)"
    beta1 <- "dnorm(0, 0.01)"
    linea4 <- cat(convert.vecteur(beta0), convert.vecteur(beta1), sep = "\n")
  }

  if (regression == "log" & param == F) {
    beta0 <- "dnorm(0, 0.01)"
    beta1 <- "dnorm(0, 0.01)"
    linea4 <- cat(convert.vecteur(beta0), convert.vecteur(beta1), sep = "\n")
  }

  if (regression == "linear" & reg == T) {
    linea5 <- cat(c("x.bar <- mean(x)",
                    "sigma <- 1/sqrt(tau)"), sep = "\n")
  }

  if (reg == F) {
    linea5 <- cat("")
  }

  lineaf <- cat("}", sep = "\n")
  cat(linea5, linea4, linea3, linea2, linea1, lineaf, sep = "\n")
}

