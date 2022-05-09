#' Coded examples of JAGS models and R runs.
#'
#' `jags_examples()` gives an example of code in `JAGS` for model specification, and code that could be passed to `run.jags` function.
#'
#' @param example Character string. Prints example models using `JAGS` specific code and parametrization. Options are `"model1"` or `"model2`.
#' @param runme Logical. If `TRUE` the R code to run the `JAGS` model will also be displayed. The default value is `runme = TRUE`.
#' @section Models:
#' `example`
#' \describe{
#'   \item{model 1}{logistic regression for a Bernoulli random variable. Linearity on eta, with a and b normal prior distributions.}
#'   \item{model 2}{logarithmic regression for a Poisson random variable.}
#' }
#' `runme`
#' \describe{specification of initial values and distributions for the `JAGS` model, as well as `run.jags` example code.
#' }
#' @seealso \code{\link[runjags]{run.jags}}
#' @export
#' @examples
#' \dontrun{
#' j.ex <- jags_examples("model1")
#' }
#'
jags_examples <- function(example, runme = T) {
  if (example == "model1") {
    linea1 <- cat(c("model1 <- \"model {",
          "  for (i in 1:N) {",
          "    eta[i] <- a + b * x[i]",
          "    logit(prob[i]) <- eta[i]",
          "    y[i] ~ dbern(prob[i])",
          "  }",
          "  a ~ dnorm(0, 0.01)",
          "  b ~ dnorm(0, 0.01)",
          "}\""), sep = "\n")
  }

  if (example == "model2") {
    linea1 <- cat(c("model2 <- \"model {",
                    "  for (i in 1:N) {",
                    "    log(eta0[i]) <- 0 + mu[i]",
                    "    log(eta1[i]) <- a + b * x[i] + mu[i]",
                    "    prob[i] <- 1/(1 + exp(-(a + b * x[i])))",
                    "    y0[i] ~ dpois(eta0[i])",
                    "    y1[i] ~ dpois(eta1[i])",
                    "    mu[i] ~ dnorm(0, 0.000001)",
                    "  }",
                    "  a ~ dnorm(0, 0.01)",
                    "  b ~ dnorm(0, 0.01)",
                    "}\""), sep = "\n")
  }



    if (example == "model1" & runme == T) {
      linea2 <- cat(c(
        "n <- 100",
        "x <- rnorm(n)",
        "eta <-  1 + x",
        "prob <- 1/(1+exp(-eta))",
        "y <- rbinom(n, size = 1, prob = prob)",
        "library(runjags)",
        "r <- combine.mcmc(run.jags(model = model1,",
        "                           data = list(y = y, x = x, N = n),",
        "                           monitor = c(\"prob[1]\", \"a\",  \"b\"),",
        "                           n.chains = 4,",
        "                           sample = 10^5,",
        "                           method = \"parallel\"))",
        "summary(r)") , sep = "\n")
    }

     if (example == "model2" & runme == T) {
       linea2 <- cat(c(
        "y0 <- 1 - y",
        "y1 <- y",
        "rr <- combine.mcmc(run.jags(model = model2,",
        "                           data = list(y0 = y0, y1 = y1, x = x, N = n),",
        "                           monitor = c(\"prob[1]\", \"a\",  \"b\"),",
        "                           n.chains = 4,",
        "                           sample = 10^5,",
        "                           method = \"parallel\"))",
        "summary(rr)"), sep = "\n")
     }
     if (runme == F) {
     linea2 <- cat("", sep = "\n")
  }

    cat(linea1, linea2, sep = "\n")
}

