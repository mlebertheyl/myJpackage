#' General code for `run.jags` function.
#'
#' `jags_runjags()` prints the general arguments passed onto the `run.jags` function, and the corresponding class
#' to which it belongs.
#' @param NULL
#' @export
#' @examples
#' \dontrun{
#' jags_runjags()
#' }
#'
jags_runjags <- function() {
  linea1 <- cat("combine.mcmc(run.jags(",
                "  model = ...",
                "  data = list(...)",
                "  monitor = c(\"...\")",
                "  sample = numeric",
                "  burnin = numeric",
                "  n.chains = number of computer cores",
                "  method = \"parallel\"",
                "))", sep = "\n")
  cat(linea1, sep = "\n")
}



