#' Website links for JAGS installation.
#'
#' `jags_init()` returns the general website links for the correct JAGS installation, as well as the main library to run JAGS in R.
#' Briefly the steps are installing Homebrew, and then using it for JAGS installation or downloding the .pkg file from JAGS source. Installation steps may vary depending on whether Mac OS X, Linux or Windows is used.
#' Note that other packages can be used instead of, or complimentary to, `runjags`.
#'
#' @param install Choose between installing JAGS using Homebrew or manually from source.
#' @family website url
#' @seealso \url{https://brew.sh/index_es} for Homebrew installation, \url{https://formulae.brew.sh/formula/jags} for JAGS installation using Homebrew,  \url{https://sourceforge.net/projects/mcmc-jags/} for JAGS installation from source, \url{https://cran.r-project.org/web/packages/runjags/index.html} runjags package documentation
#' @export
#' @examples
#' \dontrun{
#' jags_init(install = "manual")
#' }
#'
jags_init <- function(install) {
  if (install == "homebrew") {
    linea1 <- cat("https://brew.sh/index_es",
                  "https://formulae.brew.sh/formula/jags",
                  "install.packages(\"runjags\")", sep = "\n")
  }
  if (install == "manual") {
    linea1 <- cat("https://sourceforge.net/projects/mcmc-jags/",
                  "Files -> JAGS -> 4.x (or latest) -> MAC OS/Windows/Source -> JAGS-4.3.1.pkg (or latest)",
                  "install.packages(\"runjags\")", sep = "\n")
  }
  cat(linea1, sep = "\n")
}





