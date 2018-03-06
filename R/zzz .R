#' .onLoad
#'
#' \code{.onLoad} sets user configurable parameters via R global options. R will execute this function, passing the correct arguments, when this package is loaded.
#'
#' @param libname
#' @param pkgname
#'
#' @return
#'
.onLoad <- function(libname, pkgname) {

  ## User configurable options
  #
  # cf. Wickham, H. [_R packages_, "R Code"][http://r-pkgs.had.co.nz/r.html]

  library(jsonlite)

  # Get preload decisions from config file
  config <- read_json("./data-raw/config.json", simplifyVector = TRUE)

  # Default options use study tables (`study`) from package data, and build
  #  parameters (`pars`) from package internal data.

  # Make default color lists have the names of the codes they color.
  names(config$facil_cols) <- wkFocus::study$facilitation$code
  names(config$focus_cols) <- wkFocus::study$focus$code

  # Default list will be used to set user-configurable global options
  #  Use "." as semantic delim as Wickham did. something I don't know?
  defaults <- list(
    wkf.ses.start = pars$workshop_start,
    wkf.pl.duration = as.difftime(config$pl_duration, format = "%T"),
    wkf.focus.cols = config$focus_cols,
    wkf.facil.cols = config$facil_cols
  )

  op <- options()

  toset <- !(names(defaults) %in% names(op))   # only those not already defined
  if(any(toset)) options(defaults[toset])

  invisible()
}
