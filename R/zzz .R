.onLoad <- function(libname, pkgname) {

  ## User configurable options
  #
  # cf. Wickham, H. [_R packages_, "R Code"][http://r-pkgs.had.co.nz/r.html]

  # Default list will be used to set user-configurable global options
  #  Use "." as semantic delim as Wickham did. something I don't know?
  defaults <- list(
    wkf.ses.start = pars$workshop_start,
    wkf.facil.cols = pars$defaults$facil_cols,
    wkf.focus.cols = pars$defaults$focus_cols,
    wkf.pl.duration = pars$defaults$pl_duration
  )

  op <- options()

  toset <- !(names(defaults) %in% names(op))   # only those not already defined
  if(any(toset)) options(defaults[toset])

  invisible()
}
