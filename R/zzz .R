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

  # Make colors named vectors
  focus_codes = c("Rd", "Do", "Rp", "Ds", "O", "Of")
  focus_cols  = c("#8c510a","#d8b365","#FF7F00", "#CD1076","#5ab4ac","#01665e")
  names(focus_cols) <- focus_codes

  facil_codes = c("Ind", "Fac")
  facil_cols = c('#bdbdbd', '#636363')  # BW
  # facil_cols = c("#f1a340", "olivedrab3")
  names(facil_cols) <- facil_codes

  ## Add wkf options to the global options list
  op <- options()
  op.wkf <- list(
    wkf.code.types = c("facilitation", "focus"),
    wkf.focus.codes = focus_codes,
    wkf.focus.cols = focus_cols,
    wkf.facil.codes = facil_codes,
    wkf.facil.cols = facil_cols,
    wkf.time.workshop = paste("2016-08-16", "00:00:00 UTC"),  # conventional origin
    wkf.pltime.min = paste("2016-08-16", "00:00:00 UTC"),     # def start of plot
    wkf.pltime.max = paste("2016-08-16", "00:20:00 UTC"),     # def end of plot
    wkf.fr = 25  # frame rate the video was coded at
  )

  toset <- !(names(op.wkf) %in% names(op))   # only those not already defined
  if(any(toset)) options(op.wkf[toset])

  invisible()
}
