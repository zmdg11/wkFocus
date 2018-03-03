#-----------------------------------------------------

#' wkf_config
#'
#' Provides access to the package configuration data establishing global
#' parameters such as plotting scales, color codes for factor levels, etc.
#'
#' @return A list of named parameters configuring this project
#' @export
#'
#' @examples
#' pars <- wkf_config()

wkf_config <- function() {
  return(wkFocus:::pars)  # `pars` is in package environment via `sysdata.Rda`
}

#-----------------------------------------------------

#' wkf_convert_tcode
#'
#' Converts timecodes into timestamps (absolute time within a session).
#'
#' @param tcode String. A video timecode of form "hh:mm:ss:ff".
#' @param fr Numeric. The video frame rate.
#' @param origin String. A date-time giving a starting point for the timestamps. Of the format "yyyy-mm-dd hh:mm:ss <timezone>".
#'
#' @return Dataframe with four variables: Three <chr> giving the original timecode, the time only, and the frames only, and one <POSIXct> giving the timestamp from the origin.
#'
#' @export
#'
#' @template tcode_expl
#'
#' @examples
#' beg <- "2016-08-16 00:00:00 UTC"
#' timecode <- "00:12:25:12"
#' str(wkf_convert_tcode(timecode, 25, beg))
#' # Round up based on framecount
#' timecode <- "00:12:25:20"
#' str(wkf_convert_tcode(timecode, 25, beg))

wkf_convert_tcode <- function (tcode, fr, origin) {

  # Break timecode into times and frame count strings
  t_matrix <- stringr::str_match(tcode, "([0-9]{2}:[0-9]{2}:[0-9]{2}):([0-9]{2})")

  # Split timecode into HH, MM, and SS strings
  split <- stringr::str_split_fixed(t_matrix[, 2], ":", n = 3)

  # Calculate time offset in seconds
  t_secs <- as.numeric(split[, 1]) * 3600 +   # hours
    as.numeric(split[, 2]) * 60 +             # mins
    as.numeric(split[, 3]) +                  # secs
    as.numeric(t_matrix[, 3]) * (1/fr)        # frames
  # Focus coding will be rounded to the nearest second
  t_secs <- round(t_secs)

  # Timestamp is class POSIXct, type "double"
  # t_secs from orign
  t_inst <- as.POSIXct(origin, tz = "UTC") + t_secs

  # Create data frame of timecodes, times, frames, and instants
  df <- cbind(as.data.frame(t_matrix), as.data.frame(t_inst))
  names(df) <- c("timecode", "time", "frames", "datetime")

  return(df)
}

# --------------------------------------------------

#' wkf_parse_sid
#'
#' Breaks a session ID into component identifiers for the session and returns them as a named list.
#'
#' @param sid A single character string showing the session ID, "res1A", etc.
#'
#' @return A list of descriptor IDs characterizing the session.
#'
#' @details
#'
#' The results depends on which phase the session is in. Unrecognized session
#' IDs return the `sid` for the phase and NAs for other list elements.
#' \bold{Currently, only research phase IDs are recognized}.
#'
#' @export
#'
#' @examples
#' wkf_parse_sid("res1A")

wkf_parse_sid <- function(sid) {

  session <- list()
  if (stringr::str_detect(sid, "^res")) {
    # known phase. Parse it
    session$phase <- "res"
    session$round <- stringr::str_match(sid, "^res(\\d)")[,2]
    session$gid   <- stringr::str_match(sid, "^res\\d(.)")[,2]
  } else {
    # unknown phase. Flag warning and return input as phase.
    warning("Unknown phase in sid")
    session$phase <- sid
    session$round <- NA
    session$gid   <- NA
  }
  return (session)
}
