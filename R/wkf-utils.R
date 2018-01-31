#-----------------------------------------------------

#' wkf_build_dsdesc
#'
#' Builds a dataset descriptor list from an existing dataset descriptor and specified dataset identifiers.
#'
#' @param dslist A dataset descriptor list with either the name element or a set of initial dataset identifiers present.
#' @param change A list specifying how dataset identifiers should be replaced.
#'
#' @return A dataset descriptor list. The first element (name) will be the dataset name createc by applying any change elements to the description in `dslist`. Elements 2:7 (sid, type, method, coder, version, and stampe) will be the component identifiers of that name. Any identifiers unspecied by `dslist$name` and `change` will be "".
#'
#' @export
#'
#' @examples
#' dsid <- list("", "res2C", "focus", "video", "mdg", "20180101", "cstamp")
#' names(dsid) <- c("name", "sid", "type", "method", "coder", "version", "stamp")
#' change <- list(coder = "slg", version = "20180102")
#' # Build a name from identfier list with changes
#' wkf_build_dsdesc(dsid, change)
#' #
#' # Build a name from identifiers list without change
#' dsid$name <- ""
#' wkf_build_dsdesc(dsid, change = NULL)
#' #
#' # Build descriptor by swapping parts of a ds name
#' dsid$name <- "res1A_focus_hand_slg_20180101_tsample"
#' dsid[2:7] <- ""
#' wkf_build_dsdesc(dsid, list(version = "20171228"))
#' \dontrun{
#' # Poor specification - embedded identifier is missing
#' dsid$name <- "res2C_focus_hand"
#' dsid[2:7] <- ""
#' wkf_build_dsdesc(dsid, list(version = "20171228"))}
#' # Correct
#' dsid$name <- "res2C_focus_hand"
#' dsid[2:7] <- ""
#' wkf_build_dsdesc(dsid, list(coder = "slg", version = "20171228", stamp = ""))
#' # Build a blank descriptor
#' wkf_build_dsdesc()

wkf_build_dsdesc <- function(dslist = NULL, change = NULL) {

  if (is.null(dslist)) {
    # create a list of empty IDs
    dslist <- wkf_parse_dspath()
  } else if (dslist[1] != "") {
    # If there is a ds name, overwrite current IDs
    dslist <- wkf_parse_dspath(dslist$name)
  }

  # Replace identifiers according to list in `change`.
  dslist <- replace(dslist, names(change), change)

  # If any identifiers are specified, try to overwrite the name. However,
  # test for ill-formed dsnames: wkFocus tools require all leading IDs to be
  # significant. Trailing IDs can be missing, but not ones in the middle.

  if (any(dslist[-1] != "")) {       # If there's and ID, try to use it

    inx <- max(which(dslist != ""))  # find place of last ID in the list

    if (any(dslist[2:inx] == "")) {  # any blank IDs before that are in error
      stop(paste0("Nontrailing ID is missing in '", path, "'"))
    } else {
    dslist[1] <- paste(dslist[2:inx], collapse = "_")
    }

  } else  dslist[1] <- ""            # all IDs are empty

  return(dslist)
}

#-----------------------------------------------------

#'wkf_config
#'
#'Provides the project configuration file establishing global parameters such as
#'plotting scales, color codes for factor levels, etc.
#'
#'@return A list of named parameters configuring this project
#'@export
#'
wkf_config <- function() {
  return(pars)  # `pars` is in package environment via `sysdata.Rda`
}

#-----------------------------------------------------

#' wkf_convert_tcode
#'
#' Converts timecodes into a number of seconds.
#'
#' @param tcode timecode <chr> of form "hh:mm:ss:ff".
#' @param fr the frame rate the for the video timecode
#' @param origin a <chr> giving a starting point for the timestamps
#'
#' @return dataframe with four variables: three <chr> giving the original timecode, time only, and the frames only, and one <POSIXct> giving the time from the origin.
#' @export
#'
#' @examples
#' beg <- "2016-08-16 00:00:00 UTC"
#' timecode <- "00:12:25:12"
#' str(wkf_convert_tcode(timecode, 25, beg))
#' # Round up based on framecount
#' timecode <- "00:12:25:20"
#' str(wkf_convert_tcode(timecode, 25, beg))

wkf_convert_tcode <- function (tcode, fr, origin) {

  # Break timecode into timestampe and frame count
  t_matrix <- stringr::str_match(tcode, "([0-9]{2}:[0-9]{2}:[0-9]{2}):([0-9]{2})")

  # Split time into HH, MM, and SS
  split <- stringr::str_split_fixed(t_matrix[, 2], ":", n = 3)

  # Calculate time in seconds
  t_secs <- as.numeric(split[, 1]) * 3600 +      # hours
    as.numeric(split[, 2]) * 60 +        # mins
    as.numeric(split[, 3]) +             # secs
    as.numeric(t_matrix[, 3]) * (1/fr)   # frames
  # Focus coding will be rounded to the nearest second
  t_secs <- round(t_secs)

  # Timestamps is class POSIXct, type "double"
  # t_secs from orign, typically set in pars
  t_inst <- as.POSIXct(origin, tz = "UTC") + t_secs

  # Create data frame of timecodes, times, frames, and instants
  df <- cbind(as_data_frame(t_matrix), as_data_frame(t_inst))
  names(df) <- c("timecode", "time", "frames", "datetime")

  return(df)
}

#-----------------------------------------------------

#' wkf_parse_dspath
#'
#' Extracts the file name in `path`, excluding any extention, and parses it for component identifiers of a work-focus coding dataset. The identifiers are located soley by their position in the file name.
#'
#' @param path Path to a file holding a work focus dataset, or the name of a dataset. The dataset name should be of the form sid-type-method-coder-version-stamp. Trailing identifiers can be missing.
#'
#' @return A list with the full dataset name (`name`), the session ID (`sid`), the code type (`type`),the method (`method`), the coder (`coder`), the version (`version`), and the type of stamp (`stamp`). Missing trailing identifiers will return as "". An empty file path returns a descriptor with empty identifiers.
#'
#' @export
#'
#' @examples
#' path <- "~/data/res2C_focus_hand_mdg_20180101_cstamp.Rda"
#' wkf_parse_dspath(path)
#' # Does not need directory specifier
#' path <- "res1A_focus_video.xlsx"
#' wkf_parse_dspath(path)
#' # Works for bare dsname wo/extension
#' path <- "res1A_focus_hand_slg_20180101_tsample"
#' wkf_parse_dspath(path)
#' \dontrun{
#' # Can't have missinng ds IDs in the middle
#' path <- "video_focus_video__20180101.edl"
#' wkf_parse_dspath(path)}

wkf_parse_dspath <- function(path = "") {

  fn <- tools::file_path_sans_ext(basename(path))

  # Assumes file name is in proper format to split up into component identifiers
  # NB. Empty string parses into empty identifiers, which will return well-formed
  # but empty desciptor list.

  tmp <- stringr::str_split_fixed(fn, "_", n = 6)

  tmp <- list(name = fn,
              sid = tmp[1],
              type = tmp[2],
              method = tmp[3],
              coder = tmp[4],
              version = tmp[5],
              stamp = tmp[6])

  # Test for ill-formed dsnames: wkFocus tools require all leading IDs to be
  # significant. Trailing IDs can be missing, but not ones in the middle.
  # However, an empty fn produces empty IDs, which is OK.

  if (fn != "") {
    inx <- max(which(tmp != ""))
    if (any(tmp[1:inx] == ""))
      stop(paste0("Nontrailing ID is missing in '", path, "'"))
  }

  return(tmp)
}

#' wkf_parse_sid
#'
#' Breaks a session ID into component identifiers for the session and returns a list of identifiers that can be used to build a dataset descriptor. The results depends of which phase the session is in.
#'
#' @param sid A single character string showing the session ID, "res1A", etc.
#'
#' @return A list of descriptor IDs characterizing the session.
#' @export
#'
#' @examples
#' wkf_parse_sid("res1A")

wkf_parse_sid <- function(sid) {
session <- list()
  if (stringr::str_detect(sid, "^res")) {
    session$phase <- "res"
    session$round <- stringr::str_match(sid, "^res(\\d)")[,2]
    session$gid <- stringr::str_match(sid, "^res\\d(.)")[,2]
  }
  return (session)
}