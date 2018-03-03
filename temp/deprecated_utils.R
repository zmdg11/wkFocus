#-----------------------------------------------------

#' wkf_build_dsdesc
#'
#' Deprecated. Builds a dataset descriptor list from an existing dataset descriptor and specified dataset identifiers.
#'
#' @param dslist A dataset descriptor list with either the name element or a set of initial dataset identifiers present.
#' @param change A list specifying how dataset identifiers should be replaced.
#'
#' @return A dataset descriptor list. The first element (name) will be the dataset name createc by applying any change elements to the description in `dslist`. Elements 2:7 (sid, type, method, coder, version, and stampe) will be the component identifiers of that name. Any identifiers unspecied by `dslist$name` and `change` will be "".
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

#' wkf_parse_dspath
#'
#' Deprecated. Extracts the file name in `path`, excluding any extention, and parses it for component identifiers of a work-focus coding dataset. The identifiers are located soley by their position in the file name.
#'
#' @param path Path to a file holding a work focus dataset, or the name of a dataset. The dataset name should be of the form sid-type-method-coder-version-stamp. Trailing identifiers can be missing.
#'
#' @return A list with the full dataset name (`name`), the session ID (`sid`), the code type (`type`),the method (`method`), the coder (`coder`), the version (`version`), and the type of stamp (`stamp`). Missing trailing identifiers will return as "". An empty file path returns a descriptor with empty identifiers.
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


