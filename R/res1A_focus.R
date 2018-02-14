#' Handcoded focus code-stamp datasets for session "res1A"
#'
#' A list of wkFocus "datsets" of hand-coded work-focus codes by a one or more coders for session "res1A". Each dataset is a list of two elements: .$name is a charcter string giving the unique name described by the session ID ("res1A"), the code type ("focus"), the coding method ("hand"), the coder ID and a version ID for this coder, and the type of timed stamp in the datset ("cstamp"); .$data is a dataframe holding the codestamps, described below.
#'
#' @format Datasets may have differing number of rows of 8 variables:
#' \describe{
#'   \item{round}{The round for this session: Factor "1"}
#'   \item{gid}{The group for this session: Factor "C"}
#'   \item{type}{The type of code for this dataset: Factor "focus"}
#'   \item{coder}{The coder ID for this dataset: Factor}
#'   \item{In}{The In mark point for the codestamp: POSIXct object}
#'   \item{Out}{The Out mark point for the codestamp. POSIXct object}
#'   \item{bin}{The coding bin for the codestamp: Factor "fac" or "ind"}
#'   \item{code}{The code for the codestamp: "Rd", "Do", "Rp", "Ds", "O", or "Of"}
#' }
#' @source _Seeing the world differently_ study. Contact the author.
"res1A_focus"
