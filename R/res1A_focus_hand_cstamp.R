#' Handcoded focus code-stamp datasets for session "res1A"
#'
#' A list of data frames. Each data frame holds the codestamps for a set of hand-coded work-focus codes by a single coder for session "res1A". Each dataset is uniquely described by the type of timed stamp in the datset ("cstamp"), the session ID ("res1A"), the code type ("focus"), coding method ("hand"), the coder ID, and a version ID for this coder. A data-frame attribute, "desc", gives these identifiers as a named list.
#'
#' @format A list of data frames, each with 8 variables:
#' \describe{
#'   \item{Round}{The round for this session: Factor "1"}
#'   \item{GID}{The group for this session: Factor "A"}
#'   \item{Type}{The type of code for this dataset: Factor "focus"}
#'   \item{Coder}{The coder ID for this dataset: Factor}
#'   \item{In}{The In mark point for the codestamp: POSIXct object}
#'   \item{Out}{The Out mark point for the codestamp. POSIXct object}
#'   \item{Bin}{The coding bin for the codestamp: Factor "fac" or "ind"}
#'   \item{Code}{The code for the codestamp: "Rd", "Do", "Rp", "Ds", "O", or "Of"}
#' }
#' @source _Seeing the world differently_ study. Contact the author.
"res1A_focus_hand_cstamp"
