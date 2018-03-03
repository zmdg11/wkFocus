#' Workfocus codestamps for session "<%= dsname %>"
#'
#' \code{<%= dsname %>_focus} is a list of one or more code datasets ("codesets") with metadata. Each codeset describes the focus of group "A" while they worked on the academic task of round 1 of the "research phase" of the \emph{Seeing the world differently} dissertation study. The metadata for each codeset details who coded the the set, their version, etc. See FORMAT below for details.
#'
#' @format Each element of the list is itself a 4-element list. The first three elements are metadata specifying the codeset. The fourth element is a tbl containing the codeset data. The four elements are:
#'
#' \describe{
#'   \item{ds_src}{A string giving the path from the package root directory to the raw data file this dataset was built from.}
#'   \item{ds_id}{A list of three elements constituting an ID for the codeset:}
#'   \itemize{
#'     \item{sid. A string giving the session ID. For this dataset, it should be "<%= dsname %>"}
#'     \item{coder. A string giving the ID for the person who coded this codeset.}
#'     \item{version. A string given a version ID for this coder. This typically gives a starting date when this coder was assigned the task. It is NOT the date of the session being coded.}
#'   }
#'   \item{ds_type}{A string giving the type of code interval in the dataset. For this dataset, it should be "cstamp"}
#'   \item{data}{A tbl. This contains the data describing the code intervals set by the coder. For each code applied:}
#'     \itemize{
#'       \item{round. The round for this session: Factor "1"}
#'       \item{gid. The group for this session: Factor "C"}
#'       \item{type. The type of code for this dataset: Factor "focus"}
#'       \item{coder. The coder ID for this dataset: Factor}
#'       \item{In. The In mark point for the codestamp: POSIXct object}
#'       \item{Out. The Out mark point for the codestamp. POSIXct object}
#'       \item{bin. The coding bin for the codestamp: Factor "fac" or "ind"}
#'       \item{code. The code for the codestamp: "Rd", "Do", "Rp", "Ds", "O", or "Of"}
#'   }
#' }
#'
#' @source _Seeing the world differently_ study. Contact the author.
#'
#' @keywords datasets
#' @family focus_data
