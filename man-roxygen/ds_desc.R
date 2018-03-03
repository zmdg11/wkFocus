#' Workfocus codestamps for session "<%= session_id %>"
#'
#' \code{<%= session_id %>_focus} is a list of one or more code datasets ("codesets") with metadata. Each codeset describes the focus of the group working on its academic task during session <%= session_id %> of the \emph{Seeing the world differently} dissertation study. The codesets differ by coder and version for that coder, as described in the codeset metadata. See FORMAT below for details.
#'
#' @docType data
#'
#' @usage <%= session_id %>_focus
#'
#' @format Each element of the list is itself a 4-element list. The first three elements are metadata specifying the codeset and the fourth is the codeset data.
#'
#' The four elements are:
#'
#' \describe{
#'   \item{ds_src}{A string giving the path from the package root directory to the raw data file this dataset was built from, typically an EXCEL spreadsheet.}
#'   \item{ds_id}{A list of three elements constituting an ID for the codeset:}
#'   \itemize{
#'     \item{$sid. A string giving the session ID. For this dataset, it should be "<%= session_id %>"}
#'     \item{$coder. A string giving the ID for the person who coded this codeset.}
#'     \item{$version. A string given a version ID for this coder. This typically gives a starting date when this coder was assigned the task. It is NOT the date of the session being coded.}
#'   }
#'   \item{ds_type}{A string giving the type of code interval in the dataset. For this dataset, it should be "cstamp"}
#'   \item{data}{A tbl. This contains the data describing the code intervals set by the coder. Factor levels are given in wkFocus::pars. Each code applied creates a "codestamp" defined by the variables:}
#'     \itemize{
#'       \item{$round. A factor. The round for this session.}
#'       \item{$gid. A factor. The group for this session}
#'       \item{$type.  A factor. The  type of code. All codestamps in this dataset should be of type "focus".}
#'       \item{$coder. A factor. The coder ID for this dataset.}
#'       \item{$In. A POSIXct object. The In-mark time for the codestamp.}
#'       \item{$Out. A POSIXct object. The Out-mark time for the codestamp.}
#'       \item{$bin. A factor. The coding bin for the codestamp.}
#'       \item{$code. A factor. The code for the codestamp.}
#'   }
#' }
#'
#' @source \emph{Seeing the world differently} dissertation study. Contact the study author.
#'
#' @keywords datasets
#' @family focus_data
#'
#' @examples
#' str(<%= session_id %>_focus)
#' head(<%= session_id %>_focus[[1]]$data)  # look at data for first codeset
#' <%= session_id %>_focus[[1]]$ds_id$sid   # get session ID for first codeset
