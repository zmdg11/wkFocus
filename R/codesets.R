#' wkf_stack_cstamps
#'
#' Combines the codestamps from several codestamp datasets into a codestack dataset suitable for plotting code intervals.
#'
#' @param ds_list A list of `codestamp datasets ($ds_type = "cstamp").
#'
#' @return A codestack dataset ($ds_type = "stack").
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # See vignettes on exploring datasets for examples.
wkf_stack_cstamps <- function(ds_list) {

  ## Stack the codestamps and append sourcing information
  df <- ds_list %>%
    # ... make a list of data frames with sourcing info added to each
    purrr::map(~ dplyr::mutate(.data = .x$data,
                               coder = .x$ds_id$coder,
                               version = .x$ds_id$version)) %>%
    # ... reorder variables in each data frame and combine into a single data
    # frame. The added varible `Src` allows traceback to the data file
    purrr::map_df( ~ dplyr::select(.data = .x,
                      round, gid, type, coder, version, bin, In, Out, code),
                      .id = "src")

  ## Collect sources files and session IDs for each dataset
  src <- ds_list %>%
    purrr::map("ds_src")
  id  <- ds_list %>%
    purrr::map("ds_id")

  ## Here, source and IDS are lists of contributing sources and IDs
  return(list(
    ds_src  = src,
    ds_id   = id,
    ds_type = "stack",
    data    = df
  ))
}
