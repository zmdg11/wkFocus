#' wkf_stack_cstamps
#'
#' Combines the codestamps from several codeset datasets into a codestack dataset ($ds_type = "stack").
#'
#' @param ds_list A list of `ds_cstamp`s (fully specified codestamp datasets).
#'
#' @return A codestamp stack dataset
#' @export
#'
#' @examples
wkf_stack_cstamps <- function(ds_list) {

  ## Stack the codestamps and append sourcing information
  df <- ds_list %>%
    # ... make a list of data frames with sourcing info added to each
    purrr::map(~ dplyr::mutate(.data = .x$data,
                               Coder = .x$ds_id$coder,
                               Version = .x$ds_id$version)) %>%
    # ... reorder variables in each data frame and combine into a single data
    # frame. The added varible `Src` allows traceback to the data file
    purrr::map_df( ~ dplyr::select(.data = .x,
                                   Round, GID, Type, Coder, Version, Bin, In, Out, Code),
                                   .id = "Src")

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
