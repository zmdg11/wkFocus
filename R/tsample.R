# --------------------------------------------------

#' wkf_tsample_cstamps
#'
#' Samples a single cstamp dataset between two time stamps using a specified time step.
#'
#' @param ds A list with ds$name = dataset name and ds$data = a data frame of codestamps.
#' @param dt Sampling timestep. Defaults to 1 second.
#' @param bt POSIXct object giving the time to begin sampling. If NULL, sampling will begin at the earliest time in the dataset.
#' @param et POSIXct object given the time to end sampling. If NULL, sampling will end at the latest time in the dataset.
#' @param verbose If TRUE, codes will be printed at each timestep and a summary of missing codes will be given.
#' @param warn Logical. If true (default), warnings will be issued when an NA code
#'
#' @return A dataset of type "tsample".
#' @export
#'
#' @examples
#' smpl_codestamps <- res1A_focus_hand_cstamp[[1]]
#' time_samples <- wkf_tsample_cstamps(smpl_codestamps)
#' str(time_samples)

wkf_tsample_cstamps <- function(ds, dt = 1, bt = NULL, et = NULL,
                                verbose = FALSE, warn = TRUE) {

  ds_name <- ds$name
  cstamps <- ds$data

  ## Setup time params

  if (dt <= 0) stop(paste("nonsensical timestep: dt =", dt))

  if (is.null(bt)) bt <- min(cstamps$In)
  if (is.null(et)) et <- max(cstamps$Out)

  ## Set up blank time sample data frame
  tsample_df <- dplyr::data_frame(
    t = .POSIXct(double(), tz = "UTC"),  # create a null time var
    focus1 = factor(levels = pars$focus_codes, ordered = TRUE),
    focus2 = factor(levels = pars$focus_codes, ordered = TRUE))

  t <- bt
  i <- 1
  while(t <= et) {

    # Get all codestamps that contain this sample time. If the workfocus was
    # split at time t, there will be more two codestamps in `smple`

    smple <- dplyr::filter(cstamps, In <= t, t <= Out)
    if (verbose) {                       #  for tracking
      print(paste(t, smple$Code))
    }
    if (warn) {                          #  for suspect datasets
      if (is.na(smple[1, ]$Code))
        warning(paste("No codes at time", t, "for", ds_name), call. = FALSE)
    }

    # If only one code, the other will report as NA
    tsample_df[i, ]$t = t
    tsample_df[i, ]$focus1 <- smple[1, ]$Code
    tsample_df[i, ]$focus2 <- smple[2, ]$Code
    t  <- t + dt
    i <- i + 1
  }

  dsdes <- wkf_build_dsdesc(wkf_parse_dspath(ds_name), list(stamp = "tsample"))

  return(list(name = dsdes$name, data = tsample_df))
}

