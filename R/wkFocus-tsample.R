# --------------------------------------------------

#' wkf_comp_codings
#'
#' This function implements part of the decision algorithm being used to count how many times two coders coding for up to two focuses agree.
#'
#' @param {A, B} Work focus codes. NA indicates no code.
#'
#' @return A match indicator. One of "A" (agree), "D" (disagree), or "N" (nothing
#  to compare). Returns NA if the codes cause an error in the matching tree.
#' @export
#'
#' @examples
#' wkf_comp_codings("Rd", "Rd")  # returns "A"
#' wkf_comp_codings("Rd", "Do")  # returns "D"
#' wkf_comp_codings("Rd",  NA)   # returns "D"
#' wkf_comp_codings( NA,   NA)   # returns "N"

wkf_comp_codings <- function(A, B) {

  if (is.na(A) & is.na(B)) {          # no codes to compare
    match <- "N"
  } else if (is.na(A) & !is.na(B)) {  # only one code, so a disagreement
    match <- "D"
  } else if (!is.na(A) & is.na(B)) {  # only one code...
    match <- "D"
  } else if (A == B) {                # focuses match
    match <- "A"
  } else if (A != B) {
    match <- "D"                      # focuses don't match
  } else
    match <- NA

  ## Return results of comparison of two codes
  return(match)
}

# --------------------------------------------------

#' wkf_tsamples_agree
#'
#' This function implements part of the decision algorithm  being
#  used to count how many times two coders coding for up to two focuses agree.

#' @param codeset A list of length 5, giving the time and two pairs of coder
#  decisions at that time, one pair for each coder.
#'
#' @return A list giving the time and the two agreement decisions at that time.
#' @export
#'
#' @examples
wkf_tsamples_agree <- function(ts1, ts2) {

  ## Find the "matching matrix" of all possible comparisons between the two
  #  coders. Rows hold match-decisions comparing a code from coder A against both
  #  codes for coder B.

  mm <- matrix(nrow = 2, ncol = 2)

  mm[1, 1] <- wkf_comp_codings(ts1$focus1, ts2$focus1)
  mm[1, 2] <- wkf_comp_codings(ts1$focus1, ts2$focus2)
  mm[2, 1] <- wkf_comp_codings(ts1$focus2, ts2$focus1)
  mm[2, 2] <- wkf_comp_codings(ts1$focus2, ts2$focus2)

  ## Decision tree for accumulating matching decisions into agreements. Loop is
  #  over the rows of the matching matrix.

  m <- c(NA, NA)

  for (i in 1:2) {
    tmp <- mm[i, ]
    if (tmp[1] == "A") {
      if (tmp[2] == "A") {        # If A agrees with both of B, then B is error
        m[i]<- "E"
      } else if (tmp[2] == "D") { # ... agrees with one of B
        m[i]<- "A"
      } else if (tmp[2] == "N") { # ... agrees with neither of B
        m[i]<- "N"
      }
    } else if (tmp[1] == "D") {
      if (tmp[2] == "A") {        # If A agrees with one of B
        m[i]<- "A"
      } else if (tmp[2] == "D") { # ... agrees with neither
        m[i]<- "D"
      } else if (tmp[2] == "N") { # Each has an NA. Defer to 2nd comparison
        m[i]<- "N"
      }
    } else if (tmp[1] == "N") {
      if (tmp[2] == "A") {        # If A agrees with one of B
        m[i]<- "N"
      } else if (tmp[2] == "D") { # Each has an NA. Defer to 2nd comparison
        m[i]<- "N"
      } else if (tmp[2] == "N") { # No codes were compared. Error.
        m[i]<- "E"
      }
    }
  }

  ## Return vector of agreement decisions.
  return(data_frame(t = ts1$t, d1 = m[1], d2 = m[2]))
}

# --------------------------------------------------

#' wkf_tsample_cstamps
#'
#'
#' @param cstamps Data frame holding a single dataset of codestamps.
#' @param dt Sampling timestep. Defaults to 1 second.
#' @param bt POSIXct object giving the time to begin sampling. If NULL, sampling will begin at the earliest time in the dataset.
#' @param et POSIXct object given the time to end sampling. If NULL, sampling will end at the latest time in the dataset.
#' @param verbose If TRUE, codes will be printed at each timestep and a summary of missing codes will be given.
#'
#' @return A data frame of time-samples with the "desc" attribute set to the descriptor of the dataset sampled.
#' @export
#'
#' @examples
#' smpl_codestamps <- res1A_focus_hand_cstamp[[1]]
#' time_samples <- wkf_tsample_cstamps(smpl_codestamps)
#' str(time_samples)

wkf_tsample_cstamps <- function(cstamps, dt = 1, bt = NULL, et = NULL,
                                verbose = FALSE) {

  ## Setup time params

  if (dt <= 0) stop(paste("nonsensical timestep: dt =", dt))

  if (is.null(bt)) bt <- min(cstamps$In)
  if (is.null(et)) et <- max(cstamps$Out)

  ## Set up blank time sample data frame
  tsample_df <- data_frame(
    t = .POSIXct(character()),  # a way to make a null POSIXct var
    focus1 = factor(levels = wkFocus::pars$focus_codes, ordered = TRUE),
    focus2 = factor(levels = wkFocus::pars$focus_codes, ordered = TRUE))

  t <- bt
  i <- 1
  while(t <= et) {

    # Get all codestamps that contain this sample time. If the workfocus was
    # split time t, there will be more than 2 codes

    smple <- filter(cstamps, In <= t, t <= Out)
    if (verbose) {
      print(paste(t, smple$Code))  #  for tracking
      if (is.na(smple[1, ]$Code))
        warning(paste("No codes at time", t), call. = FALSE)
    }

    # If only one code, the other will report as NA
    tsample_df[i, ]$t = t
    tsample_df[i, ]$focus1 <- smple[1, ]$Code
    tsample_df[i, ]$focus2 <- smple[2, ]$Code
    t  <- t + dt
    i <- i + 1
  }

  attr(tsample_df, "desc") <-
    wkf_build_dsdesc(attr(cstamps, "desc"), list(stamp = "tsample"))

  return(tsample_df)
}

