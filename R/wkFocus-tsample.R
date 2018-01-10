# --------------------------------------------------

#' wkf_compAB_tsample
#'
#' Compare the pairs of coding decisions made by each of two coders at a given time.
#'
#' @param tsAB A list containing timesamples for two coders, A and B.
#'
#' @return A list giving the time and the two agreement decisions at that time.
#' @export
#'
#' @examples
#' # One agreement
#' tsAB1 <- list(A.t = t1, A.focus1 = "Rd", A.focus2 = NA_character_,
#'               B.t = t1, B.focus1 = "Rd", B.focus2 = NA_character_)
#' wkf_compAB_tsample(tsAB1)
#' # Two agreements - order doesn't matter
#' tsAB2 <- list(A.t = t1, A.focus1 = "Rd", A.focus2 = "Do",
#'               B.t = t1, B.focus1 = "Do", B.focus2 = "Rd")
#' wkf_compAB_tsample(tsAB2)
#' # One agreement, one disagreement
#' tsAB3 <- list(A.t = t1, A.focus1 = "Rd", A.focus2 = "Do",
#'               B.t = t1, B.focus1 = "Rd", B.focus2 = NA_character_)
#' wkf_compAB_tsample(tsAB3)
#' # One agreement, one disagreement
#' list(A.t = t1, A.focus1 = "Rd", A.focus2 = "Do",
#'      B.t = t1, B.focus1 = "Ds", B.focus2 = "Rd") %>%
#'   wkf_compAB_tsample()
#' # Two disagreements
#' list(A.t = t1, A.focus1 = "Rd", A.focus2 = "Do",
#'      B.t = t1, B.focus1 = "Rp", B.focus2 = "Ds") %>%
#'   wkf_compAB_tsample()
#' \dontrun{
#' # Coder B has duplicte code - error
#' tsAB_err <- list(A.t = t1, A.focus1 = "Rd", A.focus2 = NA_character_,
#'                  B.t = t1, B.focus1 = "Rd", B.focus2 = "Rd")
#' wkf_compAB_tsample(tsAB_err)}
#' # Can use `map()` to produce a data frame of decisions
#' tsAB1_3 <- as.data.frame(rbind(tsAB1, tsAB2, tsAB3))
#' tsAB1_3 %>% purrr::transpose() %>%
#'   purrr::map_df(wkf_compAB_tsample)

wkf_compAB_tsample <- function(tsAB) {

  ## Internal functions -------------------------

  compAB_codings <- function(A, B) {

    # Compare two work-focus codings according to the decision algorithm.
    #
    # param {A, B} Work focus codes. NA indicates no code.
    #
    # return A match indicator. One of "A" (agree), "D" (disagree), or "N" (nothing
    # to compare). Returns NA if the codes cause an error in the matching tree.
    #
    # examples
    # wkf_compAB_codings("Rd", "Rd")  # returns "A"
    # wkf_compAB_codings("Rd", "Do")  # returns "D"
    # wkf_compAB_codings("Rd",  NA)   # returns "D"
    # wkf_compAB_codings( NA,   NA)   # returns "N"
    #
    #  On any time sample across two coders, each coder can mark work under one or
    #  two focus codes. This routine is used by the algorithm to compare two of
    #  them. The single result (match) is accumulated across all four possible
    #  comparisons to figure how many agreements and disagrements the two coders
    #  had at this time sample.

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

  ## Find the "matching matrix" of all possible comparisons between two coders.
  #
  #  Rows hold match-decisions comparing a code from coder A against both
  #  codes for coder B.

  mm <- matrix(nrow = 2, ncol = 2)  # dimnames = list(c("A1", "A2"), c("B1", "B2"))

  mm[1, 1] <- compAB_codings(tsAB$A.focus1, tsAB$B.focus1)
  mm[1, 2] <- compAB_codings(tsAB$A.focus1, tsAB$B.focus2)
  mm[2, 1] <- compAB_codings(tsAB$A.focus2, tsAB$B.focus1)
  mm[2, 2] <- compAB_codings(tsAB$A.focus2, tsAB$B.focus2)

  ## Decision tree for accumulating matching decisions into agreements. Loop is
  #  over the rows of the matching matrix.

  m <- c(d1 = NA, d2 = NA)

  for (i in 1:2) {
    tmp <- mm[i, ]
    if (tmp[1] == "A") {
      if (tmp[2] == "A") {        # If A agrees with both of B -> B duplicated
        m[i]<- "E"
      } else if (tmp[2] == "D") { # ... agrees with one of B
        m[i]<- "A"
      } else if (tmp[2] == "N") { # ... each coder had one NA -> nullify comparison
        m[i]<- "N"
      }
    } else if (tmp[1] == "D") {
      if (tmp[2] == "A") {        # If A agrees with one of B
        m[i]<- "A"
      } else if (tmp[2] == "D") { # ... agrees with neither
        m[i]<- "D"
      } else if (tmp[2] == "N") { # ... each coder had one NA -> nullify comparison
        m[i]<- "N"
      }
    } else if (tmp[1] == "N") {
      if (tmp[2] == "A") {        # If A agrees with one of B
        m[i]<- "N"
      } else if (tmp[2] == "D") { # ... each coder had one NA -> nullify comparison
        m[i]<- "N"
      } else if (tmp[2] == "N") { # ... No codes were compared. Error.
        m[i]<- "E"
      }
    }
  }

  ## Return a 1-row data frame of agreement decisions.
  return(data_frame(d1 = m[1], d2 = m[2]))
}

# --------------------------------------------------

#' wkf_tsample_cstamps
#'
#'
#' @param ds A list with ds$name = dataset name and ds$data = a data frame of codestamps.
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
    t = .POSIXct(character()),  # a way to make a null POSIXct var
    focus1 = factor(levels = pars$focus_codes, ordered = TRUE),
    focus2 = factor(levels = pars$focus_codes, ordered = TRUE))

  t <- bt
  i <- 1
  while(t <= et) {

    # Get all codestamps that contain this sample time. If the workfocus was
    # split at time t, there will be more two codestamps in `smple`

    smple <- filter(cstamps, In <= t, t <= Out)
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

