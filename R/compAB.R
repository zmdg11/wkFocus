# --------------------------------------------------

compAB_codes <- function(A, B) {

  # Compare two work-focus codings according to the decision algorithm.
  #
  # param {A, B} Work focus codes. NA indicates no code.
  #
  # return A match indicator. One of "A" (agree), "D" (disagree), or "N" (nothing
  # to compare). Returns NA if the codes cause an error in the matching tree.
  #
  # examples
  # compAB_codings("Rd", "Rd")  # returns "A"
  # compAB_codings("Rd", "Do")  # returns "D"
  # compAB_codings("Rd",  NA)   # returns "D"
  # compAB_codings( NA,   NA)   # returns "N"
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

# --------------------------------------------------

#' compAB_at_time
#'
#' Compares all coding decisions made by two coders (A, B) at a given time.
#'
#' @param AB A list combining two timesamples for two coders, A and B.
#'
#' @return A list giving the time and the two agreement decisions at that time.
#'
#' @examples
#' # One agreement
#' AB1 <- list(focus1.A = "Rd", focus2.A = NA_character_,
#'             focus1.B = "Rd", focus2.B = NA_character_)
#' comp_ab_at_time(AB1)
#' # Two agreements - order doesn't matter
#' AB2 <- list(focus1.A = "Rd", focus2.A = "Do",
#'             focus1.B = "Do", focus2.B = "Rd")
#' comp_ab_at_time(AB2)
#' # One agreement, one disagreement
#' AB3 <- list(focus1.A = "Rd", focus2.A = "Do",
#'             focus1.B = "Rd", focus2.B = NA_character_)
#' comp_ab_at_time(AB3)
#' # One agreement, one disagreement
#' list(focus1.A = "Rd", focus2.A = "Do",
#'      focus1.B = "Ds", focus2.B = "Rd") %>%
#'   comp_ab_at_time()
#' # Two disagreements
#' list(focus1.A = "Rd", focus2.A = "Do",
#'      focus1.B = "Rp", focus2.B = "Ds") %>%
#'   comp_ab_at_time()
#' \dontrun{
#' # Coder B has duplicte code - error
#' AB_err <- list(focus1.A = "Rd", focus2.A = NA_character_,
#'                focus1.B = "Rd", focus2.B = "Rd")
#' comp_ab_at_time(AB_err)}
#' # Can use `map()` to produce a data frame of decisions
#' AB1_3 <- as.data.frame(rbind(AB1, AB2, AB3))
#' AB1_3 %>% purrr::transpose() %>%
#'   purrr::map_df(comp_ab_at_time)

compAB_at_time <- function(AB) {

  ## Find the "matching matrix" of all possible comparisons between two coders.
  #
  #  Rows hold match-decisions comparing a code from coder A against both
  #  codes for coder B.

  debug <- FALSE

  if (debug) {
    message("CompAB_at_time: AB")
    print(paste(AB, collapse = ", "))
  }

  mm <- matrix(nrow = 2, ncol = 2)  # dimnames = list(c("A1", "A2"), c("B1", "B2"))

  mm[1, 1] <- compAB_codes(AB$focus1.A, AB$focus1.B)
  mm[1, 2] <- compAB_codes(AB$focus1.A, AB$focus2.B)
  mm[2, 1] <- compAB_codes(AB$focus2.A, AB$focus1.B)
  mm[2, 2] <- compAB_codes(AB$focus2.A, AB$focus2.B)

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

  ## Return a 1-row data frame of agreement comparisons.
  return(data_frame(d1 = m[1], d2 = m[2]))
}

# --------------------------------------------------

#' wkf_compAB
#'
#' Compares the codings in two datasets.
#'
#' @param ds_list List of two "tsample" datasets ($ds_type = "tsample")
#' @param bt POSIXct object giving the time to begin sampling. If NULL, sampling will begin at the time of the latest beginning.
#' @param et POSIXct object given the time to end sampling. If NULL, sampling will end at the time of the earliest ending.
#'
#' @return An agreement dataset ($ds_type = "agrAB")
#' @export
#'
#' @examples
#' # See vignettes comparing two datasets for examples of time sampling
wkf_compAB <- function(ds_list, bt = NULL, et = NULL) {

  debug <- FALSE

  ## Parameters
  #  Uses internal package data `pars`

  ## Get A and B datasets.

  if (length(ds_list) < 2) {
    stop("Less than two datasets")
  } else if (length(ds_list) > 2) {
    warning("More than two datasets. First two will be compared.")
  }

  tsA   <- ds_list[[1]]$data
  tsB   <- ds_list[[2]]$data

  ## Basic error checking of input
  if (!identical(tsA$t, tsB$t)) stop("timesampling differs between A & B")

  ## Set beginning and ending sample times
  if (is.null(bt)) bt <- max(min(tsA$t), min(tsB$t))
  if (is.null(et)) et <- min(max(tsA$t), max(tsB$t))

  ## Joint two time-samplings to get list of  t x (f1A, f2A, f1B, f2B)
  agrAB <- left_join(tsA, tsB, by = c("t"), suffix = c(".A", ".B")) %>%
    # ... compare algorithm doesn't need time
    select(-t) %>%
    # ... flip 4 lists of codes into list of 4 codes at t
    transpose() %>%
    # ... compare each set of 4 codes for dis/agreements between coders at t
    map_df(compAB_at_time)

 if (debug) {
   message("wkf_compAB: Head of agrAB")
   print(head(agrAB))
 }

  ## Organize and clean up final dataset of timestamped dis/agreement compsarisons
  agrAB <-
    # ... add timestamp back to pairs of comparisons
    bind_cols(t = tsA$t, agrAB) %>%
    # ... gather all comparisons into a single column (w = which decision)
    gather(w, d, d1:d2) %>%
    select(-w) %>%
    # ... remove "no comparison" decisions
    filter(d != "N") %>%
    # ... gathering disrupte time order, so resort by timestamp
    arrange(t)

  ## return a ds_agrAB dataset
  return(list(
    ds_src  = list(A = ds_list[[1]]$ds_src, B = ds_list[[2]]$ds_src),
    ds_id   = list(A = ds_list[[1]]$ds_id,  B = ds_list[[2]]$ds_id),
    ds_type = "agrAB",
    data    = agrAB
  ))
}
