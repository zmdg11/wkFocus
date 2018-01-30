# --------------------------------------------------

#' wkf_compAB_tsample
#'
#' Compares coding decisions made by two coders (A, B) at a given time.
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

  ## Internal functions tmp <- tibble(f1 = map(dsdat, "focus1"), f2 = map(dsdat, "focus2"))

  compAB_codings <- function(A, B) {

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

  ## Return a 1-row data frame of agreement comparisons.
  return(data_frame(d1 = m[1], d2 = m[2]))
}
