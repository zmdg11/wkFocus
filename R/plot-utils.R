# --------------------------------------------------

#' pl_blank_session
#'
#' #' Uses a generic title. Add subtitle, y-axis label, etc through `pl_labs`
#'
#' @param pl_labs Optional list of ggplot plotting labels
#'
#' @return A ggplot plotting object with a datetime x-axis from over the max range of sessions and a discrete y-axis over the range of work focus codes.
#' @export
#'
#' @examples

pl_blank_session <- function(pl_labs = NULL) {

  ## Parameters -------------

  # std x-range for session graphs
  t_rng <- lubridate::ymd_hms(c(pars$t_min, pars$t_max))

  ## Build plot

  # Map x to the standard time range
  pl <- ggplot2::ggplot() +
    ggplot2::scale_x_datetime(
      name = "Time from beginning of session (min)",
      limits = t_rng,
      date_breaks = "1 min", date_labels = "%M") +
    # Do not drop absent factor levels
    ggplot2::scale_y_discrete(
      drop = FALSE,
      labels = pars$focus_codes) +
    ggplot2::labs(pl_labs)

  return(pl)
}

# --------------------------------------------------

#' pl_compAB
#'
#' This function creates a standard session plot object, markes out the intervals of coder disagreements using top-to-bottom grey bars, and optinally overlays a set of codestamps.
#'
#' @param agr_df Dataframe of timestamped comparisons between two coders
#' @param which Determines whether to plot agreements ("A"), or disagreements ("D")
#' @param cst_df Optional dataframe of work-focus codestamps.
#' @param pl_labs Optional ggplot plotting lables
#'
#' @return A ggplot plotting object showing the comparison between two code datasets.
#' @export
#'
#' @examples

pl_compAB <- function(agr_df, which = "D", cst_df = NULL, pl_labs = NULL) {

  df <- dplyr::filter(agr_df, d == which)

  pl <- pl_blank_session() +
    # Plot vertical line only where disagreements are.
    ggplot2::geom_segment(data = df, alpha = .3, color = "grey") +
    ggplot2::aes(x = t, xend = t + 1, y = 0, yend = 7) +
    ggplot2::labs(y = "") +
    ggplot2::labs(pl_labs)

  # Overlay cstamps if requested
  if (!is.null(cst_df)) {
    pl <- pl +
      ggplot2::geom_segment(data = cst_df, size = 3, alpha = 0.5,
                            ggplot2::aes(x = In, xend = Out, y = Code, yend = Code))
  }

  return(pl)
}

# --------------------------------------------------

#' pl_focus_codes
#'
#' Will graph segments for all codes in the database. Uses a generic title. Add subtitle, y-axis label, etc. through `pl_labs` if wanted.
#'
#' @param cst_df A standard codestamp data frame
#' @param pl_labs An optional list of ggplot2 plotting labels
#'
#' @return A ggplot object showing how the work-focus codes in `cst_df` are distributed over session time.
#' @export
#'
#' @examples

pl_focus_codes <- function(cst_df, pl_labs = NULL) {

  ## Parameters -------------

  # Get standard session plot object
  pl <- pl_blank_session(
    pl_labs = list(title = "Distribution of focus codes over the session time"))

  # Plot segment to mark code intervals. Color code by facilitation level
  pl <- pl +
    ggplot2::geom_segment(data = cst_df,
      ggplot2::aes(x = In, xend = Out, y = Code, yend = Code),
                   size = 3, alpha = 0.5) +
    ggplot2::aes(color = Bin) +
    ggplot2::scale_color_manual("", values = pars$fac_col) +
    ggplot2::labs(pl_labs)

  return(pl)
}

