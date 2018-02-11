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

#' wkf_pl_comps
#'
#' Plots a set of comparisons between two coders (A, B) and optinally overlays a set of focus codes.
#'
#' @param ds_agrAB A single dataframe of timestamped comparisons between two coders
#' @param to_plot Determines whether to plot agreements ("A"), or disagreements ("D")
#' @param ds_stack  A single dataframe of stacked codestamps to overlay on the plot.
#' @param pl_labs Optional ggplot plotting lables
#'
#' @return A ggplot plotting object showing the comparison between two code datasets.
#' @export
#'
#' @examples

wkf_pl_comps <- function(ds_agrAB, to_plot = "D", ds_stack = NULL, pl_labs = NULL) {

  df <- dplyr::filter(ds_agrAB, d == to_plot)

  pl <- pl_blank_session() +
    # Plot vertical line only where disagreements are.
    ggplot2::geom_segment(data = df, alpha = .4, color = "grey") +
    ggplot2::aes(x = t, xend = t + 1, y = 0, yend = 7) +
    ggplot2::labs(y = "") +
    ggplot2::labs(pl_labs)

  # Overlay cstamps if requested
  if (!is.null(ds_stack)) {
    pl <- pl +
      ggplot2::geom_segment(data = ds_stack$data,
                            ggplot2::aes(x = In, xend = Out, y = Code, yend = Code),
                            size = 3, alpha = 0.5) +
      ggplot2::aes(color = Bin) +
      ggplot2::scale_color_manual("", values = pars$fac_col)
  }

  return(pl)
}

# --------------------------------------------------

#' wkf_pl_cstamps
#'
#' Will graph segments for all codes in the database. Uses a generic title. Add subtitle, y-axis label, etc. through `pl_labs` if wanted.
#'
#' @param ds_stack A single dataframe of stacked cstamps
#' @param pl_labs An optional list of ggplot2 plotting labels
#'
#' @return A ggplot object showing how the work-focus codes in `ds_stack` are distributed over session time.
#' @export
#'
#' @examples

wkf_pl_cstamps <- function(ds_stack, pl_labs = NULL) {

  ## Parameters -------------

  # Get standard session plot object
  pl <- pl_blank_session(
    pl_labs = list(title = "Distribution of focus codes over the session time"))

  # Plot segment to mark code intervals. Color code by facilitation level
  pl <- pl +
    ggplot2::geom_segment(data = ds_stack$data,
      ggplot2::aes(x = In, xend = Out, y = Code, yend = Code),
                   size = 3, alpha = 0.5) +
    ggplot2::aes(color = Bin) +
    ggplot2::scale_color_manual("", values = pars$fac_col) +
    ggplot2::labs(pl_labs)

  return(pl)
}

