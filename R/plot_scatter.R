#' @title Create basic ggplot2 scatter
#'
#' @description Create a basic ggplot2 scatter plot with the option to also add
#' a line of equivalence. This could be used for comparing two versions of
#' the same model or a model and a comparator from another research group. One
#' could just use their own ggplot syntax to make these graphs but this
#' function provides a basic structure that encourages improved formatting
#' (requires axis titles, uses commas for the y-axis values etc.)
#'
#' @inheritParams plot_line_graph
#' @param x_col \[`character(1)`\]\cr
#'   Name of the column in `dt` that corresponds to x-value.
#' @param y_col \[`character(1)`\]\cr
#'   Name of the column in `dt` that corresponds to y-value.
#' @param equivalence_line \[`logical(1)`\]\cr
#'   Whether to add a line with slope 1 and intercept 0 to show where x = y.
#'   Default FALSE.
#'
#' @inherit plot_line_graph details
#'
#' @return \[`ggplot()`\] object
#'
#' @seealso
#' This function follows similar syntax to [plot_line_graph()]. See
#' `vignette("plot_line_graph")` for details.
#'
#' @examples
#' dt1 <- demCore::burkina_faso_initial_estimates$asfr
#' dt2 <- demCore::thailand_initial_estimates$asfr
#' dt <- merge(dt1, dt2, by = c("year_start", "year_end", "age_start", "age_end"))
#' plot_scatter(
#'   dt = dt,
#'   plot_title = "ASFR: Burknia Faso vs Thailand",
#'   x_col = "value.x",
#'   x_axis_title = "Burkina Faso ASFR",
#'   y_col = "value.y",
#'   y_axis_title = "Thailand ASFR",
#'   facet_form = "age_start ~ ."
#' )
#'
#' @export
plot_scatter <- function(dt,
                         plot_title,
                         x_col,
                         x_axis_title,
                         y_col,
                         y_axis_title,
                         y_axis_commas = FALSE,
                         equivalence_line = FALSE,
                         facet_type = "grid",
                         facet_form = "age_name ~ sex",
                         facet_scales = "free_y",
                         scale_manual = NULL) {

  # Validate input arguments ------------------------------------------------

  assertthat::assert_that(
    !is.null(equivalence_line),
    msg = "`equivalence_line` cannot be NULL."
  )

  aesthetics <- validate_plot_args(
    dt, plot_title, x_col, x_axis_title, y_col, y_axis_title,
    y_axis_commas, y_lb = NULL, y_ub = NULL, ribbon_alpha = NULL,
    point_col = NULL, equivalence_line, facet_type, facet_form, facet_scales,
    scale_manual
  )

  # Create ggplot -----------------------------------------------------------

  # create group aesthetic
  group_cols <- NULL
  if (!is.null(aesthetics)) {
    cols <- unique(sapply(scale_manual, function(s) s[["col"]]))
    if (length(cols) == 1) {
      group_cols <- cols
    } else {
      group_cols <- interaction(cols)
    }
  }

  p <- ggplot(data = dt,
              aes_string(
                x = x_col,
                y = y_col,
                colour = scale_manual[["colour"]][["col"]],
                fill = scale_manual[["fill"]][["col"]],
                alpha = scale_manual[["alpha"]][["col"]],
                shape = scale_manual[["shape"]][["col"]],
                linetype = scale_manual[["linetype"]][["col"]],
                size = scale_manual[["size"]][["col"]],
                group = group_cols)) +
    geom_point() +
    scale_x_continuous(name = x_axis_title) +
    theme_bw() +
    labs(title = plot_title)

  # add on geom_point if specified
  if (equivalence_line) {
    p <- p + geom_abline(slope = 1, intercept = 0)
  }

  # add on manually specified aesthetics
  for (aesthetic in aesthetics) {
    scale_aesthetic_manual_func <- get(paste0("scale_", aesthetic, "_manual"))
    scale <- scale_manual[[aesthetic]][["scale"]]
    legend_title <- scale_manual[[aesthetic]][["legend_title"]]
    p <- p + scale_aesthetic_manual_func(values = scale, limits = names(scale),
                                         name = legend_title)
  }

  # add on facet
  if (!is.null(facet_type)) {
    if (facet_type == "grid") {
      p <- p + facet_grid(stats::as.formula(facet_form), scales = facet_scales)
    } else if (facet_type == "wrap") {
      p <- p + facet_wrap(stats::as.formula(facet_form), scales = facet_scales)
    }
  }

  # add on nicely formatted y-axis
  if (y_axis_commas) {
    p <- p + scale_y_continuous(name = y_axis_title, labels = scales::comma)
  } else {
    p <- p + scale_y_continuous(name = y_axis_title)
  }

  return(p)
}
