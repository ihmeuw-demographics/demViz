#' @title Create basic ggplot2 line graph
#'
#' @description Create a basic ggplot2 line graph with the option to also add
#' points. This could be used for plotting time series or age patterns for
#' different parameters. One could just use their own ggplot syntax to make
#' these graphs but this function provides a basic structure that encourages
#' improved formatting (requires axis titles, uses commas for the y-axis values
#' etc.)
#'
#' @param dt \[`data.table()`\]\cr
#'   The data.table with the values to be plotted.
#' @param plot_title \[`character(1)`\]\cr
#'   Overall title of the plot.
#' @param x_col \[`character(1)`\]\cr
#'   Name of the column in `dt` that corresponds to x-value of each line.
#' @param x_axis_title \[`character(1)`\]\cr
#'   X-axis title.
#' @param y_col \[`character(1)`\]\cr
#'   Name of the column in `dt` that corresponds to y-value of each line.
#' @param y_axis_title \[`character(1)`\]\cr
#'   X-axis title.
#' @param y_axis_commas \[`logical(1)`\]\cr
#'   Whether the y-axis scale labels should use comma formatting
#'   (scales::comma). Default is FALSE.
#' @param y_lb \[`character(1)`\]\cr
#'   Name of the column in `dt` that corresponds to the lower bound of the
#'   `y_col` variable. Default is NULL and is not plotted.
#' @param y_ub \[`character(1)`\]\cr
#'   Name of the column in `dt` that corresponds to the upper bound of the
#'   `y_col` variable. Default is NULL and is not plotted.
#' @param ribbon_alpha \[`numeric(1)`\]\cr
#'   The alpha value for the `ggplot2::geom_ribbon` call. Default is '0.2'.
#' @param point_col \[`character(1)`\]\cr
#'   Name of the column in `dt` that differentiates between values that should
#'   be plotted as a line versus a point. This column should be made up of
#'   logical values, with 'TRUE' indicating a point.
#' @param facet_type \[`character(1)`\]\cr
#'   Whether to use `gplot2::facet_wrap` ('wrap') or `gplot2::facet_grid`
#'   ('grid'). Default is 'grid'. Can be NULL if no faceting is required.
#' @param facet_form \[`character(1)`\]\cr
#'   String representing the formula that defines the rows and/or columns in the
#'   facet call.
#' @param facet_scales \[`character(1)`\]\cr
#'   `scales` argument to `gplot2::facet_wrap` or `gplot2::facet_grid`. Should
#'   scales be 'fixed, 'free', or free in one dimension ('free_x', 'free_y')?
#'   Default is 'free_y'.
#' @param scale_manual \[`list(1)`\]\cr
#'   Defines the manual accessory aesthetics to be used when plotting the lines
#'   (and points). See **Details**.
#'
#' @details
#' The `scale_manual` argument is a nested list of lists for each manual
#' aesthetic applied. The first level of the list is the name of the aesthetic,
#' the possible aethetics are 'colour', 'fill', 'alpha', 'shape', 'linetype',
#' and 'size'. Each aesthetic must have a nested list that specifies the column
#' in `dt` that the aesthetic corresponds to (`col`), the legend title for the
#' aesthetic (`legend_title`), and the scale that maps values of the aesthetic
#' to the values of the column in `dt` (`scale`).
#'
#' The groupings for lines and points will be determined by the `scale_manual`
#' columns since each combination of those should be plotted separately.
#'
#' @return \[`ggplot()`\] object
#'
#' @seealso `vignette("plot_line_graph")`
#'
#' @examples
#' plot_line_graph(
#'   dt = demCore::burkina_faso_initial_estimates$asfr,
#'   plot_title = "Burknia Faso ASFR",
#'   x_col = "year_start",
#'   x_axis_title = "Start of Year Interval",
#'   y_col = "value",
#'   y_axis_title = "ASFR",
#'   facet_form = "age_start ~ ."
#' )
#'
#' @import ggplot2
#' @export
plot_line_graph <- function(dt,
                            plot_title,
                            x_col,
                            x_axis_title,
                            y_col,
                            y_axis_title,
                            y_axis_commas = FALSE,
                            y_lb = NULL,
                            y_ub = NULL,
                            ribbon_alpha = 0.2,
                            point_col = NULL,
                            facet_type = "grid",
                            facet_form = "age_name ~ sex",
                            facet_scales = "free_y",
                            scale_manual = NULL) {

  # Validate input arguments ------------------------------------------------

  aesthetics <- validate_plot_args(
    dt, plot_title, x_col, x_axis_title, y_col, y_axis_title, y_axis_commas,
    y_lb, y_ub, ribbon_alpha, point_col, equivalence_line = NULL, facet_type,
    facet_form, facet_scales, scale_manual
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

  line_dt <- dt
  point_dt <- NULL
  if (!is.null(point_col)) {
    point_dt <- dt[get(point_col)]
    line_dt <- dt[!get(point_col)]
  }

  p <- ggplot(data = line_dt,
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
    geom_line() +
    scale_x_continuous(name = x_axis_title) +
    theme_bw() +
    labs(title = plot_title)

  # add on geom_point if specified
  if (!is.null(point_col)) {
    p <- p + geom_point(data = point_dt)
  }

  # add on geom_ribbon if specified
  if (!is.null(y_lb) & !is.null(y_ub)) {
    p <- p + geom_ribbon(aes_string(ymin = y_lb, ymax = y_ub),
                         alpha = ribbon_alpha, colour = NA)
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
