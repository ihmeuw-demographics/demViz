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
#'   The alpha value for the `geom_ribbon` call. Default is '0.2'.
#' @param point_col \[`character(1)`\]\cr
#'   Name of the column in `dt` that differentiates between values that should
#'   be plotted as a line versus a point. This column should be made up of
#'   logical values, with 'TRUE' indicating a point.
#' @param facet_type \[`character(1)`\]\cr
#'   Whether to use `facet_wrap` ('wrap') or `facet_grid` ('grid'). Default is
#'   'grid'.
#' @param facet_form \[`character(1)`\]\cr
#'   String representing the formula that defines the rows and/or columns in the
#'   facet call.
#' @param facet_scales \[`character(1)`\]\cr
#'   `scales` argument to `facet_wrap` or `facet_grid`. Should scales be fixed
#'   ("fixed", the default), free ("free"), or free in one dimension
#'   ("free_x", "free_y")? Default is 'free_y'.
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

  # check title arguments
  assertthat::assert_that(
    assertthat::is.string(plot_title),
    assertthat::is.string(x_axis_title),
    assertthat::is.string(y_axis_title),
    msg = "`plot_title`, `x_axis_title` and `y_axis_title` must be strings"
  )

  # check `y_axis_commas` argument
  assertive::assert_is_logical(y_axis_commas)

  # check `ribbon_alpha` argument
  assertthat::assert_that(
    assertthat::is.number(ribbon_alpha),
    msg = "`ribbon_alpha` must be a number"
  )

  # check `dt`
  assertive::assert_is_data.table(dt)

  # check `x_col` and `y_col` arguments
  assertthat::assert_that(
    assertthat::is.string(x_col),
    assertthat::is.string(y_col),
    all(c(x_col, y_col) %in% names(dt)),
    msg = "`x_col` and `y_col` must be strings that correspond to columns in `dt`"
  )

  # check `y_lb` and `y_ub` arguments
  assertthat::assert_that(
    assertthat::is.string(y_lb) | is.null(y_lb),
    assertthat::is.string(y_ub) | is.null(y_ub),
    identical(class(y_lb), class(y_ub)),
    ifelse(!is.null(y_lb), all(c(y_lb, y_ub) %in% names(dt)), T),
    msg = "`y_lb` and `y_ub` must both be strings (or NULL) that correspond to columns in `dt`"
  )

  aesthetics <- NULL
  if (!is.null(scale_manual)) {

    # determine the manual aesthetics specified (colour, alpha etc.)
    allowed <- c("colour", "fill", "alpha", "shape", "linetype", "size")
    assertthat::assert_that(
      assertive::is_list(scale_manual),
      assertive::has_names(scale_manual),
      all(names(scale_manual) %in% allowed),
      msg = paste0("`scale_manual` must be a named list whose names correspond to ",
                   "one of the allowed aesthetics ('",
                   paste(allowed, collapse = "', '"), "')")
    )
    aesthetics <- names(scale_manual)

    for (aesthetic in aesthetics) {

      # check manually specified aesthetic columns are strings in `dt`
      column <- scale_manual[[aesthetic]][["col"]]
      assertthat::assert_that(
        assertthat::is.string(column),
        column %in% names(dt),
        msg = paste0("Column specified for ", aesthetic, " must be a string ",
                     " that corresponds to a column in `dt`")
      )

      # check title arguments
      title <- scale_manual[[aesthetic]][["legend_title"]]
      assertthat::assert_that(
        assertthat::is.string(title) | is.null(title),
        msg = paste0("Legend title specified for ", aesthetic, " must be a ",
                     "string or NULL")
      )

      # check scale argument is a named vector and corresponds to values in the specified `dt` column
      scale <- scale_manual[[aesthetic]][["scale"]]
      assertthat::assert_that(
        assertive::is_vector(scale),
        assertive::has_names(scale),
        all(unique(dt[[column]]) %in% names(scale)),
        msg = paste0("Each unique value of the ", aesthetic, " column in `dt` ",
                     "must correspond to one of the named elements of its scale")
      )
    }
  }

  # check facet arguments
  if (!is.null(facet_type)) {
    # check `facet_type` argument
    assertthat::assert_that(
      assertthat::is.string(facet_type),
      facet_type %in% c("grid", "wrap"),
      msg = "`facet_type` must be one of 'grid' or 'wrap' or NULL"
    )
    # check `facet_form` argument
    assertthat::assert_that(
      assertthat::is.string(facet_form),
      msg = "`facet_form` must be a string when `facet_type` is not NULL"
    )
    assertthat::assert_that(
      assertthat::is.string(facet_scales),
      facet_scales %in% c("fixed", "free", "free_x", "free_y"),
      msg = paste("`facet_scales` must be on of 'fixed', 'free', 'free_x' or",
                  "'free_y' when `facet_type` is not NULL")
    )
  }

  # check `point_col` argument
  if (!is.null(point_col)) {
    assertthat::assert_that(
      assertthat::is.string(point_col),
      point_col %in% names(dt),
      assertive::is_logical(dt[[point_col]]),
      msg = paste("`point_col` must be a string that corresponds to a column",
                  "in `dt` with logical values")
    )
  }

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
