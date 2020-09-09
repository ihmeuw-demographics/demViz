#' @title Validate plot arguments
#' @description Helper function for common validations to demViz plotting
#'   functions. Also includes prep of `aesthetics` input.
#' @inheritParams plot_line_graph
#' @inheritParams plot_scatter
#' @return \[`list()`\]\cr aesthetics prepped
validate_plot_args <- function(dt,
                               plot_title,
                               x_col,
                               x_axis_title,
                               y_col,
                               y_axis_title,
                               y_axis_commas = NULL,
                               y_lb = NULL,
                               y_ub = NULL,
                               ribbon_alpha = NULL,
                               point_col = NULL,
                               equivalence_line = NULL,
                               facet_type = NULL,
                               facet_form = NULL,
                               facet_scales = NULL,
                               scale_manual = NULL) {

  # simple checks -----------------------------------------------------------

  # check title arguments
  assertthat::assert_that(
    assertthat::is.string(plot_title),
    assertthat::is.string(x_axis_title),
    assertthat::is.string(y_axis_title),
    msg = "`plot_title`, `x_axis_title` and `y_axis_title` must be strings"
  )

  # check `y_axis_commas` argument
  if (!is.null(y_axis_commas)) {
    assertive::assert_is_logical(y_axis_commas)
  }

  # check `ribbon_alpha` argument
  if (!is.null(ribbon_alpha)) {
    assertthat::assert_that(
      assertthat::is.number(ribbon_alpha),
      msg = "`ribbon_alpha` must be a number"
    )
  }

  # check `equivalence_line`
  if (!is.null(equivalence_line)) {
    assertive::assert_is_logical(equivalence_line)
  }

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

  # build and check aesthetics ----------------------------------------------

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

  # return prepped `aesthetics`
  return(aesthetics)

}


