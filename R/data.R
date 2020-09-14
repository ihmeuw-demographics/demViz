#' @title Demographics palette
#'
#' @description Palette which can be used as `values` argument to
#'   `ggplot2::scale_color_manual` or `ggplot2::scale_fill_manual`, and
#'   includes common GBD comparators and output types.
#'
#' @format
#' A named list, where the names are:
#'   - "gbd_previous_cycle"
#'   - "gbd_current"
#'   - "gbd_recent"
#'   - "vr"
#'   - "hmd"
#'   - "wpp"
#'   - "stage_1"
#'   - "stage_2"
#'   - "prior"
#'   - "posterior"
#'   - "data"
#'
#' And the values are hex codes for colors to use for these data types.
"dem_palette"
