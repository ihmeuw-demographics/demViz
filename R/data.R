#' @title Demographics palette
#'
#' @description Palette which can be used as `values` argument to
#'   `ggplot2::scale_color_manual` or `ggplot2::scale_fill_manual`, and
#'   includes common GBD comparators and output types.
#'
#' @format
#' A named list, where the names are:
#'
#'   - "gbd_previous_cycle" : final results for previous GBD release
#'   - "gbd_current" : results for the current model
#'   - "gbd_recent" : results for a compare version from the same GBD cycle
#'   - "hmd" : Human Mortality Database comparator
#'   - "wpp" : UN World Population Prospects comparator
#'   - "us_census_bureau" : US Census Bureau comparator
#'   - "unicef" : UNICEF comparator
#'   - "stage_1" : first-stage results for multi-stage models
#'   - "stage_2" : second-stage results for multi-stage models (commonly
#'       space-time results)
#'   - "unscaled" : final results prior to scaling
#'   - "prior" : the prior to a Bayesian model
#'   - "posterior" : the posterior from a Bayesian model
#'   - "data" : input data
#'
#' And the values are hex codes for colors to use for these data types.
#'
#' @examples
#' library(ggplot2)
#' ggplot(data = data.frame(x = names(dem_palette)),
#'        aes(y = 1, fill = x)) +
#'        geom_bar() +
#'        theme_minimal() +
#'        scale_fill_manual(values = dem_palette) +
#'        facet_grid(. ~ x) +
#'        labs(y = "", fill = "", x = "") +
#'        theme(axis.text.y = element_blank(),
#'              axis.text.x = element_blank(),
#'              legend.position = "none")
#'
"dem_palette"
