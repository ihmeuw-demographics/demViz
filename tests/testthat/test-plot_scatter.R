# create basic dataset
dt1 <- demCore::burkina_faso_initial_estimates$asfr
dt2 <- demCore::thailand_initial_estimates$asfr
dt <- merge(dt1, dt2, by = c("year_start", "year_end", "age_start", "age_end"))

# create color scale on year
years <- sort(unique(dt$year_start))
n_colors <- length(years)
year_color_scale <-
  grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Spectral"))(n_colors)
names(year_color_scale) <- as.character(years)
dt[, year_start := as.factor(year_start)]

testthat::test_that("basic scatter works", {

  testthat::expect_silent(
    plot_scatter(
      dt = dt,
      plot_title = "ASFR: Burknia Faso vs Thailand",
      x_col = "value.x",
      x_axis_title = "Burkina Faso ASFR",
      y_col = "value.y",
      y_axis_title = "Thailand ASFR",
      equivalence_line = TRUE,
      facet_form = "age_start ~ .",
      facet_type = "wrap",
      scale_manual = list(
        colour = list(
          col = "year_start",
          legend_title = "Year",
          scale = year_color_scale
        )
      )
    )
  )

})
