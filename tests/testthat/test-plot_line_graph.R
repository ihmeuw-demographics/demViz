# create population projections
thailand_population_estimates <- demCore::ccmpp(
  inputs = demCore::thailand_initial_estimates,
  settings = list(
    years = seq(1960, 1995, 5),
    sexes = c("female", "male"),
    ages = seq(0, 80, 5),
    ages_survival = seq(0, 85, 5),
    ages_asfr = seq(15, 45, 5)
  )
)
thailand_population_estimates[, source_name := "CCMPP Projection"]
thailand_population_estimates[, source_type := "Estimate"]

thailand_population_data <- copy(demCore::thailand_data$population)
thailand_population_data[, source_name := "Census Data"]
thailand_population_data[, source_type := "Data"]

# combine projections with population data
thailand_population <- rbind(
  thailand_population_estimates,
  thailand_population_data,
  use.names = T
)

# add 'age_name' column
hierarchyUtils::gen_name(thailand_population, col_stem = "age")
thailand_population[, age_name := factor(age_name, levels = unique(thailand_population[, age_name]))]

# add column differentiating the data to be plotted as a point
thailand_population[, point := source_type == "Data"]

testthat::test_that("basic time series works", {
  # faceted by age and sex works
  testthat::expect_silent(
    plot_line_graph(
      dt = thailand_population,
      plot_title = "Thailand Population Time Series",
      x_col = "year",
      x_axis_title = "Year",
      y_col = "value",
      y_axis_title = "Population",
      y_axis_commas = TRUE,
      point_col = "point",
      scale_manual = list(
        colour = list(
          col = "source_name",
          legend_title = "Source",
          scale = c("Census Data" = "#A55B3B", "CCMPP Projection" = "#4DAF4A")
        )
      )
    )
  )

  # not faceted by anything
  testthat::expect_silent(
    plot_line_graph(
      dt = thailand_population[sex == "female" & age_start == 0],
      plot_title = "Thailand Population Time Series",
      x_col = "year",
      x_axis_title = "Year",
      y_col = "value",
      y_axis_title = "Population",
      y_axis_commas = TRUE,
      point_col = "point",
      facet_type = NULL,
      scale_manual = list(
        colour = list(
          col = "source_name",
          legend_title = "Source",
          scale = c("Census Data" = "#A55B3B", "CCMPP Projection" = "#4DAF4A")
        )
      )
    )
  )
})
