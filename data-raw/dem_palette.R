
library(ggplot2)
library(data.table)

# set colors
dem_palette <- c(
  "gbd_previous_cycle" = "#CBE19A",  # light green
  "gbd_current"  = "#57AF55",        # ihme green
  "gbd_recent"   = "#256b23",        # dark green
  "hmd"          = "#02028B",        # royal blue
  "wpp"          = "#00A1D9",        # UN blue
  "us_census_bureau" = "#132e50",    # dark blue
  "unicef"       = "#99dcf7",        # light blue
  "stage_1"      = "#ff9a1a",        # orange
  "stage_2"      = "#c03a3b",        # red
  "prior"        = "#d99adb",        # pink
  "posterior"    = "#8d8ae0",        # lavendar
  "data"         = "#292c3d"        # dark grey
)

# test
ggplot(data = data.table(x = names(dem_palette)),
       aes(y = 1, fill = x)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = dem_palette) +
  facet_grid(. ~ x) +
  labs(y = "", fill = "", x = "") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

# save
usethis::use_data(dem_palette, overwrite = TRUE)
