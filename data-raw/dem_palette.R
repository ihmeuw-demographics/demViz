
library(ggplot2)
library(data.table)

# set colors
dem_palette <- c(
  "gbd_previous_cycle" = "#e41a1c",  # red
  "gbd_current"  = "#377eb8",        # blue
  "gbd_recent"   = "#ff9a1a",        # orange
  "vr"           = "#984ea3",        # purple
  "hmd"          = "#b3adb4",        # grey
  "wpp"          = "#4daf4a",        # green
  "stage_1"      = "#3cf837",        # lime green
  "stage_2"      = "#3326c7",        # royal blue
  "prior"        = "#8d8ae0",        # lavendar
  "posterior"    = "#d99adb",        # pink
  "data"         = "#292c3d"         # dark grey
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
