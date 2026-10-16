###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(maptiles)
library(rnaturalearth)

###############################################################################
## Settings                                                                  ##
###############################################################################

first_year <- 2020
last_year <- 2025
tile_zoom <- 10

#Note to self - try a CARTO map, but need API key

esri_dark <- create_provider(
  name = "esri_dark_gray",
  url = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}",
  citation = "Tiles © Esri — Esri, DeLorme, NAVTEQ"
)

bg_col <- "#0b0b0b"
bike_col <- "#FFF9A6"
bar_col <- "#3d3d3d"
urban_col <- "#FFF9A6"
open_col <- "#6b6b6b"

###############################################################################
## Load data                                                                 ##
###############################################################################

cas_raw <- read_csv(
  "2026/Day9/data/Crash_Analysis_System_(CAS)_data.csv",
  col_select = c(
    X,
    Y,
    bicycle,
    crashYear,
    crashSeverity,
    region,
    tlaName,
    urban,
    speedLimit,
    light
  )
)

bike_crashes <- cas_raw |>
  filter(
    str_detect(region, "Whanganui"),
    bicycle > 0,
    crashYear >= first_year,
    crashYear <= last_year
  ) |>
  mutate(
    crashSeverity = fct_relevel(
      crashSeverity,
      "Fatal Crash",
      "Serious Crash",
      "Minor Crash",
      "Non-Injury Crash"
    ),
    area_type = if_else(urban == "Urban", "Urban", "Open road")
  )

glimpse(bike_crashes)
count(bike_crashes, crashYear)
count(bike_crashes, area_type)

###############################################################################
## Exploratory graphs                                                        ##
###############################################################################

p_year <- bike_crashes |>
  count(crashYear) |>
  ggplot(aes(x = factor(crashYear), y = n)) +
  geom_col(fill = bar_col, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Bike crashes") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

p_year

p_severity <- bike_crashes |>
  count(crashSeverity) |>
  ggplot(aes(x = crashSeverity, y = n)) +
  geom_col(fill = bar_col, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Bike crashes") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

p_severity

p_area <- bike_crashes |>
  count(crashYear, area_type) |>
  ggplot(aes(x = factor(crashYear), y = n, fill = area_type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(Urban = bar_col, `Open road` = "#b5b5b5")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Bike crashes", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

p_area

p_tla <- bike_crashes |>
  count(tlaName, area_type) |>
  mutate(tlaName = fct_reorder(tlaName, n, .fun = sum)) |>
  ggplot(aes(x = n, y = tlaName, fill = area_type)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(Urban = bar_col, `Open road` = "#b5b5b5")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Bike crashes", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

p_tla

p_speed <- bike_crashes |>
  filter(!is.na(speedLimit)) |>
  count(speedLimit) |>
  ggplot(aes(x = factor(speedLimit), y = n)) +
  geom_col(fill = bar_col, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Speed limit (km/h)", y = "Bike crashes") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

p_speed

p_light <- bike_crashes |>
  count(light) |>
  mutate(light = fct_reorder(light, n)) |>
  ggplot(aes(x = n, y = light)) +
  geom_col(fill = bar_col, width = 0.7) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Bike crashes", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

p_light

fatal_by_year <- bike_crashes |>
  filter(crashSeverity == "Fatal Crash") |>
  count(crashYear)

fatal_by_year


###############################################################################
## Region boundary                                                          ##
###############################################################################

nz_regions <- ne_states(country = "New Zealand", returnclass = "sf")

region_boundary <- nz_regions |>
  filter(str_detect(name, "Manawat|Wanganui|Whanganui"))

stopifnot(nrow(region_boundary) > 0)

region_boundary <- region_boundary |>
  st_union() |>
  st_make_valid() |>
  st_sf()

###############################################################################
## Basemap tiles                                                             ##
###############################################################################

dir.create("2026/Day9/data/tiles", showWarnings = FALSE)

#This gets an error look at a tutorial on how to use maps.
region_tiles <- get_tiles(
  st_buffer(st_transform(region_boundary, 2193), 5000),
  provider = esri_dark,
  zoom = tile_zoom,
  crop = TRUE,
  cachedir = "2026/Day9/data/tiles"
)

#Try https://github.com/nrennie/tidytuesday/blob/main/2025/2025-12-16/20251216.R

tile_crs <- st_crs(crs(region_tiles))

region_tiles <- mask(
  region_tiles,
  vect(st_transform(region_boundary, tile_crs))
)

###############################################################################
## Crash points                                                              ##
###############################################################################

bike_points <- bike_crashes |>
  filter(!is.na(X), !is.na(Y)) |>
  st_as_sf(coords = c("X", "Y"), crs = 2193) |>
  st_transform(tile_crs)

region_outline <- st_transform(region_boundary, tile_crs)

###############################################################################
## Plot                                                                      ##
###############################################################################

p_map <- ggplot() +
  geom_spatraster_rgb(data = region_tiles, maxcell = Inf) +
  geom_sf(
    data = region_outline,
    fill = NA,
    color = "#8a8a8a",
    linewidth = 0.3
  ) +
  geom_sf(
    data = bike_points,
    color = bike_col,
    size = 0.9,
    alpha = 0.85,
    shape = 16
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

p_map

###############################################################################
## Save png                                                                  ##
###############################################################################

dir.create("2026/Day9/output", showWarnings = FALSE)

ggsave(
  filename = "2026/Day9/output/manawatu_whanganui_bike_crashes.png",
  plot = p_map,
  width = 8,
  height = 9,
  dpi = 300,
  device = ragg::agg_png
)

###############################################################################
## Palmerston North map                                                      ##
###############################################################################

pn_points_nztm <- bike_crashes |>
  filter(tlaName == "Palmerston North City", !is.na(X), !is.na(Y)) |>
  st_as_sf(coords = c("X", "Y"), crs = 2193)

pn_extent <- pn_points_nztm |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(1500)

pn_tiles <- get_tiles(
  pn_extent,
  provider = esri_dark,
  zoom = 13,
  crop = TRUE,
  cachedir = "2026/Day9/data/tiles"
)

pn_points <- st_transform(pn_points_nztm, st_crs(crs(pn_tiles)))

p_pn <- ggplot() +
  geom_spatraster_rgb(data = pn_tiles, maxcell = Inf) +
  geom_sf(
    data = pn_points,
    color = bike_col,
    size = 1.6,
    alpha = 0.85,
    shape = 16
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

p_pn

ggsave(
  filename = "2026/Day9/output/palmerston_north_bike_crashes.png",
  plot = p_pn,
  width = 8,
  height = 8,
  dpi = 300,
  device = ragg::agg_png
)
