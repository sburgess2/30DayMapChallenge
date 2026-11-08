###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(osmextract)

###############################################################################
## Settings                                                                  ##
###############################################################################

first_year <- 2020
last_year <- 2025
tile_zoom <- 10

#Note to self - try a CARTO map, but need API key

label_pt <- 8
crash_size <- 3

bg_col <- "#f5f5f5"
bike_col <- "#d7301f"
bar_col <- "#3d3d3d"
road_major_col <- "#4a4a4a"
road_minor_col <- "#a8a8a8"
major_roads <- c("motorway", "trunk", "primary")
river_col <- "#8ec3e6"
fatal_col <- "#E02800FF"
other_col <- "#F8C800FF"

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
    light,
    crashLocation1,
    crashLocation2
  )
)

bike_crashes <- cas_raw |>
  filter(
    #str_detect(region, "Whanganui"),
    tlaName == "Palmerston North City",
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
## Palmerston North map                                                      ##
###############################################################################

osm_pbf <- "2026/Day9/data/new-zealand-261105.osm.pbf"

pn_points_nztm <- bike_crashes |>
  st_as_sf(coords = c("X", "Y"), crs = 2193) # turn X/Y columns into points in NZTM (metres)

pn_extent <- pn_points_nztm |>
  st_bbox() |> # find the rectangle that contains all the points
  st_as_sfc() |> # turn that rectangle into a shape
  st_buffer(1000) # grow the shape outwards by 1500 m (1.5 km)

pn_bbox <- st_bbox(pn_extent)

pn_coords <- st_coordinates(pn_points_nztm)


pn_centre <- c(median(pn_coords[, "X"]), median(pn_coords[, "Y"]))

pn_radius <- 0.75 *
  min(
    pn_bbox[["xmax"]] - pn_bbox[["xmin"]],
    pn_bbox[["ymax"]] - pn_bbox[["ymin"]]
  ) /
  2

pn_circle <- st_point(pn_centre) |>
  st_sfc(crs = 2193) |>
  st_buffer(pn_radius, nQuadSegs = 100) |>
  st_sf()

pn_extent_4326 <- pn_circle |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(4326)


pn_roads <- oe_read(
  osm_pbf,
  layer = "lines",
  query = "SELECT highway, geometry FROM lines WHERE highway IN ('motorway','trunk','primary','secondary','tertiary','residential','unclassified')",
  boundary = pn_extent_4326,
  boundary_type = "clipsrc"
) |>
  st_transform(2193) # reproject back to NZTM to match the crash points

pn_rivers <- oe_read(
  osm_pbf,
  layer = "lines",
  extra_tags = "waterway",
  query = "SELECT waterway, geometry FROM lines WHERE waterway = 'river'",
  boundary = pn_extent_4326,
  boundary_type = "clipsrc"
) |>
  st_transform(2193) # reproject back to NZTM to match the crash points

pn_roads <- st_intersection(pn_roads, pn_circle)

pn_rivers <- st_intersection(pn_rivers, pn_circle)


pn_points <- st_filter(pn_points_nztm, pn_circle)

top_locations <- bike_crashes |>
  filter(
    !is.na(X),
    !is.na(Y),
    !is.na(crashLocation1),
    !is.na(crashLocation2)
  ) |>
  mutate(
    road_a = pmin(crashLocation1, crashLocation2),
    road_b = pmax(crashLocation1, crashLocation2),
    location = paste(road_a, "&", road_b)
  ) |>
  summarise(n = n(), X = mean(X), Y = mean(Y), .by = location) |>
  slice_max(n, n = 20, with_ties = FALSE) |>
  mutate(
    rank = row_number(),
    label = paste0(rank, ". ", location, " (", n, ")")
  ) |>
  st_as_sf(coords = c("X", "Y"), crs = 2193)

n_fatal <- sum(pn_points$crashSeverity == "Fatal Crash")

n_other <- nrow(pn_points) - n_fatal

key_x <- pn_centre[1] - 0.5 * pn_radius

key_y_other <- pn_centre[2] + 0.7 * pn_radius

key_y_fatal <- pn_centre[2] + 0.6 * pn_radius

key_other_label <- paste0("Other bike crash (", n_other, ")")


key_fatal_label <- paste0("Fatal crash (", n_fatal, ")")

plot_title <- "Bike crashes in Palmerston North from 2020 - 2025"

p_pn <- ggplot() +
  geom_sf(data = pn_circle, fill = bg_col, color = NA) +
  geom_sf(data = pn_rivers, color = river_col, linewidth = 0.3) +
  geom_sf(
    data = filter(pn_roads, !highway %in% major_roads),
    color = road_minor_col,
    linewidth = 0.25
  ) +
  geom_sf(
    data = filter(pn_roads, highway %in% major_roads),
    color = road_major_col,
    linewidth = 0.35
  ) +
  geom_sf(
    data = filter(pn_points, crashSeverity != "Fatal Crash"),
    color = other_col,
    fill = other_col,
    size = crash_size,
    stroke = 0.5,
    shape = 21,
    alpha = 0.7
  ) +
  geom_sf(
    data = filter(pn_points, crashSeverity == "Fatal Crash"),
    color = fatal_col,
    fill = fatal_col,
    size = crash_size,
    stroke = 0.5,
    shape = 21,
    alpha = 0.7
  ) +
  geom_sf(
    data = filter(top_locations, rank <= 3),
    color = road_major_col,
    fill = NA,
    size = 9,
    shape = 21,
    stroke = 1
  ) +
  geom_sf_label(
    data = filter(top_locations, rank <= 3),
    aes(label = paste0(location, "\n", n, " crashes")),
    nudge_y = 0.18 * pn_radius,
    size = label_pt / .pt,
    fill = "white",
    color = road_major_col,
    label.size = 0.3
  ) +
  geom_sf(
    data = pn_circle,
    fill = NA,
    color = road_major_col,
    linewidth = 0.6
  ) +
  annotate(
    "rect",
    xmin = key_x - 0.05 * pn_radius,
    xmax = key_x + 0.7 * pn_radius,
    ymin = key_y_fatal - 0.07 * pn_radius,
    ymax = key_y_other + 0.07 * pn_radius,
    fill = bg_col,
    alpha = 0.9
  ) +
  annotate(
    "point",
    x = key_x,
    y = key_y_other,
    color = other_col,
    fill = other_col,
    size = crash_size,
    stroke = 0.5,
    shape = 21,
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = key_x + 0.06 * pn_radius,
    y = key_y_other,
    label = key_other_label,
    hjust = 0,
    size = label_pt / .pt,
    color = road_major_col
  ) +
  annotate(
    "point",
    x = key_x,
    y = key_y_fatal,
    color = fatal_col,
    fill = fatal_col,
    size = crash_size,
    stroke = 0.5,
    shape = 21,
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = key_x + 0.06 * pn_radius,
    y = key_y_fatal,
    label = key_fatal_label,
    hjust = 0,
    size = label_pt / .pt,
    color = road_major_col
  ) +
  coord_sf(expand = FALSE) +
  labs(title = plot_title) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
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

###############################################################################
## Top ten crash locations                                                   ##
###############################################################################

top_locations

top_title <- paste0(
  nrow(top_locations),
  " locations account for ",
  sum(top_locations$n),
  " of the ",
  nrow(bike_crashes),
  " bike crashes in Palmerston North"
)

top_caption <- paste(top_locations$label, collapse = "\n")

p_top <- ggplot() +
  geom_sf(data = pn_circle, fill = bg_col, color = NA) +
  geom_sf(data = pn_rivers, color = river_col, linewidth = 0.3) +
  geom_sf(
    data = filter(pn_roads, !highway %in% major_roads),
    color = road_minor_col,
    linewidth = 0.25
  ) +
  geom_sf(
    data = filter(pn_roads, highway %in% major_roads),
    color = road_major_col,
    linewidth = 0.35
  ) +
  geom_sf(
    data = pn_circle,
    fill = NA,
    color = road_major_col,
    linewidth = 0.6
  ) +
  geom_sf(
    data = top_locations,
    aes(size = n),
    color = fatal_col,
    fill = fatal_col,
    shape = 21,
    alpha = 0.75
  ) +
  geom_sf_text(
    data = top_locations,
    aes(label = rank),
    color = "white",
    size = label_pt / .pt,
    fontface = "bold"
  ) +
  geom_sf(
    data = filter(top_locations, rank <= 3),
    color = road_major_col,
    fill = NA,
    size = 14,
    shape = 21,
    stroke = 1
  ) +
  geom_sf_label(
    data = filter(top_locations, rank <= 3),
    aes(label = paste0(location, "\n", n, " crashes")),
    nudge_y = 0.18 * pn_radius,
    size = label_pt / .pt,
    fill = "white",
    color = road_major_col,
    label.size = 0.3
  ) +
  scale_size_continuous(range = c(5, 11), guide = "none") +
  coord_sf(expand = FALSE) +
  labs(title = top_title, caption = top_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 10,
      lineheight = 1.2,
      margin = margin(t = 10)
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

p_top

ggsave(
  filename = "2026/Day9/output/palmerston_north_top_ten_locations.png",
  plot = p_top,
  width = 8,
  height = 9,
  dpi = 300,
  device = ragg::agg_png
)
