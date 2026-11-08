###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(osmextract)
library(ggtext)
library(sbBrand)

###############################################################################
## Settings                                                                  ##
###############################################################################

first_year <- 2020
last_year <- 2025
tile_zoom <- 10

crash_size <- 3

systemfonts::require_font("Barlow")

font <- "Barlow"

bg_col <- "#f5f5f5"
bike_col <- "#d7301f"
bar_col <- "#3d3d3d"
road_major_col <- "#4a4a4a"
road_minor_col <- "#a8a8a8"
major_roads <- c("motorway", "trunk", "primary")

river_col <- "#b9dcf2"
hotspot_col <- "#E08A00"
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

pn_square <- st_point(pn_centre) |>
  st_sfc(crs = 2193) |>
  st_buffer(pn_radius, endCapStyle = "SQUARE") |>
  st_sf()

pn_extent_4326 <- pn_square |>
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

pn_roads <- st_intersection(pn_roads, pn_square)

pn_rivers <- st_intersection(pn_rivers, pn_square)


pn_points <- st_filter(pn_points_nztm, pn_square)

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

plot_title <- "Palmerston North’s top 3 bike crash sites are all intersections in the central city"
subtitle <- paste0(
  nrow(bike_crashes),
  " crashes involving a bike were reported between ",
  first_year,
  "-",
  last_year,
  ". Each dot represents one crash."
)
social_caption <- sb_social_caption(
  data_source = "NZTA Crash Analysis System (CAS) | Basemap: OpenStreetMap contributors"
)

hotspot_offsets <- tibble(
  group = 1:2,
  dx = c(-0.95, 0.25),
  dy = c(-0.6, 0.85)
)


hotspots <- top_locations |>
  filter(rank <= 3) |>
  mutate(
    group = c(1, 1, 2)[rank],
    px = st_coordinates(geometry)[, "X"],
    py = st_coordinates(geometry)[, "Y"],
    name = location |>
      str_to_title() |>
      str_replace_all("\\bStreet\\b", "St") |>
      str_replace_all("\\bAvenue\\b", "Ave") |>
      str_replace_all("\\bRoad\\b", "Rd")
  ) |>
  st_drop_geometry() |>
  summarise(
    crashes = first(n),
    n_places = n(),
    px = mean(px),
    py = mean(py),
    heading = paste(name, collapse = " and "),
    .by = group
  ) |>
  left_join(hotspot_offsets, by = "group") |>
  mutate(
    tx = pn_centre[1] + dx * pn_radius,
    ty = pn_centre[2] + dy * pn_radius,
    heading = str_wrap(heading, 24) |> str_replace_all("\n", "<br>"),
    sub_text = if_else(
      n_places > 1,
      paste0(crashes, " crashes each, one block apart"),
      paste0(crashes, " crashes")
    ),
    note = paste0(
      "<b style='color:",
      hotspot_col,
      "; font-size:10pt'>",
      heading,
      "</b><br>",
      sub_text
    ),
    ring_size = if_else(n_places > 1, 14, 9),
    len = sqrt((tx - px)^2 + (ty - py)^2),
    sx = px + (tx - px) / len * 0.07 * pn_radius,
    sy = py + (ty - py) / len * 0.07 * pn_radius
  )

fatal_xy <- pn_points |>
  filter(crashSeverity == "Fatal Crash") |>
  st_union() |>
  st_centroid() |>
  st_coordinates()

fatal_note <- tibble(
  px = fatal_xy[1, "X"],
  py = fatal_xy[1, "Y"],
  tx = pn_centre[1] + 0.2 * pn_radius,
  ty = pn_centre[2] - 0.75 * pn_radius,
  heading = paste0(
    n_fatal,
    " fatal ",
    if_else(n_fatal == 1, "crash", "crashes")
  ),
  sub_text = "Both crashes were on a 60 km/h peri-urban road",
  note = paste0(
    "<b style='color:",
    str_sub(fatal_col, 1, 7),
    "; font-size:10pt'>",
    heading,
    "</b><br>",
    sub_text
  ),
  len = sqrt((tx - px)^2 + (ty - py)^2),
  sx = px + (tx - px) / len * 0.07 * pn_radius,
  sy = py + (ty - py) / len * 0.07 * pn_radius
)

square_bbox <- st_bbox(pn_square)

library(camcorder)

gg_record(
  device = "png",
  width = 8,
  height = 9,
  units = "in",
  dpi = 300
)

p_pn <- ggplot() +
  geom_sf(data = pn_square, fill = bg_col, color = NA) +
  geom_sf(data = pn_rivers, color = river_col, linewidth = 1.2) +
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
    size = 3,
    #stroke = 0.5,
    shape = 21,
    alpha = 0.5
  ) +
  geom_sf(
    data = filter(pn_points, crashSeverity == "Fatal Crash"),
    color = fatal_col,
    fill = fatal_col,
    size = 3,
    stroke = 0.5,
    shape = 21,
    alpha = 0.7
  ) +
  geom_point(
    data = hotspots,
    aes(x = px, y = py, size = ring_size),
    color = hotspot_col,
    fill = NA,
    shape = 21,
    stroke = 1
  ) +
  scale_size_identity() +
  geom_segment(
    data = hotspots,
    aes(x = sx, y = sy, xend = tx, yend = ty),
    color = hotspot_col,
    linewidth = 0.4
  ) +
  geom_richtext(
    data = hotspots,
    aes(x = tx, y = ty, label = note),
    color = "#2b2b2b",
    fill = "#fcfcfc",
    label.color = NA,
    family = font,
    size = 8 / .pt,
    hjust = 0,
    vjust = 1
  ) +
  geom_segment(
    data = fatal_note,
    aes(x = sx, y = sy, xend = tx, yend = ty),
    color = fatal_col,
    linewidth = 0.4
  ) +
  geom_richtext(
    data = fatal_note,
    aes(x = tx, y = ty, label = note),
    color = "#2b2b2b",
    fill = "#fcfcfc",
    label.color = NA,
    family = font,
    size = 8 / .pt,
    hjust = 0,
    vjust = 1
  ) +
  coord_sf(
    xlim = c(square_bbox[["xmin"]], square_bbox[["xmax"]]),
    ylim = c(square_bbox[["ymin"]], square_bbox[["ymax"]]),
    expand = FALSE
  ) +
  labs(title = plot_title, subtitle = subtitle, caption = social_caption) +
  theme_void(base_family = font) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      face = "bold",
      size = 16
    ),
    plot.subtitle.position = "plot",
    plot.subtitle = element_textbox_simple(size = 12),
    plot.caption = element_textbox_simple(hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(15, 15, 15, 15)
  )

p_pn

ggsave(
  filename = "2026/Day9/output/palmerston_north_bike_crashes.png",
  plot = p_pn,
  width = 8,
  height = 9,
  dpi = 300,
  device = ragg::agg_png
)
