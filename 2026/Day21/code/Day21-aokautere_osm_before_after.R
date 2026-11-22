###################
## Load libraries ##
###################

library(tidyverse)
library(sf)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(patchwork)
library(sbBrand)

###################
## Load data     ##
###################

nztm <- 2193

osm_files <- c(
  before = "2026/Day21/data/map_aokautere20261121.osm",
  after = "2026/Day21/data/map_aokautere20261122.osm"
)

read_osm_layer <- function(file, layer) {
  read_sf(file, layer = layer, quiet = TRUE) |>
    st_transform(nztm)
}

osm <- map(osm_files, \(file) {
  list(
    lines = read_osm_layer(file, "lines"),
    polygons = read_osm_layer(file, "multipolygons")
  )
})

map_bbox <- st_bbox(
  c(xmin = 175.6545, xmax = 175.6645, ymin = -40.3879, ymax = -40.3845),
  crs = st_crs(4326)
) |>
  st_as_sfc() |>
  st_transform(nztm) |>
  st_bbox()

###################
## Look at data  ##
###################

glimpse(osm$before$lines)
glimpse(osm$before$polygons)

osm |> map(\(x) count(st_drop_geometry(x$lines), highway, sort = TRUE))
osm |> map(\(x) count(st_drop_geometry(x$lines), waterway, sort = TRUE))
osm |> map(\(x) count(st_drop_geometry(x$polygons), natural, sort = TRUE))
osm |> map(\(x) count(st_drop_geometry(x$polygons), landuse, sort = TRUE))
osm |> map(\(x) count(st_drop_geometry(x$polygons), leisure, sort = TRUE))
osm |> map(\(x) count(st_drop_geometry(x$polygons), amenity, sort = TRUE))

###################
## Define colours ##
###################

bg_col <- "#f2efe9"
text_col <- "#3b3b3b"
col_wood <- "#7ead80"
col_scrub <- "#7ead80"
col_reserve <- "#c8facc"
col_reserve_line <- "#abdf96"
col_grass <- "#cdebb0"
col_residential <- "#e0dfdf"
col_farmland <- "#eef0d5"
col_water <- "#aad3df"
col_parking <- "#eeeeee"
col_parking_line <- "#d0d0d8"
col_building <- "#d9d0c9"
col_building_line <- "#b8a99c"
col_road_fill <- "#ffffff"
col_road_casing <- "#bbbbbb"
col_path <- "#fa8072"
col_track <- "#996822"
label_stream <- "#4f9dbd"
label_road <- "#7d7d7d"
label_path <- "#e0604f"
label_reserve <- "#4fb868"
label_bush <- "#2e7d32"
col_parking_symbol <- "#0092da"

###################
## Prepare data  ##
###################

road_types <- c(
  "trunk",
  "primary",
  "secondary",
  "tertiary",
  "unclassified",
  "residential",
  "living_street"
)
service_types <- c("service", "pedestrian")
path_types <- c("footway", "path", "steps", "cycleway")
bush_natural <- c("wood", "scrub")
reserve_leisure <- c("nature_reserve", "park")

get_features <- function(x) {
  list(
    landuse = x$polygons |>
      filter(
        landuse %in% c("residential", "grass", "farmland") | leisure == "garden"
      ) |>
      mutate(
        fill = case_when(
          landuse == "residential" ~ col_residential,
          landuse == "farmland" ~ col_farmland,
          .default = col_grass
        )
      ),
    reserve = filter(x$polygons, leisure %in% reserve_leisure),
    bush = x$polygons |>
      filter(natural %in% bush_natural | landuse == "forest") |>
      mutate(
        fill = if_else(
          !is.na(natural) & natural == "scrub",
          col_scrub,
          col_wood
        )
      ),
    water = filter(x$polygons, natural == "water"),
    parking = filter(x$polygons, amenity == "parking"),
    parking_symbol = x$polygons |>
      filter(amenity == "parking") |>
      st_centroid(),
    buildings = filter(x$polygons, !is.na(building)),
    stream = filter(x$lines, waterway == "stream"),
    paths = filter(x$lines, highway %in% path_types),
    tracks = filter(x$lines, highway == "track"),
    service = filter(x$lines, highway %in% service_types),
    roads = filter(x$lines, highway %in% road_types)
  )
}

features <- map(osm, get_features)

features |> map(\(x) map_int(x, nrow))

###################
## Load fonts    ##
###################

require_font("Lato")
require_font("Baloo 2")

font <- "Lato"
brand_font <- "Baloo 2"

###################
## Social media  ##
###################

github_username <- "sburgess2"
linkedin_username <- "sa-burgess2"

###################
## Titles etc.   ##
###################

plot_title <- ""

subtitle <- glue(
  "<span style='color:{label_stream}'>**Stream**</span>, ",
  "<span style='color:{label_road}'>**roads**</span>, ",
  "<span style='color:{label_path}'>**paths and tracks**</span>, ",
  "<span style='color:{label_reserve}'>**reserves**</span>, ",
  "<span style='color:{label_bush}'>**bush**</span> and ",
  "<span style='color:{col_parking_symbol}'>**car parking**</span>"
)

social_caption <- sb_social_caption(
  data_source = "© OpenStreetMap contributors",
  github_username = github_username,
  linkedin_username = linkedin_username
)

###################
## Plot          ##
###################

library(camcorder)

gg_record(
  device = "png",
  width = 8,
  height = 9,
  unit = "in",
  dpi = 300
)

make_map <- function(features, panel_label) {
  ggplot() +
    geom_sf(data = features$landuse, aes(fill = fill), colour = NA) +
    geom_sf(
      data = features$reserve,
      fill = col_reserve,
      colour = col_reserve_line,
      linewidth = 0.5
    ) +
    geom_sf(data = features$bush, aes(fill = fill), colour = NA) +
    geom_sf(data = features$water, fill = col_water, colour = NA) +
    geom_sf(
      data = features$parking,
      fill = col_parking,
      colour = col_parking_line,
      linewidth = 0.3
    ) +
    geom_sf(
      data = features$buildings,
      fill = col_building,
      colour = col_building_line,
      linewidth = 0.2
    ) +
    geom_sf(
      data = features$stream,
      colour = col_water,
      linewidth = 0.8,
      lineend = "round"
    ) +
    geom_sf(
      data = features$paths,
      colour = col_path,
      linewidth = 0.4,
      linetype = "dashed"
    ) +
    geom_sf(
      data = features$tracks,
      colour = col_track,
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    geom_sf(
      data = features$service,
      colour = col_road_casing,
      linewidth = 1.1,
      lineend = "round"
    ) +
    geom_sf(
      data = features$roads,
      colour = col_road_casing,
      linewidth = 1.9,
      lineend = "round"
    ) +
    geom_sf(
      data = features$service,
      colour = col_road_fill,
      linewidth = 0.8,
      lineend = "round"
    ) +
    geom_sf(
      data = features$roads,
      colour = col_road_fill,
      linewidth = 1.4,
      lineend = "round"
    ) +
    geom_sf(
      data = features$parking_symbol,
      shape = 22,
      fill = col_parking_symbol,
      colour = "white",
      size = 4.5,
      stroke = 0.4
    ) +
    geom_sf_text(
      data = features$parking_symbol,
      label = "P",
      family = font,
      fontface = "bold",
      colour = "white",
      size = 8 / .pt
    ) +
    scale_fill_identity() +
    coord_sf(
      xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
      ylim = c(map_bbox["ymin"], map_bbox["ymax"]),
      expand = FALSE
    ) +
    labs(title = panel_label) +
    theme_void(base_family = font, base_size = 10) +
    theme(
      plot.background = element_rect(fill = bg_col, colour = NA),
      panel.background = element_rect(fill = bg_col, colour = NA),
      panel.border = element_rect(
        colour = text_col,
        fill = NA,
        linewidth = 0.8
      ),
      plot.title.position = "panel",
      plot.title = element_text(
        family = font,
        face = "bold",
        colour = text_col,
        size = 10,
        hjust = 0,
        margin = margin(b = 4)
      ),
      plot.margin = margin(4, 4, 8, 4)
    )
}

before_map <- make_map(features$before, "Before")
after_map <- make_map(features$after, "After")

aokautere_map <- before_map /
  after_map +
  plot_annotation(
    caption = social_caption,
    theme = theme(
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      plot.margin = margin(10, 10, 10, 10),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_textbox_simple(
        family = font,
        colour = text_col,
        face = "bold",
        size = 14,
        width = unit(7.6, "in")
      ),
      plot.subtitle = element_textbox_simple(
        family = font,
        colour = text_col,
        size = 11,
        width = unit(7.6, "in"),
        margin = margin(t = 5, b = 8)
      ),
      plot.caption = element_textbox_simple(
        family = brand_font,
        colour = text_col,
        size = 10,
        width = unit(7.6, "in"),
        margin = margin(t = 10)
      )
    )
  )

aokautere_map

gg_stop_recording()
###################
## Save png      ##
###################

ggsave(
  filename = "2026/Day21/output/aokautere_osm_before_after.png",
  plot = aokautere_map,
  device = ragg::agg_png,
  width = 8,
  height = 9,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  create.dir = TRUE
)
