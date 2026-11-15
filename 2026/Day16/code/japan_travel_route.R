library(tidyverse)
library(jsonlite)
library(sf)
library(osmextract)
library(rnaturalearth)

points <- read_json(
  "2026/Day16/data/locations.json",
  simplifyVector = TRUE
)$locations |>
  as_tibble()

japan <- points |>
  mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "Asia/Tokyo")) |>
  filter(between(lon, 122, 154), between(lat, 24, 46)) |>
  arrange(time) |>
  distinct(lat, lon, .keep_all = TRUE) |>
  mutate(
    segment = cumsum(c(TRUE, diff(as.numeric(time)) > 3 * 3600)),
    day = as_date(time)
  )


extent <- route |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(6677) |>
  st_buffer(30000) |>
  st_transform(4326)

bb <- st_bbox(extent)

pbf <- "2026/Day16/data/japan-261114.osm.pbf"
bb_wkt <- st_as_text(st_geometry(extent))

get_osm_lines <- function(where, file) {
  if (file.exists(file)) {
    return(readRDS(file))
  }
  lines <- oe_read(
    pbf,
    layer = "lines",
    query = paste("SELECT geometry FROM lines WHERE", where),
    wkt_filter = bb_wkt,
    quiet = FALSE
  ) |>
    st_set_crs(4326)
  saveRDS(lines, file)
  lines
}

motorway <- get_osm_lines(
  "highway IN ('motorway', 'trunk')",
  "2026/Day16/data/osm_motorway_trunk.rds"
)

land <- ne_countries(scale = 50, country = "Japan", returnclass = "sf") |> # Natural Earth country outline for Japan at 1:50m detail
  st_make_valid() |> # repairs any invalid polygon geometry so cropping doesn't fail
  st_crop(c(xmin = 122, ymin = 24, xmax = 146.5, ymax = 46)) # trims to a box covering Okinawa to Hokkaido, dropping remote Pacific islands

crs_plot <- 6677

library(camcorder)
gg_record(
  device = "png",
  width = 8,
  height = 10,
  unit = "in",
  dpi = 300
)

library(camcorder)

gg_record(
  device = "png",
  width = 8,
  height = 10,
  dpi = 300
)
p <- ggplot() +
  geom_sf(
    #plot map shape
    data = st_transform(land, crs_plot),
    fill = "#f6efdc",
    colour = "#d8cdb0",
    linewidth = 0.2
  ) +
  geom_sf(
    #plot the roads
    data = st_transform(motorway, crs_plot),
    colour = "#e3d9bf",
    linewidth = 0.15
  ) +
  geom_sf(
    data = st_transform(route, crs_plot),
    aes(colour = day),
    linewidth = 0.9
  ) +
  scale_colour_viridis_c(
    option = "plasma",
    transform = "date",
    guide = "none"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    caption = "Motorways and trunk roads: © OpenStreetMap contributors. Land: Natural Earth"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e4edf0", colour = NA),
    plot.caption = element_text(
      colour = "grey40",
      size = 8,
      margin = margin(t = 6, b = 6, r = 6)
    )
  )

p

#Other colours
#d5e3ea
record_polaroid()

ggsave(
  "2026/Day16/output/japan_travel_route.png",
  p,
  width = 8,
  height = 10,
  dpi = 300
)
