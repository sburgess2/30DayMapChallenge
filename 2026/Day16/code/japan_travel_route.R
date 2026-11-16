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

legs <- japan |>
  mutate(
    lon_end = lead(lon), #lead pulss the next points long and lat and time onto the same row.
    lat_end = lead(lat),
    time_end = lead(time)
  ) |>
  filter(!is.na(lon_end))

dist_km <- as.numeric(
  st_distance(
    st_as_sf(legs, coords = c("lon", "lat"), crs = 4326),
    st_as_sf(legs, coords = c("lon_end", "lat_end"), crs = 4326),
    by_element = TRUE
  )
) /
  1000

route <- legs |>
  mutate(
    dist_km = dist_km,
    hours = as.numeric(difftime(time_end, time, units = "secs")) / 3600,
    speed = dist_km / hours,
    mode = case_when(
      dist_km > 200 ~ "flight",
      speed >= 7 ~ "car",
      .default = "walk"
    )
  ) |>
  filter(mode == "flight" | hours <= 3) |>
  mutate(
    run = cumsum(
      mode != lag(mode, default = first(mode)) |
        time != lag(time_end, default = first(time))
    )
  ) |>
  group_by(run) |>
  summarise(
    mode = first(mode),
    day = first(day),
    km = sum(dist_km),
    geometry = list(
      st_linestring(cbind(c(lon, last(lon_end)), c(lat, last(lat_end))))
    )
  ) |>
  ungroup() |>
  mutate(geometry = st_sfc(geometry, crs = 4326)) |>
  st_as_sf()


km_by_mode <- route |>
  st_drop_geometry() |>
  summarise(km = sum(km), .by = mode) |>
  deframe()

km_caption <- paste0(
  round(km_by_mode[["car"]]),
  " km driving, ",
  round(km_by_mode[["walk"]]),
  " km walking"
)

land <- ne_countries(scale = 50, country = "Japan", returnclass = "sf") |> # Natural Earth country outline for Japan at 1:50m detail
  st_make_valid() |> # repairs any invalid polygon geometry
  st_cast("POLYGON") |> # splits the single multi-island shape into one row per island
  slice_max(st_area(geometry), n = 1) # keeps only the largest island, which is Honshu

extent <- land |>
  st_bbox() |>
  st_as_sfc()

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
  "highway IN ('motorway')",
  "2026/Day16/data/osm_motorway_honshu.rds"
) |>
  st_filter(land)

crs_plot <- 6677

stays <- read_json("2026/Day16/data/trip.json")$all_steps |>
  map(
    \(s) {
      tibble(
        place = if (nzchar(s$name)) s$name else s$location$name,
        country = s$location$country_code,
        lat = s$location$lat,
        lon = s$location$lon
      )
    }
  ) |>
  list_rbind() |>
  filter(country == "JP", !place %in% c("Narita", "Nagoya airport")) |>
  distinct(place, .keep_all = TRUE) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

icons <- route |>
  st_transform(crs_plot) |>
  filter(mode == "flight" | run == min(run[mode == "car"])) |>
  mutate(
    icon = if_else(mode == "flight", "✈", "\U0001F697"),
    geometry = st_cast(st_line_sample(geometry, sample = 0.5), "POINT")
  )

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
    fill = "#f5f5f5",
    colour = "#e3d9bf",
    linewidth = 0.2
  ) +
  geom_sf(
    #plot the roads
    data = st_transform(motorway, crs_plot),
    colour = "#bdbdbd",
    linewidth = 0.15
  ) +
  geom_sf(
    data = st_transform(route, crs_plot),
    aes(linetype = mode),
    colour = "#4a67b0",
    linewidth = 0.9
  ) +
  geom_sf(
    data = stays,
    shape = 21,
    size = 3,
    stroke = 1,
    fill = "#f5f5f5",
    colour = "#4a67b0"
  ) +
  geom_sf_text(
    data = icons,
    aes(label = icon),
    family = "Segoe UI Symbol",
    size = 7,
    colour = "#4a67b0"
  ) +
  scale_linetype_manual(
    values = c(car = "solid", walk = "dotted", flight = "dashed"),
    guide = "none"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Japan - Nakasendo Way",
    subtitle = "12 - 24 October 2026",
    caption = paste0(
      km_caption,
      "\nMotorways: © OpenStreetMap contributors. Land: Natural Earth"
    )
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#dcdcdc", colour = NA),
    plot.title = element_text(
      colour = "grey20",
      face = "bold",
      size = 18,
      margin = margin(t = 10, l = 10, b = 2)
    ),
    plot.subtitle = element_text(
      colour = "grey40",
      size = 11,
      margin = margin(l = 10, b = 6)
    ),
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
