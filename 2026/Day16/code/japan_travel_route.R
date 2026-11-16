library(tidyverse)
library(lubridate)
library(jsonlite)
library(sf)
library(osmextract)
library(rnaturalearth)
library(ggtext)
library(terra)
library(tidyterra)
library(ggnewscale)

systemfonts::require_font("Cormorant Garamond")

points <- read_json(
  "2026/Day16/data/locations.json",
  simplifyVector = TRUE
)$locations |>
  as_tibble()

japan <- points |>
  mutate(time = as_datetime(time, tz = "Asia/Tokyo")) |>
  filter(between(lon, 122, 154), between(lat, 24, 46)) |>
  arrange(time) |>
  distinct(lat, lon, .keep_all = TRUE) |>
  mutate(
    segment = cumsum(c(TRUE, time_length(diff(time), "hour") > 3)),
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

legs <- legs |>
  mutate(
    dist_km = dist_km,
    hours = time_length(interval(time, time_end), "hour")
  )

night_legs <- legs |>
  filter(hours > 8, dist_km < 5) |>
  mutate(night = row_number())

roll5 <- function(x) {
  coalesce(lag(x, 2), 0) +
    coalesce(lag(x), 0) +
    x +
    coalesce(lead(x), 0) +
    coalesce(lead(x, 2), 0)
}

route <- legs |>
  filter(dist_km > 200 | hours <= 3) |>
  mutate(
    dist_c = if_else(dist_km > 200, 0, dist_km),
    hours_c = if_else(dist_km > 200, 0, hours),
    speed = roll5(dist_c) / roll5(hours_c),
    mode = case_when(
      dist_km > 200 ~ "flight",
      speed >= 7 ~ "car",
      .default = "walk"
    )
  ) |>
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
  " km driving | ",
  round(km_by_mode[["walk"]]),
  " km walking"
)

crs_plot <- 6677

land <- ne_countries(scale = 50, country = "Japan", returnclass = "sf") |> # Natural Earth country outline for Japan at 1:50m detail
  st_make_valid() # repairs any invalid polygon geometry

bb_main <- route |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(crs_plot) |>
  st_buffer(60000) |>
  st_bbox()

extent <- bb_main |>
  st_as_sfc() |>
  st_segmentize(5000) |>
  st_transform(4326)

land_main <- land

land_inset <- st_crop(land, c(xmin = 122, ymin = 24, xmax = 146.5, ymax = 46))

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
  "2026/Day16/data/osm_motorway_route.rds"
)

stay_names <- c(
  "Tokyo",
  "Karuizawa",
  "Narai",
  "Kiso-Fukushima",
  "Nagiso",
  "Ena",
  "Kyoto",
  "Nagoya"
)

stays <- night_legs |>
  mutate(lat_r = round(lat, 1), lon_r = round(lon, 1)) |>
  summarise(
    nights = n(),
    lon = first(lon),
    lat = first(lat),
    first_night = min(night),
    .by = c(lat_r, lon_r)
  ) |>
  arrange(first_night) |>
  mutate(
    num = row_number(),
    place = stay_names[num],
    label = if_else(nights > 1, paste0(place, " (", nights, " nights)"), place)
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_plot)

icons <- route |>
  st_transform(crs_plot) |>
  filter(mode == "flight") |>
  mutate(
    icon = "✈",
    geometry = st_cast(st_line_sample(geometry, sample = 0.5), "POINT")
  )

airports <- route |>
  st_transform(crs_plot) |>
  filter(mode == "flight") |>
  st_cast("POINT") |>
  mutate(label = c("Chubu Centrair Airport", "Narita Airport"))

plot_title <- "Walking the Nakasendo Way"

subtitle <- paste0(
  "Barry & Christine's walking tour | 12 - 24 October 2026<br>",
  km_caption
)

library(camcorder)
gg_record(
  device = "png",
  width = 10,
  height = 6.5,
  unit = "in",
  dpi = 300
)

inset <- ggplot() +
  geom_sf(
    data = st_transform(land_inset, crs_plot),
    fill = "#f3ead8",
    colour = "#d9ccae",
    linewidth = 0.1
  ) +
  geom_sf(
    data = st_transform(extent, crs_plot),
    fill = NA,
    colour = "#c0392b",
    linewidth = 0.5
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "#cdd5db",
      colour = "grey40",
      linewidth = 0.3
    )
  )

inset_w <- 0.18 * (bb_main[["xmax"]] - bb_main[["xmin"]])
inset_h <- inset_w * 1.1
inset_xmin <- bb_main[["xmin"]] + 0.02 * (bb_main[["xmax"]] - bb_main[["xmin"]])
inset_ymin <- bb_main[["ymax"]] -
  inset_h -
  0.02 * (bb_main[["xmax"]] - bb_main[["xmin"]])

p <- ggplot() +
  geom_sf(
    #plot map shape
    data = st_transform(land_main, crs_plot),
    fill = "#f3ead8",
    colour = "#d9ccae",
    linewidth = 0.2
  ) +
  geom_sf(
    #plot the roads
    data = st_transform(motorway, crs_plot),
    colour = "#dccfb3",
    linewidth = 0.15
  ) +
  geom_sf(
    data = st_transform(filter(route, mode != "walk"), crs_plot),
    aes(linetype = mode),
    colour = "#1f3557",
    linewidth = 0.9
  ) +
  geom_sf(
    data = st_transform(filter(route, mode == "walk"), crs_plot),
    colour = "#1f3557",
    linewidth = 1.8,
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_sf(
    data = stays,
    shape = 21,
    size = 6,
    stroke = 0.4,
    fill = "#c0392b",
    colour = "#c0392b"
  ) +
  geom_sf_text(
    data = stays,
    aes(label = num),
    colour = "white",
    fontface = "bold",
    size = 8 / .pt
  ) +
  geom_sf_text(
    data = stays,
    aes(label = label),
    colour = "#2b2b2b",
    size = 8 / .pt,
    hjust = 0,
    nudge_x = 14000
  ) +
  geom_sf_text(
    data = icons,
    aes(label = icon),
    family = "Segoe UI Symbol",
    size = 34 / .pt,
    colour = "#1f3557"
  ) +
  geom_sf_text(
    data = filter(airports, label == "Chubu Centrair Airport"),
    aes(label = label),
    colour = "#2b2b2b",
    fontface = "italic",
    size = 8 / .pt,
    hjust = 0,
    nudge_x = 8000,
    nudge_y = -12000
  ) +
  geom_sf_text(
    data = filter(airports, label == "Narita Airport"),
    aes(label = label),
    colour = "#2b2b2b",
    fontface = "italic",
    size = 8 / .pt,
    hjust = 1,
    nudge_y = 14000
  ) +
  scale_linetype_manual(
    values = c(car = "solid", walk = "dotted", flight = "dashed"),
    guide = "none"
  ) +
  annotation_custom(
    ggplotGrob(inset),
    xmin = inset_xmin,
    xmax = inset_xmin + inset_w,
    ymin = inset_ymin,
    ymax = inset_ymin + inset_h
  ) +
  coord_sf(
    xlim = c(bb_main[["xmin"]], bb_main[["xmax"]]),
    ylim = c(bb_main[["ymin"]], bb_main[["ymax"]]),
    expand = FALSE
  ) +
  labs(
    title = plot_title,
    subtitle = subtitle,
    caption = "Motorways: © OpenStreetMap contributors. Land: Natural Earth"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#eef1f3", colour = NA),
    panel.background = element_rect(fill = "#cdd5db", colour = NA),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      family = "Cormorant Garamond",
      colour = "grey20",
      face = "bold",
      size = 28,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_textbox_simple(
      family = "Cormorant Garamond",
      face = "italic",
      colour = "grey40",
      size = 14,
      margin = margin(b = 6)
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
  width = 10,
  height = 6.5,
  dpi = 300
)

elev_file <- "2026/Day16/data/elevation_route.tif"

if (!file.exists(elev_file)) {
  elevatr::get_elev_raster(
    locations = st_sf(geometry = extent),
    z = 9,
    clip = "bbox"
  ) |>
    rast() |>
    writeRaster(elev_file)
}

elev <- rast(elev_file) |>
  project("EPSG:6677") |>
  crop(ext(
    bb_main[["xmin"]],
    bb_main[["xmax"]],
    bb_main[["ymin"]],
    bb_main[["ymax"]]
  )) |>
  clamp(lower = 0)

hill <- shade(
  terrain(elev, "slope", unit = "radians"),
  terrain(elev, "aspect", unit = "radians"),
  angle = 40,
  direction = 315
)

land_vect <- vect(st_transform(land, crs_plot))

elev <- mask(elev, land_vect)
hill <- mask(hill, land_vect)

names(elev) <- "elev"
names(hill) <- "hill"

route_blue <- "#00b4ff"

inset_topo <- ggplot() +
  geom_sf(
    data = st_transform(land_inset, crs_plot),
    fill = "#cfdcb9",
    colour = "#a9b59a",
    linewidth = 0.1
  ) +
  geom_sf(
    data = st_transform(extent, crs_plot),
    fill = NA,
    colour = "#c0392b",
    linewidth = 0.5
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "#d6dde0",
      colour = "grey40",
      linewidth = 0.3
    )
  )

gg_record(
  device = "png",
  width = 10,
  height = 6.5,
  unit = "in",
  dpi = 300
)

p_topo <- ggplot() +
  geom_spatraster(data = elev, aes(fill = elev), maxcell = 5e6) +
  scale_fill_gradientn(
    colours = c("#e4ead3", "#cfdcb9", "#b9c9a1", "#c5c4a3", "#d8d3c0", "#efece3"),
    values = scales::rescale(c(0, 300, 800, 1500, 2300, 3200)),
    limits = c(0, 3200),
    na.value = NA,
    guide = "none"
  ) +
  new_scale_fill() +
  geom_spatraster(
    data = hill,
    aes(fill = hill),
    alpha = 0.45,
    maxcell = 5e6
  ) +
  scale_fill_gradient(
    low = "#2b3b4a",
    high = "white",
    na.value = NA,
    guide = "none"
  ) +
  geom_sf(
    data = st_transform(land, crs_plot),
    fill = NA,
    colour = "#a9b59a",
    linewidth = 0.2
  ) +
  geom_sf(
    data = st_transform(filter(route, mode != "walk"), crs_plot),
    aes(linetype = mode),
    colour = route_blue,
    linewidth = 0.9
  ) +
  geom_sf(
    data = st_transform(filter(route, mode == "walk"), crs_plot),
    colour = route_blue,
    linewidth = 1.8,
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_sf(
    data = stays,
    shape = 21,
    size = 6,
    stroke = 0.4,
    fill = "#c0392b",
    colour = "#c0392b"
  ) +
  geom_sf_text(
    data = stays,
    aes(label = num),
    colour = "white",
    fontface = "bold",
    size = 8 / .pt
  ) +
  geom_sf_label(
    data = stays,
    aes(label = label),
    fill = alpha("#f7f5ee", 0.75),
    colour = "#2b2b2b",
    label.size = 0,
    size = 8 / .pt,
    hjust = 0,
    nudge_x = 14000
  ) +
  geom_sf_text(
    data = icons,
    aes(label = icon),
    family = "Segoe UI Symbol",
    size = 34 / .pt,
    colour = route_blue
  ) +
  geom_sf_label(
    data = filter(airports, label == "Chubu Centrair Airport"),
    aes(label = label),
    fill = alpha("#f7f5ee", 0.75),
    colour = "#2b2b2b",
    fontface = "italic",
    label.size = 0,
    size = 8 / .pt,
    hjust = 0,
    nudge_x = 8000,
    nudge_y = -12000
  ) +
  geom_sf_label(
    data = filter(airports, label == "Narita Airport"),
    aes(label = label),
    fill = alpha("#f7f5ee", 0.75),
    colour = "#2b2b2b",
    fontface = "italic",
    label.size = 0,
    size = 8 / .pt,
    hjust = 1,
    nudge_y = 14000
  ) +
  scale_linetype_manual(
    values = c(car = "solid", walk = "dotted", flight = "dashed"),
    guide = "none"
  ) +
  annotation_custom(
    ggplotGrob(inset_topo),
    xmin = inset_xmin,
    xmax = inset_xmin + inset_w,
    ymin = inset_ymin,
    ymax = inset_ymin + inset_h
  ) +
  coord_sf(
    xlim = c(bb_main[["xmin"]], bb_main[["xmax"]]),
    ylim = c(bb_main[["ymin"]], bb_main[["ymax"]]),
    expand = FALSE
  ) +
  labs(
    title = plot_title,
    subtitle = subtitle,
    caption = "Elevation: Terrain Tiles on AWS (Mapzen/Tilezen). Land: Natural Earth"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#eef1f3", colour = NA),
    panel.background = element_rect(fill = "#d6dde0", colour = NA),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      family = "Cormorant Garamond",
      colour = "grey20",
      face = "bold",
      size = 28,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_textbox_simple(
      family = "Cormorant Garamond",
      face = "italic",
      colour = "grey40",
      size = 14,
      margin = margin(b = 6)
    ),
    plot.caption = element_text(
      colour = "grey40",
      size = 8,
      margin = margin(t = 6, b = 6, r = 6)
    )
  )

p_topo

ggsave(
  "2026/Day16/output/japan_travel_route_topo.png",
  p_topo,
  width = 10,
  height = 6.5,
  dpi = 300
)
