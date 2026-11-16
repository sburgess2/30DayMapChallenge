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

sysfonts::font_add_google(
  "Cormorant Garamond",
  "cormorant_lab",
  regular.wt = 600,
  bold.wt = 700
)
sysfonts::font_add_google("Cormorant Garamond", "Cormorant Garamond")
sysfonts::font_add("Segoe UI Symbol", "seguisym.ttf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

col_marker1 <- "#c0392b"
col_number <- "white"
col_text <- "#2b2b2b"
col_text_light <- "grey40"
col_title <- "grey20"
col_page <- "#eef1f3"
col_inset_border <- "grey40"
col_cream <- "#f7f5ee"

col_land <- "#f3ead8"
col_coast <- "#d9ccae"
col_sea <- "#cdd5db"
col_road <- "#dccfb3"
col_route <- "#1f3557"

col_indigo <- "#1F3A5F"
col_route_topo <- col_indigo
col_label_text <- "#142640"
col_halo <- "#FBF6EC"
col_marker2 <- "#D4A72C"
col_sea_topo <- "#d6dde0"
col_coast_topo <- "#a9b59a"
col_land_inset_topo <- "#cfdcb9"
col_relief <- c(
  "#e4ead3",
  "#cfdcb9",
  "#b9c9a1",
  "#c5c4a3",
  "#d8d3c0",
  "#efece3"
)
col_hill_low <- "#2b3b4a"
col_hill_high <- "white"

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

nakasendo_days <- ymd(c("2026-10-15", "2026-10-18"))

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
  filter(mode != "walk" | between(day, nakasendo_days[1], nakasendo_days[2])) |>
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

map_ratio <- 1.75

bb_half_h <- max(
  (bb_main[["xmax"]] - bb_main[["xmin"]]) / map_ratio,
  bb_main[["ymax"]] - bb_main[["ymin"]]
) /
  2

bb_mid_y <- (bb_main[["ymin"]] + bb_main[["ymax"]]) / 2

bb_main <- st_bbox(
  c(
    xmin = bb_main[["xmin"]],
    ymin = bb_mid_y - bb_half_h,
    xmax = bb_main[["xmax"]],
    ymax = bb_mid_y + bb_half_h
  ),
  crs = st_crs(crs_plot)
)

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

route_proj <- st_transform(route, crs_plot)

route_walk <- filter(
  route_proj,
  mode == "walk",
  between(day, nakasendo_days[1], nakasendo_days[2])
)

walk_zone <- route_walk |>
  st_union() |>
  st_buffer(500)

route_car <- route_proj |>
  filter(mode == "car") |>
  st_difference(walk_zone) |>
  filter(!st_is_empty(geometry))

route_other <- bind_rows(route_car, filter(route_proj, mode == "flight"))

route_topo <- mutate(route_proj, mode = if_else(mode == "walk", "car", mode))

plot_title <- "Walking the Nakasendo Way"

subtitle <- paste0(
  "Barry & Christine's walking tour | 12 - 24 October 2026<br>",
  km_caption
)

library(camcorder)
gg_record(
  device = "png",
  width = 11.69,
  height = 8.27,
  unit = "in",
  dpi = 300
)

inset <- ggplot() +
  geom_sf(
    data = st_transform(land_inset, crs_plot),
    fill = col_land,
    colour = col_coast,
    linewidth = 0.1
  ) +
  geom_sf(
    data = st_transform(extent, crs_plot),
    fill = NA,
    colour = col_marker1,
    linewidth = 0.5
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = col_sea,
      colour = col_inset_border,
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
    fill = col_land,
    colour = col_coast,
    linewidth = 0.2
  ) +
  geom_sf(
    #plot the roads
    data = st_transform(motorway, crs_plot),
    colour = col_road,
    linewidth = 0.15
  ) +
  geom_sf(
    data = route_other,
    aes(linetype = mode),
    colour = col_route,
    linewidth = 0.9
  ) +
  geom_sf(
    data = route_walk,
    colour = col_route,
    linewidth = 2.6,
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_sf(
    data = stays,
    shape = 21,
    size = 6,
    stroke = 0.4,
    fill = col_marker1,
    colour = col_marker1
  ) +
  geom_sf_text(
    data = stays,
    aes(label = num),
    colour = col_number,
    fontface = "bold",
    size = 8 / .pt
  ) +
  geom_sf_text(
    data = stays,
    aes(label = label),
    colour = col_text,
    size = 8 / .pt,
    hjust = 0,
    nudge_x = 14000
  ) +
  geom_sf_text(
    data = icons,
    aes(label = icon),
    family = "Segoe UI Symbol",
    size = 34 / .pt,
    colour = col_route
  ) +
  geom_sf_text(
    data = filter(airports, label == "Chubu Centrair Airport"),
    aes(label = label),
    colour = col_text,
    fontface = "italic",
    size = 8 / .pt,
    hjust = 0,
    nudge_x = 8000,
    nudge_y = -12000
  ) +
  geom_sf_text(
    data = filter(airports, label == "Narita Airport"),
    aes(label = label),
    colour = col_text,
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
    plot.background = element_rect(fill = col_page, colour = NA),
    panel.background = element_rect(fill = col_sea, colour = NA),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      family = "Cormorant Garamond",
      colour = col_title,
      face = "bold",
      size = 28,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_textbox_simple(
      family = "Cormorant Garamond",
      face = "italic",
      colour = col_text_light,
      size = 14,
      margin = margin(b = 6)
    ),
    plot.caption = element_text(
      colour = col_text_light,
      size = 8,
      margin = margin(t = 6, b = 6, r = 6)
    )
  )

p

#Other colours

record_polaroid()

ggsave(
  "2026/Day16/output/japan_travel_route.png",
  p,
  width = 11.69,
  height = 8.27,
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

inset_topo <- ggplot() +
  geom_sf(
    data = st_transform(land_inset, crs_plot),
    fill = col_land_inset_topo,
    colour = col_coast_topo,
    linewidth = 0.1
  ) +
  geom_sf(
    data = st_transform(extent, crs_plot),
    fill = NA,
    colour = col_indigo,
    linewidth = 0.3
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = col_sea_topo,
      colour = col_indigo,
      linewidth = 0.3
    )
  )

stays_xy <- st_coordinates(stays)
airports_xy <- st_coordinates(airports)

labels_topo <- bind_rows(
  tibble(
    text = stays$place,
    x = stays_xy[, "X"] + 4000,
    y = stays_xy[, "Y"],
    hjust = 0
  ),
  tibble(
    text = airports$label,
    x = airports_xy[, "X"] + c(8000, 0),
    y = airports_xy[, "Y"] + c(-12000, 14000),
    hjust = c(0, 1)
  )
) |>
  st_as_sf(coords = c("x", "y"), crs = crs_plot)

gg_record(
  device = "png",
  width = 11.69,
  height = 8.27,
  unit = "in",
  dpi = 300
)

p_topo <- ggplot() +
  geom_spatraster(data = elev, aes(fill = elev), maxcell = 5e6) +
  scale_fill_gradientn(
    colours = col_relief,
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
    low = col_hill_low,
    high = col_hill_high,
    na.value = NA,
    guide = "none"
  ) +
  geom_sf(
    data = st_transform(land, crs_plot),
    fill = NA,
    colour = col_coast_topo,
    linewidth = 0.2
  ) +
  geom_sf(
    data = route_topo,
    aes(linetype = mode),
    colour = col_route_topo,
    linewidth = 0.9
  ) +
  shadowtext::geom_shadowtext(
    data = labels_topo,
    aes(geometry = geometry, label = text, hjust = hjust),
    stat = "sf_coordinates",
    family = "cormorant_lab",
    fontface = "bold.italic",
    size = 14 / .pt,
    colour = col_label_text,
    bg.colour = col_halo,
    bg.r = 0.18
  ) +
  geom_sf_text(
    data = icons,
    aes(label = icon),
    family = "Segoe UI Symbol",
    size = 34 / .pt,
    colour = col_route_topo,
    nudge_y = -9000
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
    plot.background = element_rect(fill = col_page, colour = NA),
    panel.background = element_rect(fill = col_sea_topo, colour = NA),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      family = "Cormorant Garamond",
      colour = col_title,
      face = "bold",
      size = 28,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_textbox_simple(
      family = "Cormorant Garamond",
      face = "italic",
      colour = col_text_light,
      size = 14,
      margin = margin(b = 6)
    ),
    plot.caption = element_text(
      colour = col_text_light,
      size = 8,
      margin = margin(t = 6, b = 6, r = 6)
    )
  )

p_topo
record_polaroid()
ggsave(
  "2026/Day16/output/japan_travel_route_topo.png",
  p_topo,
  width = 11.69,
  height = 8.27,
  dpi = 300
)
