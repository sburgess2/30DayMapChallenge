library(tidyverse)
library(sf)
library(arcgislayers)
library(patchwork)
library(ggtext)
library(sbBrand)
library(camcorder)

routes_url <- "https://services9.arcgis.com/ygJ1AFEQ1sNGJz4H/arcgis/rest/services/ST_All_Bus_Routes_Nov21Update_view/FeatureServer/42"

snapshot_date <- "20261123"

routes_file <- str_c(
  "2026/Day24/data/pn_school_bus_routes_",
  snapshot_date,
  ".gpkg"
)

if (!file.exists(routes_file)) {
  arc_open(routes_url) |>
    arc_select(where = "Route_Number LIKE 'D0802%'") |>
    st_transform(2193) |>
    st_write(routes_file)
}

primary_schools <- c(
  "Rongotea School",
  "Tokomaru School",
  "Awahou School",
  "Ashhurst School",
  "Cornerstone Christian School"
)

routes_raw <- read_sf(routes_file) |>
  filter(!Head_School %in% primary_schools)

route_changes <- tribble(
  ~status      , ~route              , ~schools                                                                                      , ~area                       , ~merged_into ,
  "Cancelled"  , "D080206"           , "Palmerston North schools"                                                                    , "Newbury/Bunnythorpe"       , NA           ,
  "Cancelled"  , "D080222"           , "Palmerston North schools"                                                                    , "Sanson"                    , NA           ,
  "Cancelled"  , "D080224"           , "Palmerston North schools"                                                                    , "Tangimoana"                , NA           ,
  "Cancelled"  , "D080229"           , "Palmerston North schools"                                                                    , "Foxton"                    , NA           ,
  "Merged"     , "D080204"           , "Awahou School"                                                                               , "Raumai/Pohangina Valley"   , "D080203"    ,
  "Merged"     , "D080211"           , "Palmerston North schools"                                                                    , "Bainesse"                  , "D080221"    ,
  "Redesigned" , "D080203"           , "Awahou School"                                                                               , "Pohangina Valley"          , NA           ,
  "Redesigned" , "D080207"           , "Freyberg High School"                                                                        , "Hiwinui"                   , NA           ,
  "Redesigned" , "D080208"           , "Awatapu College; St James' Catholic School"                                                  , "Tokomaru"                  , NA           ,
  "Redesigned" , "D080209"           , "Longburn Adventist College; St Peter's College; Palmerston North Adventist Christian School" , "Shannon"                   , NA           ,
  "Redesigned" , "D080210"           , "St Peter's College"                                                                          , "Pohangina Village"         , NA           ,
  "Redesigned" , "D080212"           , "St Peter's College"                                                                          , "Tokomaru"                  , NA           ,
  "Redesigned" , "D080217"           , "Awatapu College; Palmerston North Intermediate"                                              , "Aokautere/Turitea"         , NA           ,
  "Redesigned" , "D080218"           , "Palmerston North Girls' High School"                                                         , "Forrest Hill Road/Turitea" , NA           ,
  "Redesigned" , "D080219"           , "Palmerston North Girls' High School"                                                         , "Polson Hill"               , NA           ,
  "Redesigned" , "D080221"           , "St Peter's College; Longburn Adventist College"                                              , "Bainesse/Rongotea"         , NA           ,
  "Redesigned" , "D080226"           , "Longburn Adventist College"                                                                  , "Ashhurst"                  , NA           ,
  "Redesigned" , "D080227"           , "Awatapu College; Longburn School"                                                            , "Opiki"                     , NA           ,
  "Redesigned" , "D080228"           , "Palmerston North Girls' High School; Palmerston North Intermediate"                          , "Tokomaru"                  , NA           ,
  "Redesigned" , "D080230"           , "Longburn Adventist College"                                                                  , "Pacific Drive"             , NA           ,
  "Redesigned" , "D080231"           , "Longburn Adventist College"                                                                  , "Kelvin Grove"              , NA           ,
  "Temporary"  , "D080205 & D080234" , "Freyberg High School"                                                                        , "Ashhurst"                  , NA           ,
  "Temporary"  , "D080213 & D080215" , "Palmerston North Boys' School; Queen Elizabeth College"                                      , "Ashhurst"                  , NA           ,
  "Temporary"  , "D080225"           , "Awatapu College"                                                                             , "Ashhurst"                  , NA           ,
  "Temporary"  , "D080235"           , "St Peter's College"                                                                          , "Ashhurst"                  , NA           ,
  "Temporary"  , "D080236"           , "Palmerston North Girls' High School; Palmerston North Intermediate"                          , "Ashhurst"                  , NA
)

write_csv(
  route_changes,
  "2026/Day24/data/pn_school_bus_changes_term1_2026.csv",
  na = ""
)

routes_per_area <- route_changes |>
  filter(status %in% c("Redesigned", "Temporary")) |>
  mutate(area = str_replace(area, "Aokautere/Turitea", "Turitea")) |>
  summarise(
    n_services = n(),
    n_schools = n_distinct(unlist(str_split(schools, "; "))),
    .by = area
  ) |>
  arrange(desc(n_services))

routes_per_area

changes_plot <- routes_per_area |>
  filter(n_services > 1) |>
  mutate(area = fct_reorder(area, n_services)) |>
  ggplot(aes(n_services, area)) +
  geom_col(fill = "#0072B2", width = 0.6) +
  geom_text(aes(label = n_services), hjust = -0.4, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "School-specific bus services from the area, Term 1 2026",
    y = NULL,
    caption = "Source: Ministry of Education, Palmerston North area school bus changes effective Term 1, 2026."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, colour = "grey30", size = 9)
  )

changes_plot

ggsave(
  "2026/Day24/output/pn_bus_services_by_area.png",
  changes_plot,
  width = 8,
  height = 4.5,
  dpi = 300,
  bg = "white"
)

routes <- routes_raw |>
  group_by(Route_Number) |>
  summarise(do_union = TRUE, .groups = "drop")

routes_raw |>
  st_drop_geometry() |>
  distinct(
    Route_Number,
    Head_School,
    Schools_on_Route,
    Patronage,
    Total_Kilometers
  ) |>
  arrange(Route_Number) |>
  print(n = Inf)

routes |>
  ggplot() +
  geom_sf(aes(colour = Route_Number)) +
  geom_sf_text(aes(label = Route_Number), size = 3, check_overlap = TRUE) +
  theme_void() +
  theme(legend.position = "none")

stops_file <- "2026/Day24/data/polson_hill_old_route_stops.csv"

if (!file.exists(stops_file)) {
  tribble(
    ~bus , ~step , ~place                                        , ~label                       ,
       1 ,     1 , "Aokautere School"                            , "Aokautere School"           ,
       1 ,     2 , "Orrs Road"                                   , NA                           ,
       1 ,     3 , "Pahiatua Track"                              , NA                           ,
       1 ,     4 , "Polson Hill Drive"                           , NA                           ,
       1 ,     5 , "Aokautere Drive"                             , NA                           ,
       1 ,     6 , "Palmerston North Girls' High School"         , "Girls' High (transfer)"     ,
       2 ,     1 , "Palmerston North Girls' High School"         , "Girls' High (transfer)"     ,
       2 ,     2 , "Awatapu College"                             , "Awatapu College"            ,
       2 ,     3 , "Palmerston North Intermediate Normal School" , "PN Intermediate"            ,
       2 ,     4 , "Palmerston North Boys' High School"          , "Boys' High"                 ,
       3 ,     1 , "Palmerston North Girls' High School"         , "Girls' High (transfer)"     ,
       3 ,     2 , "St Peter's College"                          , "St Peter's College"         ,
       3 ,     3 , "Longburn Adventist College"                  , "Longburn Adventist College"
  ) |>
    mutate(query = str_c(place, ", Palmerston North, New Zealand")) |>
    tidygeocoder::geocode(
      address = query,
      method = "osm",
      lat = lat,
      long = lon
    ) |>
    select(-query) |>
    write_csv(stops_file)
}

stops <- read_csv(stops_file) |>
  mutate(
    is_longburn = bus == 3 & step == 3,
    is_boys_high = label == "Boys' High",
    is_pahiatua = place == "Pahiatua Track",
    is_polson = place == "Polson Hill Drive",
    place = if_else(is_longburn, "Longburn Adventist College", place),
    label = if_else(is_longburn, "Longburn Adventist", label),
    lat = case_when(
      is_longburn ~ -40.388877,
      is_boys_high ~ -40.3465,
      is_pahiatua ~ -40.3763,
      is_polson ~ -40.3808656,
      .default = lat
    ),
    lon = case_when(
      is_longburn ~ 175.558319,
      is_boys_high ~ 175.6084,
      is_pahiatua ~ 175.6662,
      is_polson ~ 175.665972,
      .default = lon
    )
  ) |>
  select(-is_longburn, -is_boys_high, -is_pahiatua, -is_polson) |>
  bind_rows(
    tibble(
      bus = 1,
      step = 3.5,
      place = "The Bush Track",
      label = NA_character_,
      lat = -40.3781,
      lon = 175.6634
    )
  ) |>
  arrange(bus, step)

boys_high_stop <- filter(stops, bus == 2, label == "Boys' High")
st_peters_stop <- filter(stops, bus == 3, label == "St Peter's College")
longburn_stop <- filter(stops, bus == 3, label == "Longburn Adventist")

stops <- bind_rows(
  filter(stops, bus %in% c(1, 2)),
  mutate(boys_high_stop, bus = 3, step = 1),
  mutate(st_peters_stop, bus = 3, step = 2),
  mutate(boys_high_stop, bus = 4, step = 1),
  mutate(longburn_stop, bus = 4, step = 2)
)

stops

stopifnot(!anyNA(stops$lon))

stops_sf <- stops |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

loop_start <- routes_raw |>
  filter(Route_Number == "D080217") |>
  slice(1) |>
  st_geometry() |>
  st_transform(4326) |>
  st_coordinates() |>
  (\(m) m[1, 1:2])()

loop_start_sf <- st_sf(
  bus = 1,
  step = 3,
  geometry = st_sfc(st_point(loop_start), crs = 4326)
)

route_stops_sf <- stops_sf |>
  filter(!(bus == 1 & step > 2)) |>
  bind_rows(loop_start_sf) |>
  arrange(bus, step)

old_routes_file <- "2026/Day24/data/polson_hill_old_route_osrm_orrs.gpkg"

if (!file.exists(old_routes_file)) {
  route_stops_sf |>
    group_by(bus) |>
    group_map(\(d, k) {
      osrm::osrmRoute(loc = d) |>
        mutate(bus = k$bus)
    }) |>
    bind_rows() |>
    st_as_sf() |>
    st_write(old_routes_file)
}

old_routes <- read_sf(old_routes_file) |>
  st_transform(2193) |>
  mutate(
    bus = factor(
      bus,
      levels = 1:4,
      labels = c(
        "Bus 1: Polson Hill to Girls' High",
        "Bus 2: Girls' High to Awatapu, PN Intermediate, Boys' High",
        "Bus 3: Boys' High to St Peter's",
        "Bus 4: Boys' High to Longburn Adventist"
      )
    )
  )

loop_box <- st_bbox(
  c(xmin = 175.647, xmax = 175.668, ymin = -40.380, ymax = -40.368),
  crs = 4326
) |>
  st_as_sfc() |>
  st_transform(2193)

loop_parts <- routes_raw |>
  filter(Route_Number == "D080212", str_squish(AM_PM) == "AM") |>
  slice(1) |>
  st_geometry() |>
  st_transform(2193) |>
  st_cast("LINESTRING")

loop_parts <- loop_parts[st_within(loop_parts, loop_box, sparse = FALSE)[, 1]]

shared_xy <- routes_raw |>
  filter(Route_Number == "D080217") |>
  slice(1) |>
  st_geometry() |>
  st_transform(2193) |>
  st_coordinates() |>
  (\(m) m[, 1:2])()

girls_high_xy <- stops_sf |>
  filter(label == "Girls' High (transfer)") |>
  slice(1) |>
  st_transform(2193) |>
  st_coordinates()

girls_high_index <- which.min(
  (shared_xy[, 1] - girls_high_xy[1, 1])^2 +
    (shared_xy[, 2] - girls_high_xy[1, 2])^2
)

bus_1_row <- which(as.integer(old_routes$bus) == 1)

orrs_xy <- st_coordinates(old_routes[bus_1_row, ])[, 1:2]

bus_1_line <- st_sfc(
  st_linestring(rbind(orrs_xy, shared_xy[seq_len(girls_high_index), ])),
  crs = 2193
)

st_geometry(old_routes)[bus_1_row] <- st_union(c(bus_1_line, loop_parts))

new_route <- routes_raw |>
  filter(Route_Number == "D080217") |>
  st_union() |>
  st_sf(route = "D080217", geometry = _) |>
  st_transform(2193)

other_routes <- routes |>
  filter(Route_Number != "D080217")

school_points <- stops_sf |>
  filter(!is.na(label)) |>
  distinct(label, .keep_all = TRUE) |>
  st_transform(2193)

west <- 175.545
east <- 175.730

south <- -40.410
north <- -40.322

map_box <- st_bbox(
  c(xmin = west, xmax = east, ymin = south, ymax = north),
  crs = 4326
) |>
  st_as_sfc()

map_extent <- map_box |>
  st_transform(2193) |>
  st_bbox()

osm_pbf <- "2026/Day9/data/new-zealand-261105.osm.pbf"

osm_file <- "2026/Day24/data/pn_osm_basemap_longburn_ashhurst.gpkg"

if (!file.exists(osm_file)) {
  osmextract::oe_read(
    osm_pbf,
    layer = "lines",
    query = "SELECT highway, geometry FROM lines WHERE highway IN ('motorway','trunk','primary','secondary','tertiary','residential','unclassified')",
    boundary = map_box,
    boundary_type = "clipsrc"
  ) |>
    st_write(osm_file, layer = "roads")

  osmextract::oe_read(
    osm_pbf,
    layer = "lines",
    extra_tags = "waterway",
    query = "SELECT waterway, geometry FROM lines WHERE waterway IN ('river','stream')",
    boundary = map_box,
    boundary_type = "clipsrc"
  ) |>
    st_write(osm_file, layer = "waterways")
}

osm_roads <- read_sf(osm_file, layer = "roads") |>
  st_transform(2193)

osm_waterways <- read_sf(osm_file, layer = "waterways") |>
  st_transform(2193)

new_bus_routes <- tribble(
  ~bus                          , ~route    ,
  "Awatapu and PN Intermediate" , "D080217" ,
  "St Peter's"                  , "D080212" ,
  "Girls' High"                 , "D080218" ,
  "Longburn Adventist"          , "D080230"
)

new_bus_levels <- new_bus_routes$bus

new_buses <- routes_raw |>
  mutate(AM_PM = str_squish(AM_PM)) |>
  inner_join(new_bus_routes, by = c("Route_Number" = "route")) |>
  filter(AM_PM %in% c("AM", "AM/PM")) |>
  group_by(bus) |>
  filter(AM_PM == if (any(AM_PM == "AM")) "AM" else "AM/PM") |>
  summarise(do_union = TRUE, .groups = "drop") |>
  mutate(bus = factor(bus, levels = new_bus_levels)) |>
  arrange(bus) |>
  st_transform(2193)

new_buses |>
  st_drop_geometry() |>
  mutate(km = as.numeric(st_length(new_buses)) / 1000)

new_bus_colours <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73") |>
  set_names(new_bus_levels)

bus_colours <- c("black", "#CC79A7", "#E69F00", "#009E73") |>
  set_names(levels(old_routes$bus))

bus_gap <- 0.0055 * unname(map_extent["xmax"] - map_extent["xmin"])

new_shifts <- c(1, 0, -1, 0) * bus_gap

shifted_lines <- map2(
  st_geometry(new_buses),
  new_shifts,
  \(line, shift) line + c(shift, -shift)
)

st_geometry(new_buses) <- st_sfc(shifted_lines, crs = st_crs(new_buses))

crop_to_map <- function(x) {
  x <- suppressWarnings(st_crop(x, map_extent))
  x[!st_is_empty(x), ]
}

osm_roads <- crop_to_map(osm_roads)
osm_waterways <- crop_to_map(osm_waterways)
old_routes <- crop_to_map(old_routes)
new_buses <- crop_to_map(new_buses)
school_points <- crop_to_map(school_points)

systemfonts::require_font("Lato")
systemfonts::require_font("Baloo 2")

font <- "Lato"
text_col <- "#3b3b3b"
bg_col <- "white"

plot_title <- "Three school buses now run along Polson Hill Drive, where one bus used to service five schools"

subtitle <- "Morning school bus routes from Aokautere to Palmerston North intermediate and high schools, before and after the Term 1 2026 changes. From 2026 there was no longer an Aokautere bus for Boys High School and Longburn Adventist College."

social_caption <- sb_social_caption(
  data_source = "Ministry of Education school bus routes and route changes notice (Term 1 2026); OpenStreetMap contributors. Before routes are approximate."
)

label_points <- function(x) {
  xy <- st_coordinates(suppressWarnings(st_point_on_surface(x)))

  x |>
    st_drop_geometry() |>
    mutate(x = xy[, 1], y = xy[, 2])
}

school_points_before <- school_points |>
  mutate(label = if_else(label == "Boys' High", "Boys' High (transfer)", label))

school_points_now <- school_points |>
  mutate(label = str_remove(label, fixed(" (transfer)")))

polson_xy <- stops_sf |>
  filter(bus == 1, step == 4) |>
  st_transform(2193) |>
  st_coordinates()

polson_label <- ggrepel::geom_text_repel(
  data = tibble(
    x = polson_xy[1, 1],
    y = polson_xy[1, 2],
    label = "Polson Hill Drive"
  ),
  aes(x, y, label = label),
  family = font,
  fontface = "bold",
  size = 8 / .pt,
  colour = "black",
  bg.colour = "white",
  bg.r = 0.2,
  nudge_y = -500,
  min.segment.length = 0,
  seed = 24
)

bus_count_label <- function(count) {
  annotate(
    "text",
    x = polson_xy[1, 1],
    y = polson_xy[1, 2] - 1400,
    label = count,
    family = font,
    fontface = "bold",
    colour = "black",
    size = 16 / .pt
  )
}

base_map <- function() {
  ggplot() +
    geom_sf(data = osm_waterways, colour = "#bcd4e6", linewidth = 0.3) +
    geom_sf(data = osm_roads, colour = "grey88", linewidth = 0.25) +
    coord_sf(
      xlim = as.numeric(map_extent[c("xmin", "xmax")]),
      ylim = as.numeric(map_extent[c("ymin", "ymax")]),
      expand = FALSE
    ) +
    theme_void(base_family = font, base_size = 10) +
    theme(
      legend.position = "none",
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 10, 10),
      plot.title = ggtext::element_textbox_simple(
        family = font,
        colour = text_col,
        face = "bold",
        size = 12,
        lineheight = 1.15,
        margin = margin(b = 10)
      ),
      panel.background = element_rect(fill = bg_col, colour = NA),
      plot.background = element_rect(fill = bg_col, colour = NA),
      panel.border = element_rect(fill = NA, colour = "grey15", linewidth = 1)
    )
}

label_colours <- c(
  pink = "#CC79A7",
  green = "#009E73",
  yellow = "#E69F00",
  blue = "#56B4E9",
  other = text_col
)

school_groups <- tribble(
  ~label                   , ~group   ,
  "Awatapu College"        , "pink"   ,
  "PN Intermediate"        , "pink"   ,
  "Boys' High (transfer)"  , "pink"   ,
  "Longburn Adventist"     , "green"  ,
  "St Peter's College"     , "yellow" ,
  "Girls' High"            , "blue"   ,
  "Girls' High (transfer)" , "blue"
)

school_layers <- function(points, grey_labels = character()) {
  points <- points |>
    left_join(school_groups, by = "label") |>
    mutate(
      group = replace_na(group, "other"),
      group = if_else(label %in% grey_labels, "other", group),
      nudge_x = if_else(label == "Girls' High (transfer)", 400, 0),
      nudge_y = if_else(label == "Girls' High (transfer)", 400, 0)
    )

  list(
    geom_sf(
      data = points,
      aes(colour = group),
      shape = 21,
      size = 2.5,
      fill = "white",
      stroke = 1.2
    ),
    ggrepel::geom_text_repel(
      data = points,
      aes(geometry = geometry, label = label, colour = group),
      stat = "sf_coordinates",
      family = font,
      fontface = "bold",
      size = 8 / .pt,
      bg.colour = "white",
      bg.r = 0.15,
      nudge_x = points$nudge_x,
      nudge_y = points$nudge_y,
      min.segment.length = 0,
      seed = 24
    )
  )
}

gg_record(
  device = "png",
  width = 14,
  height = 9,
  unit = "in",
  dpi = 300
)

map_before <- base_map() +
  geom_sf(
    data = old_routes,
    aes(colour = bus),
    linewidth = 1.2,
    lineend = "round"
  ) +
  school_layers(school_points_before) +
  polson_label +
  bus_count_label("1 bus") +
  scale_colour_manual(values = c(bus_colours, label_colours)) +
  labs(
    title = "Before: one bus along Polson Hill, with transfers at Girls High and Boys High School."
  )

map_after <- base_map() +
  geom_sf(
    data = new_buses,
    aes(colour = bus),
    linewidth = 1.2,
    lineend = "round"
  ) +
  school_layers(
    school_points_now,
    grey_labels = c("Boys' High", "Longburn Adventist")
  ) +
  polson_label +
  bus_count_label("3 buses") +
  scale_colour_manual(values = c(new_bus_colours, label_colours)) +
  labs(title = "Now: three buses along Polson Hill Drive")

polson_annotation <- plot_annotation(
  title = plot_title,
  subtitle = subtitle,
  caption = social_caption,
  theme = theme(
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      family = font,
      colour = text_col,
      face = "bold",
      size = 16,
      lineheight = 1.15,
      margin = margin(b = 10)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font,
      colour = text_col,
      size = 11,
      lineheight = 1.2,
      margin = margin(b = 14)
    ),
    plot.caption = ggtext::element_textbox_simple(
      family = "Baloo 2",
      colour = text_col,
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = bg_col, colour = NA)
  )
)

map_polson <- (map_before + map_after) + polson_annotation

map_polson
gg_stop_recording()
ggsave(
  "2026/Day24/output/polson_hill_before_after.png",
  map_polson,
  width = 14,
  height = 9,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

south_portrait <- -40.410

portrait_extent <- st_bbox(
  c(xmin = west, xmax = east, ymin = south_portrait, ymax = north),
  crs = 4326
) |>
  st_as_sfc() |>
  st_transform(2193) |>
  st_bbox()

portrait_limits <- coord_sf(
  xlim = as.numeric(portrait_extent[c("xmin", "xmax")]),
  ylim = as.numeric(portrait_extent[c("ymin", "ymax")]),
  expand = FALSE
)

panel_label <- function(label) {
  annotate(
    "label",
    x = portrait_extent[["xmin"]] + 400,
    y = portrait_extent[["ymax"]] - 400,
    label = label,
    hjust = 0,
    vjust = 1,
    family = font,
    fontface = "bold",
    colour = text_col,
    fill = "white",
    linewidth = 0,
    size = 9 / .pt
  )
}

map_before_portrait <- map_before +
  portrait_limits +
  panel_label("Before: one bus along Polson Hill") +
  labs(title = NULL)

map_after_portrait <- map_after +
  portrait_limits +
  panel_label("Now: three buses along Polson Hill Drive") +
  labs(title = NULL)

subtitle_portrait <- "Morning school bus routes from Aokautere to Palmerston North intermediate and high schools, before and after the Term 1 2026 changes."

map_polson_portrait <- (map_before_portrait / map_after_portrait) &
  theme(
    plot.margin = margin(4, 6, 4, 6),
    plot.title = ggtext::element_textbox_simple(
      family = font,
      colour = text_col,
      face = "bold",
      size = 9,
      lineheight = 1.1,
      margin = margin(b = 3)
    )
  )

map_polson_portrait <- map_polson_portrait +
  plot_annotation(
    title = plot_title,
    subtitle = subtitle_portrait,
    caption = social_caption,
    theme = theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(
        family = font,
        colour = text_col,
        face = "bold",
        size = 10,
        lineheight = 1.1,
        margin = margin(b = 10)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        family = font,
        colour = text_col,
        size = 8.5,
        lineheight = 1,
        margin = margin(b = 10)
      ),
      plot.caption = ggtext::element_textbox_simple(
        family = "Baloo 2",
        colour = text_col,
        size = 8,
        hjust = 0,
        margin = margin(t = 4)
      ),
      plot.margin = margin(8, 8, 8, 8),
      plot.background = element_rect(fill = bg_col, colour = NA)
    )
  )

gg_record(
  device = "png",
  width = 8,
  height = 9,
  unit = "in",
  dpi = 300
)

map_polson_portrait

gg_stop_recording()
ggsave(
  "2026/Day24/output/polson_hill_before_after_portrait.png",
  map_polson_portrait,
  width = 8,
  height = 9,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)
