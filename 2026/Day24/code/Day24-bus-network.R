library(tidyverse)
library(sf)
library(arcgislayers)
library(patchwork)

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

routes_raw <- read_sf(routes_file)

route_changes <- tribble(
  ~status        , ~route                , ~schools                                                                                                    , ~area                  , ~merged_into ,
  "Cancelled"    , "D080206"             , "Palmerston North schools"                                                                                  , "Newbury/Bunnythorpe"  , NA           ,
  "Cancelled"    , "D080222"             , "Palmerston North schools"                                                                                  , "Sanson"               , NA           ,
  "Cancelled"    , "D080224"             , "Palmerston North schools"                                                                                  , "Tangimoana"           , NA           ,
  "Cancelled"    , "D080229"             , "Palmerston North schools"                                                                                  , "Foxton"               , NA           ,
  "Merged"       , "D080204"             , "Awahou School"                                                                                             , "Raumai/Pohangina Valley" , "D080203"  ,
  "Merged"       , "D080211"             , "Palmerston North schools"                                                                                  , "Bainesse"             , "D080221"    ,
  "Redesigned"   , "D080203"             , "Awahou School"                                                                                             , "Pohangina Valley"     , NA           ,
  "Redesigned"   , "D080207"             , "Freyberg High School"                                                                                      , "Hiwinui"              , NA           ,
  "Redesigned"   , "D080208"             , "Awatapu College; St James' Catholic School"                                                                , "Tokomaru"             , NA           ,
  "Redesigned"   , "D080209"             , "Longburn Adventist College; St Peter's College; Palmerston North Adventist Christian School"               , "Shannon"              , NA           ,
  "Redesigned"   , "D080210"             , "St Peter's College"                                                                                        , "Pohangina Village"    , NA           ,
  "Redesigned"   , "D080212"             , "St Peter's College"                                                                                        , "Tokomaru"             , NA           ,
  "Redesigned"   , "D080217"             , "Awatapu College; Palmerston North Intermediate"                                                            , "Aokautere/Turitea"    , NA           ,
  "Redesigned"   , "D080218"             , "Palmerston North Girls' High School"                                                                       , "Forrest Hill Road/Turitea" , NA      ,
  "Redesigned"   , "D080219"             , "Palmerston North Girls' High School"                                                                       , "Polson Hill"          , NA           ,
  "Redesigned"   , "D080221"             , "St Peter's College; Longburn Adventist College"                                                            , "Bainesse/Rongotea"    , NA           ,
  "Redesigned"   , "D080226"             , "Longburn Adventist College"                                                                                , "Ashhurst"             , NA           ,
  "Redesigned"   , "D080227"             , "Awatapu College; Longburn School"                                                                          , "Opiki"                , NA           ,
  "Redesigned"   , "D080228"             , "Palmerston North Girls' High School; Palmerston North Intermediate"                                        , "Tokomaru"             , NA           ,
  "Redesigned"   , "D080230"             , "Longburn Adventist College"                                                                                , "Pacific Drive"        , NA           ,
  "Redesigned"   , "D080231"             , "Longburn Adventist College"                                                                                , "Kelvin Grove"         , NA           ,
  "Temporary"    , "D080205 & D080234"   , "Freyberg High School"                                                                                      , "Ashhurst"             , NA           ,
  "Temporary"    , "D080213 & D080215"   , "Palmerston North Boys' School; Queen Elizabeth College"                                                    , "Ashhurst"             , NA           ,
  "Temporary"    , "D080225"             , "Awatapu College"                                                                                           , "Ashhurst"             , NA           ,
  "Temporary"    , "D080235"             , "St Peter's College"                                                                                        , "Ashhurst"             , NA           ,
  "Temporary"    , "D080236"             , "Palmerston North Girls' High School; Palmerston North Intermediate"                                        , "Ashhurst"             , NA
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

stops_file <- "2026/Day24/data/polson_hill_old_route_stops.csv"

if (!file.exists(stops_file)) {
  tribble(
    ~bus , ~step , ~place                                 , ~label                       ,
    1    ,     1 , "Aokautere School"                     , "Aokautere School"           ,
    1    ,     2 , "Orrs Road"                            , NA                           ,
    1    ,     3 , "Pahiatua Track"                       , NA                           ,
    1    ,     4 , "Polson Hill Drive"                    , NA                           ,
    1    ,     5 , "Aokautere Drive"                      , NA                           ,
    1    ,     6 , "Palmerston North Girls' High School"  , "Girls' High (transfer)"    ,
    2    ,     1 , "Palmerston North Girls' High School"  , "Girls' High (transfer)"    ,
    2    ,     2 , "Awatapu College"                      , "Awatapu College"            ,
    2    ,     3 , "Palmerston North Intermediate Normal School" , "PN Intermediate"    ,
    2    ,     4 , "Palmerston North Boys' High School"   , "Boys' High"                 ,
    3    ,     1 , "Palmerston North Girls' High School"  , "Girls' High (transfer)"    ,
    3    ,     2 , "St Peter's College"                   , "St Peter's College"         ,
    3    ,     3 , "Palmerston North Adventist Christian School" , "Adventist Christian School"
  ) |>
    mutate(query = str_c(place, ", Palmerston North, New Zealand")) |>
    tidygeocoder::geocode(address = query, method = "osm", lat = lat, long = lon) |>
    select(-query) |>
    write_csv(stops_file)
}

stops <- read_csv(stops_file) |>
  arrange(bus, step)

stops

stopifnot(!anyNA(stops$lon))

stops_sf <- stops |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

old_routes_file <- "2026/Day24/data/polson_hill_old_route_osrm.gpkg"

if (!file.exists(old_routes_file)) {
  stops_sf |>
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
      levels = 1:3,
      labels = c(
        "Bus 1: Aokautere, Orrs Road and Polson Hill to Girls' High",
        "Bus 2: Girls' High to Awatapu, PN Intermediate and Boys' High",
        "Bus 3: Girls' High to St Peter's and Adventist school"
      )
    )
  )

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

inset_extent <- c(st_geometry(old_routes), st_geometry(new_route)) |>
  st_union() |>
  st_buffer(1500) |>
  st_bbox()

main_extent <- routes |>
  st_union() |>
  st_buffer(2000) |>
  st_bbox()

osm_pbf <- "2026/Day9/data/new-zealand-261105.osm.pbf"

osm_file <- "2026/Day24/data/pn_osm_basemap_region.gpkg"

main_extent_4326 <- main_extent |>
  st_as_sfc() |>
  st_transform(4326)

if (!file.exists(osm_file)) {
  osmextract::oe_read(
    osm_pbf,
    layer = "lines",
    query = "SELECT highway, geometry FROM lines WHERE highway IN ('motorway','trunk','primary','secondary','tertiary','residential','unclassified')",
    boundary = main_extent_4326,
    boundary_type = "clipsrc"
  ) |>
    st_write(osm_file, layer = "roads")

  osmextract::oe_read(
    osm_pbf,
    layer = "lines",
    extra_tags = "waterway",
    query = "SELECT waterway, geometry FROM lines WHERE waterway IN ('river','stream')",
    boundary = main_extent_4326,
    boundary_type = "clipsrc"
  ) |>
    st_write(osm_file, layer = "waterways")
}

osm_roads <- read_sf(osm_file, layer = "roads") |>
  st_transform(2193)

osm_waterways <- read_sf(osm_file, layer = "waterways") |>
  st_transform(2193)

bus_colours <- c("#0072B2", "#D55E00", "#009E73") |>
  set_names(levels(old_routes$bus))

base_map <- function(extent, road_width, route_width) {
  ggplot() +
    geom_sf(data = osm_waterways, colour = "#bcd4e6", linewidth = 0.3) +
    geom_sf(data = osm_roads, colour = "grey88", linewidth = road_width) +
    geom_sf(data = other_routes, colour = "grey45", linewidth = route_width) +
    coord_sf(
      xlim = extent[c("xmin", "xmax")],
      ylim = extent[c("ymin", "ymax")],
      expand = FALSE
    ) +
    theme_void(base_size = 12)
}

map_main <- base_map(main_extent, 0.15, 0.4) +
  geom_sf(
    data = new_route,
    colour = "#0072B2",
    linewidth = 1.3,
    lineend = "round"
  ) +
  geom_sf(
    data = st_as_sfc(inset_extent),
    fill = NA,
    colour = "grey15",
    linewidth = 0.5
  ) +
  theme(plot.margin = margin(5, 5, 5, 5))

map_before <- base_map(inset_extent, 0.25, 0.4) +
  geom_sf(
    data = old_routes,
    aes(colour = bus),
    linewidth = 1.1,
    lineend = "round"
  ) +
  geom_sf(
    data = school_points,
    shape = 21,
    size = 2.5,
    fill = "white",
    colour = "grey15",
    stroke = 0.9
  ) +
  ggrepel::geom_text_repel(
    data = school_points,
    aes(geometry = geometry, label = label),
    stat = "sf_coordinates",
    size = 2.8,
    colour = "grey15",
    bg.colour = "white",
    bg.r = 0.15,
    min.segment.length = 0,
    seed = 24
  ) +
  scale_colour_manual(values = bus_colours, name = NULL) +
  guides(colour = guide_legend(ncol = 1)) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    plot.background = element_rect(fill = "white", colour = "grey15"),
    plot.margin = margin(4, 4, 4, 4)
  )

map_polson <- map_main +
  inset_element(
    map_before,
    left = 0.5,
    bottom = 0.02,
    right = 0.99,
    top = 0.55,
    align_to = "panel"
  )

map_polson

ggsave(
  "2026/Day24/output/polson_hill_before_after.png",
  map_polson,
  width = 12,
  height = 11,
  dpi = 300,
  bg = "white"
)
