library(tidyverse)
library(sf)
library(osrm)
library(leaflet)
library(fontawesome)
library(htmlwidgets)
library(htmltools)
library(maptiles)
library(tidyterra)
library(shadowtext)

stops_geocoded <- read_csv("data/persuasivethinking/stops_geocoded.csv") |>
  filter(travel_day <= 7)

all_stops_unique <- stops_geocoded |>
  distinct()

stops_sf <- stops_geocoded |>
  st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))

hotel_towns <- tribble(
  ~hotel                   , ~town     ,
  "Tashi Namgay Resort"    , "Paro"    ,
  "Sonam Zhidey Resort"    , "Haa"     ,
  "Norkhil Boutique Hotel" , "Thimphu" ,
  "Dumra Farm Resort"      , "Punakha"
)

all_stops_cat <- all_stops_unique |>
  mutate(
    category = case_when(
      grepl("Airport", place, ignore.case = TRUE) ~ "airport",
      grepl("Resort|Hotel|Ecolodge", place, ignore.case = TRUE) ~ "hotel",
      grepl("Camp", place, ignore.case = TRUE) ~ "camping",
      grepl("River", place, ignore.case = TRUE) ~ "rafting",
      grepl(
        "Dzong|Temple|Monastery|Nunnery|Goenpa|Nest|Chorten",
        place,
        ignore.case = TRUE
      ) ~ "temple",
      grepl("View|Black|Preserve", place, ignore.case = TRUE) ~ "wildlife",
      grepl("Heritage|Festival", place, ignore.case = TRUE) ~ "culture",
      grepl("Pass", place, ignore.case = TRUE) ~ "pass",
      TRUE ~ "other"
    )
  )

get_route <- function(src_coords, dst_coords, route_type) {
  if (route_type == "walking") {
    line <- sf::st_sfc(
      sf::st_linestring(rbind(src_coords, dst_coords)),
      crs = 4326
    )
    dist_km <- as.numeric(sf::st_length(line)) / 1000

    sf::st_sf(
      src = paste(src_coords, collapse = ","),
      dst = paste(dst_coords, collapse = ","),
      duration = NA_real_,
      distance = dist_km,
      geometry = line
    )
  } else {
    osrmRoute(
      src = src_coords,
      dst = dst_coords,
      osrm.profile = "car"
    )
  }
}

routes_geocoded <- stops_sf |>
  mutate(
    origin_place = place,
    destination_place = lead(place),
    dest_travel_day = lead(travel_day),
    src_long = st_coordinates(geometry)[, 1],
    src_lat = st_coordinates(geometry)[, 2],
    dest_long = lead(st_coordinates(geometry)[, 1]),
    dest_lat = lead(st_coordinates(geometry)[, 2])
  ) |>
  filter(!is.na(destination_place)) |>
  rowwise() |>
  mutate(
    route = list(get_route(
      src_coords = c(src_long, src_lat),
      dst_coords = c(dest_long, dest_lat),
      route_type = route_type
    ))
  )

routes_sf <- routes_geocoded |>
  ungroup() |>
  sf::st_drop_geometry() |>
  dplyr::select(-route) |>
  dplyr::bind_cols(
    do.call(rbind, routes_geocoded$route) |>
      sf::st_as_sf() |>
      dplyr::select(geometry)
  ) |>
  sf::st_as_sf()

fa_svg_icon <- function(name, fill = "black", size = 16) {
  url <- sprintf(
    "https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@6.5.1/svgs/solid/%s.svg",
    name
  )
  svg <- paste(readLines(url, warn = FALSE), collapse = "")
  svg_colored <- sub("<svg ", sprintf('<svg fill="%s" ', fill), svg)
  data_uri <- paste0(
    "data:image/svg+xml;utf8,",
    utils::URLencode(svg_colored, reserved = TRUE)
  )
  makeIcon(iconUrl = data_uri, iconWidth = size, iconHeight = size)
}

icons_svg <- iconList(
  airport = fa_svg_icon("plane", "lightgrey"),
  camping = fa_svg_icon("tent", "lightgrey"),
  culture = fa_svg_icon("camera", "lightgrey"),
  rafting = fa_svg_icon("water", "lightgrey"),
  pass = fa_svg_icon("mountain", "lightgrey"),
  temple = fa_svg_icon("vihara", "lightgrey"),
  wildlife = fa_svg_icon("crow", "lightgrey"),
  hotel = fa_svg_icon("building", "lightgrey")
)

route_legend <- tags$div(
  style = paste(
    "background: rgba(250, 250, 250);",
    "color: #1E2019;",
    "border: 2px solid #777;",
    "border-top: none;",
    "padding: 2px 8px 4px;",
    "font-size: 11px;",
    "line-height: 22px;"
  ),
  tags$b("Route type"),
  tags$br(),
  tags$span(
    style = "display:inline-block; width:36px; height:0; border-top:4px solid #AB6AD5; vertical-align:middle; margin-right:6px;"
  ),
  "Driving",
  tags$br(),
  tags$span(
    style = "display:inline-block; width:36px; height:0; border-top:3px dashed #DC5FBD; vertical-align:middle; margin-right:6px;"
  ),
  "Walking"
)

build_day_map <- function(day) {
  day_stops <- all_stops_cat |>
    filter(
      travel_day == day,
      category %in% names(icons_svg),
      category != "hotel"
    )

  day_routes <- routes_sf |>
    filter(dest_travel_day == day)

  day_hotel_places <- unique(c(
    day_routes$origin_place,
    day_routes$destination_place
  ))

  day_towns <- all_stops_cat |>
    filter(category == "hotel", place %in% day_hotel_places) |>
    left_join(hotel_towns, by = c("place" = "hotel")) |>
    rename(hotel = place)

  if (day == 4) {
    day4_place_order <- c(
      "Sonam Zhidey Resort",
      "White Temple",
      "ChuZom-Haa Highway1",
      "ChuZom-Haa Highway2",
      "Tango Monastery"
    )

    day4_waypoints <- all_stops_unique |>
      filter(place %in% day4_place_order) |>
      distinct(place, .keep_all = TRUE) |>
      arrange(match(place, day4_place_order)) |>
      st_as_sf(coords = c("long", "lat"), crs = 4326)

    day4_drive <- osrmRoute(loc = day4_waypoints, osrm.profile = "car") |>
      mutate(route_type = "driving")

    day_routes <- day_routes |>
      filter(route_type != "driving") |>
      bind_rows(day4_drive)
  }

  leaflet() |>
    addProviderTiles("Esri.WorldImagery") |>
    addPolylines(
      data = day_routes |> filter(route_type == "driving"),
      color = "#AB6AD5FF",
      weight = 4,
      opacity = 0.8,
      dashArray = NULL
    ) |>
    addPolylines(
      data = day_routes |> filter(route_type == "walking"),
      color = "#DC5FBDFF",
      weight = 3,
      opacity = 0.8,
      dashArray = "1, 5"
    ) |>
    addMarkers(
      data = day_stops,
      lat = ~lat,
      lng = ~long,
      icon = ~ icons_svg[category],
      label = ~ paste0("Day ", as.integer(travel_day), ": ", place)
    ) |>
    addMarkers(
      data = day_towns,
      lat = ~lat,
      lng = ~long,
      icon = icons_svg[["hotel"]],
      label = ~ paste0(town, ": ", hotel)
    ) |>
    addLabelOnlyMarkers(
      data = day_towns,
      lat = ~lat,
      lng = ~long,
      label = ~town,
      labelOptions = labelOptions(
        noHide = TRUE,
        textOnly = TRUE,
        style = list(
          "font-weight" = "bold",
          "font-size" = "14px",
          "color" = "white"
        )
      )
    ) |>
    addScaleBar(
      position = "bottomleft",
      options = scaleBarOptions(
        maxWidth = 100,
        updateWhenIdle = TRUE,
        imperial = FALSE
      )
    ) |>
    addControl(
      html = route_legend,
      position = "bottomright",
      className = "route-legend"
    ) |>
    htmlwidgets::prependContent(
      tags$style(HTML(".route-legend { padding: 0 !important; }"))
    )
}

day2_map <- build_day_map(2)
day3_map <- build_day_map(3)
day4_map <- build_day_map(4)
day6_map <- build_day_map(6)

day3_routes <- routes_sf |>
  filter(dest_travel_day == 3)

day3_stops <- all_stops_cat |>
  filter(travel_day == 3, category != "hotel")

day3_hotel_places <- unique(c(
  day3_routes$origin_place,
  day3_routes$destination_place
))

day3_towns <- all_stops_cat |>
  filter(category == "hotel", place %in% day3_hotel_places) |>
  left_join(hotel_towns, by = c("place" = "hotel")) |>
  rename(hotel = place)

day3_bbox <- day3_routes |>
  st_union() |>
  st_buffer(3000) |>
  st_bbox()

day3_tiles <- get_tiles(
  x = st_as_sfc(day3_bbox),
  provider = "Esri.WorldImagery",
  crop = TRUE,
  zoom = 11,
  verbose = TRUE
)

day3_static_map <- ggplot() +
  geom_spatraster_rgb(data = day3_tiles) +
  geom_sf(
    data = day3_routes |> filter(route_type == "driving"),
    color = "#AB6AD5FF",
    linewidth = 1.4
  ) +
  geom_sf(
    data = day3_routes |> filter(route_type == "walking"),
    color = "#DC5FBDFF",
    linewidth = 1.2,
    linetype = "dotted"
  ) +
  geom_point(
    data = day3_stops,
    mapping = aes(x = long, y = lat),
    color = "white",
    size = 1.8
  ) +
  geom_shadowtext(
    data = day3_stops,
    mapping = aes(x = long, y = lat, label = place),
    color = "white",
    bg.color = "black",
    size = 3,
    vjust = 1.8
  ) +
  geom_shadowtext(
    data = day3_towns,
    mapping = aes(x = long, y = lat, label = town),
    color = "white",
    bg.color = "black",
    fontface = "bold",
    size = 4.5,
    vjust = -0.6
  ) +
  coord_sf(
    default_crs = st_crs(4326),
    xlim = c(day3_bbox["xmin"], day3_bbox["xmax"]),
    ylim = c(day3_bbox["ymin"], day3_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(legend.position = "none")

day3_static_map
