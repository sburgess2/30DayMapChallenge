library(tidyverse)
library(sf)
library(osmdata)

getbb("Tucson")

streets <- getbb("washington dc") |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c("motorway", "primary", "secondary", "tertiary")
  )

thimphu_stops <- read_csv("2026/Day21/data/thimphu_stops_geocoded.csv")

thimphu_bbox <- thimphu_stops |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> #turns CSV rows into spatial points. crs = 4326 says the numbers are ordinary longitude and latitude (WGS84).
  st_buffer(1000) |> #draws a 1,000 m circle around each point so the map has a margin and no point sits on the edge
  st_bbox() #returns the smallest rectangle (xmin, ymin, xmax, ymax) containing all the buffers

#An Overpass query is a request you send to the Overpass API.
#That's a read-only search service for the OpenStreetMap database.
#OpenStreetMap holds the whole world's map data, which is far too large to download.
# Overpass lets you ask for just the part you want, such as every road inside a rectangle, and it sends back only that.

thimphu_streets <- opq(bbox = unname(thimphu_bbox)) |> #starts an Overpass query for that rectangle; unname() because opq wants a plain vector of four numbers
  add_osm_feature(
    key = "highway", #the OpenStreetMap tag used for all roads and paths
    value = c(
      "motorway", #the road types to keep, from the largest roads
      "trunk"
    ), #The most important roads
    #"primary", #Major roads linking large towns
    #"secondary", #Roads linking smaller towns
    #"tertiary", #Roads linking viallages
    #"unclassified", #Minor public roads that aren't residential
    #"residential", #Streets in housing area
    #"living_street", #Residential streets where pedestrians have priority
    #"service" #down to the smallest, such as car parks and driveways
  ) |>
  osmdata_sf() |> #runs the query and returns the result as sf objects (points, lines, polygons)
  pluck("osm_lines") #keeps only the lines, which is where the streets are

thimphu_map <- ggplot() +
  geom_sf(
    data = thimphu_streets,
    color = "grey60",
    linewidth = 0.2
  ) +
  geom_point(
    data = thimphu_stops |> distinct(place, .keep_all = TRUE),
    mapping = aes(x = long, y = lat),
    color = "#AB6AD5",
    size = 3
  ) +
  coord_sf(
    default_crs = st_crs(4326),
    xlim = c(thimphu_bbox["xmin"], thimphu_bbox["xmax"]),
    ylim = c(thimphu_bbox["ymin"], thimphu_bbox["ymax"]),
    expand = FALSE
  ) +
  labs(caption = "Data: © OpenStreetMap contributors") +
  theme_void() +
  theme(plot.caption = element_text(size = 8, color = "grey40", hjust = 1))

thimphu_map
