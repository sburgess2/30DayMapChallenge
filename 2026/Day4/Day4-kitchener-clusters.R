library(tidyverse)
library(sf)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(magick)
library(patchwork)
library(tidyterra)
library(geomtextpath)
library(sbBrand)

patches <- st_read("2026/Day4/data/karka_patches2.gpkg") |>
  st_transform(2193)

map_area <- patches |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(100)

map_box <- st_bbox(map_area)

map_xmin <- unname(map_box["xmin"])
map_xmax <- unname(map_box["xmax"])
map_ymin <- unname(map_box["ymin"])
map_ymax <- unname(map_box["ymax"])
map_width <- map_xmax - map_xmin
header_height <- (map_ymax - map_ymin) * 0.3
fig_width <- 8
fig_height <- fig_width * (map_ymax - map_ymin + header_height) / map_width + 1.5

osm_file <- "2026/Day4/data/map.osm"

main_road_width <- 4
minor_road_width <- 1.5

park_area <- st_read(osm_file, layer = "multipolygons", quiet = TRUE) |>
  filter(
    landuse == "forest" |
      natural == "wood" |
      leisure %in% c("park", "nature_reserve")
  ) |>
  st_transform(2193)

osm_lines <- st_read(osm_file, layer = "lines", quiet = TRUE) |>
  st_transform(2193)

roads <- osm_lines |>
  filter(
    !is.na(highway),
    !highway %in% c("path", "footway", "track", "bridleway", "steps")
  ) |>
  mutate(
    road_width = if_else(
      highway %in% c("secondary", "tertiary"),
      main_road_width,
      minor_road_width
    )
  )

streams <- osm_lines |>
  filter(waterway %in% c("stream", "river", "drain"))

stream_label <- streams |>
  filter(grepl("kino Stream", name)) |>
  st_union() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  st_as_sf() |>
  mutate(label = "Mākino Stream")

road_label <- roads |>
  filter(name == "Kawakawa Road") |>
  st_union() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  st_as_sf() |>
  mutate(label = "Kawakawa Rd")

park_area <- st_crop(park_area, map_box)
roads <- st_crop(roads, map_box)
streams <- st_crop(streams, map_box)

patch_union <- st_union(patches)

park_shape <- park_area |>
  filter(name == "Kitchener Park") |>
  st_union()

grid_points <- st_make_grid(map_area, cellsize = 10, what = "centers") |>
  st_as_sf() |>
  mutate(dist_patch = as.numeric(st_distance(x, patch_union)))

park_nudge <- c(40, -40)
note_nudge <- c(-40, 40)

park_label <- grid_points |>
  filter(lengths(st_intersects(x, park_shape)) > 0) |>
  mutate(
    clearance = pmin(
      dist_patch,
      as.numeric(st_distance(x, st_boundary(park_shape)))
    )
  ) |>
  slice_max(clearance, n = 1, with_ties = FALSE) |>
  mutate(label = "Awahuri Forest Park")

st_geometry(park_label) <- st_geometry(park_label) + park_nudge

patch_polys <- st_cast(patches, "POLYGON")

target_patch <- patch_polys |>
  slice_max(as.numeric(st_area(patch_polys)), n = 1)

target_xy <- target_patch |>
  st_point_on_surface() |>
  st_coordinates()

note_spot <- grid_points |>
  mutate(
    dist_target = as.numeric(st_distance(x, target_patch)),
    dist_label = as.numeric(st_distance(x, park_label))
  ) |>
  filter(dist_target > 30, dist_target < 160, dist_label > 120) |>
  slice_max(dist_patch, n = 1, with_ties = FALSE)

note_xy <- st_coordinates(note_spot)

patch_note <- tibble(
  x = target_xy[1],
  y = target_xy[2],
  x_text = note_xy[1] + note_nudge[1],
  y_text = note_xy[2] + note_nudge[2],
  label = "Tall reed<br>(<i>Phragmites karka</i>)"
)

spread_img <- image_read("2026/Day4/images/p_karka_plant_only.png") |>
  image_scale("600")

spread_rgba <- image_data(spread_img, channels = "rgba")

spread_arr <- array(as.integer(spread_rgba), dim = dim(spread_rgba)) |>
  aperm(c(3, 2, 1))

spread_layers <- map(1:3, \(i) {
  layer <- spread_arr[, , i]
  layer[spread_arr[, , 4] == 0] <- NA
  layer
})

image_width <- map_width * 0.34
image_height <- image_width * dim(spread_arr)[1] / dim(spread_arr)[2]
image_xmin <- map_xmax - image_width
image_xmax <- map_xmax
image_ymax <- map_ymax + header_height
image_ymin <- image_ymax - image_height

spread_rast <- terra::rast(
  array(unlist(spread_layers), dim = c(dim(spread_arr)[1:2], 3))
)
terra::ext(spread_rast) <- c(image_xmin, image_xmax, image_ymin, image_ymax)
terra::crs(spread_rast) <- "EPSG:2193"

curve_path <- function(x, y, xend, yend, bend = 0.25, n = 50) {
  ctrl_x <- (x + xend) / 2 - bend * (yend - y)
  ctrl_y <- (y + yend) / 2 + bend * (xend - x)
  t <- seq(0, 1, length.out = n)
  tibble(
    x = (1 - t)^2 * x + 2 * (1 - t) * t * ctrl_x + t^2 * xend,
    y = (1 - t)^2 * y + 2 * (1 - t) * t * ctrl_y + t^2 * yend
  )
}

label_gap <- 18
image_target_x <- image_xmin + 0.4 * image_width
image_target_y <- image_ymin + 0.35 * image_height

curve_to_patch <- curve_path(
  patch_note$x_text,
  patch_note$y_text - label_gap,
  patch_note$x,
  patch_note$y
)

curve_to_image <- curve_path(
  patch_note$x_text,
  patch_note$y_text + label_gap,
  image_target_x,
  image_target_y,
  bend = -0.2
)

#systemfonts::require_font("Avenir")
systemfonts::require_font("Baloo 2")
systemfonts::require_font("Nunito Sans")
font <- "Nunito Sans"

#font <- "Avenir"
brand_font <- "Baloo 2"

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
patch_col <- "#3F6B2A"
land_col <- "#F0EFEC"
park_col <- "#DDE6D8"
road_col <- "#FFFFFF"
stream_col <- "#8FBBE0"
stream_label_col <- "#4A86B5"
label_col <- "#545454"
label_pt <- 10
park_pt <- 13

plot_title <- "Where tall reed clusters in Awahuri Forest Park"

subtitle <- paste(
  "<i>Phragmites karka</i> grows in dense clusters along Makino Stream",
  "and has spread into Awahuri Forest Park. It spreads by surface runners,",
  "so a single plant becomes a thick stand up to 4 m tall."
)

subtitle_pt <- 10

social_caption <- sb_social_caption(
  data_source = "Awahuri Forest Kitchener Park Trust. Basemap: OpenStreetMap contributors"
)

library(camcorder)

gg_record(
  device = "png",
  width = fig_width,
  height = fig_height,
  units = "in",
  dpi = 300
)

karka_map <- ggplot() +
  annotate(
    "rect",
    xmin = map_xmin,
    xmax = map_xmax,
    ymin = map_ymin,
    ymax = map_ymax,
    fill = land_col
  ) +
  geom_sf(data = park_area, fill = park_col, colour = NA) +
  geom_sf(
    data = roads,
    mapping = aes(linewidth = road_width),
    colour = road_col,
    lineend = "round"
  ) +
  scale_linewidth_identity() +
  geom_sf(data = streams, colour = stream_col, linewidth = 0.8) +
  geom_sf(
    data = patches,
    fill = patch_col,
    colour = NA
  ) +
  geom_textsf(
    data = road_label,
    mapping = aes(label = label),
    family = font,
    colour = label_col,
    size = label_pt / .pt,
    linecolour = NA
  ) +
  geom_textsf(
    data = stream_label,
    mapping = aes(label = label),
    family = font,
    colour = stream_label_col,
    size = label_pt / .pt,
    vjust = -0.5,
    linecolour = NA
  ) +
  geom_sf_text(
    data = park_label,
    mapping = aes(label = label),
    family = font,
    colour = label_col,
    size = park_pt / .pt
  ) +
  geom_spatraster_rgb(data = spread_rast, maxcell = Inf) +
  geom_path(
    data = curve_to_image,
    mapping = aes(x = x, y = y),
    colour = label_col,
    linewidth = 0.3,
    linetype = "dashed"
  ) +
  geom_path(
    data = curve_to_patch,
    mapping = aes(x = x, y = y),
    colour = label_col,
    linewidth = 0.3
  ) +
  geom_point(
    data = patch_note,
    mapping = aes(x = x, y = y),
    colour = label_col,
    size = 1
  ) +
  geom_textbox(
    data = tibble(x = map_xmin, y = map_ymax + header_height, label = subtitle),
    mapping = aes(x = x, y = y, label = label),
    family = font,
    colour = text_col,
    size = subtitle_pt / .pt,
    width = unit(0.6, "npc"),
    box.colour = NA,
    fill = NA,
    hjust = 0,
    vjust = 1
  ) +
  geom_richtext(
    data = patch_note,
    mapping = aes(x = x_text, y = y_text, label = label),
    family = font,
    colour = label_col,
    size = label_pt / .pt,
    fill = NA,
    label.colour = NA
  ) +
  coord_sf(
    xlim = c(map_xmin, map_xmax),
    ylim = c(map_ymin, map_ymax + header_height),
    crs = 2193,
    default_crs = 2193,
    expand = FALSE
  ) +
  labs(title = plot_title, caption = social_caption) +
  theme_void(base_family = font) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      family = font,
      face = "bold",
      size = rel(1.2),
      colour = text_col,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      margin = margin(t = 10)
    ),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )

karka_map
record_polaroid()
gg_stop_recording()

ragg::agg_png(
  filename = "2026/Day4/output/day04-karka-clusters.png",
  width = fig_width,
  height = fig_height,
  units = "in",
  res = 300,
  background = bg_col
)
print(karka_map)
dev.off()
