library(tidyverse)
library(sf)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(osmdata)
library(magick)
library(patchwork)
library(sbBrand)

patches <- st_read("2026/Day4/data/karka_patches2.gpkg") |>
  st_transform(2193)

n_clusters <- patches |>
  st_cast("POLYGON") |>
  nrow()

map_area <- patches |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(100)

map_box <- st_bbox(map_area)

bbox_ll <- map_area |>
  st_transform(4326) |>
  st_bbox()

osm_park <- opq(bbox = bbox_ll) |>
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve")) |>
  osmdata_sf()

osm_roads <- opq(bbox = bbox_ll) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

osm_water <- opq(bbox = bbox_ll) |>
  add_osm_feature(key = "waterway", value = c("stream", "river", "drain")) |>
  osmdata_sf()

park_area <- st_transform(osm_park$osm_polygons, 2193)

roads <- osm_roads$osm_lines |>
  filter(!highway %in% c("path", "footway", "track", "bridleway", "steps")) |>
  st_transform(2193)

streams <- st_transform(osm_water$osm_lines, 2193)

spread_graphic <- image_read("2026/Day4/data/karka_spread.png") |>
  image_ggplot()

systemfonts::require_font("Lato")
systemfonts::require_font("Baloo 2")

font <- "Lato"
brand_font <- "Baloo 2"

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
patch_col <- "#3F6B2A"
land_col <- "#F0EFEC"
park_col <- "#DDE6D8"
road_col <- "#FFFFFF"
stream_col <- "#8FBBE0"

plot_title <- glue(
  "<i>P. karka</i> has spread into {n_clusters} separate clusters across Kitchener Park"
)

social_caption <- sb_social_caption(
  data_source = "OpenStreetMap contributors"
)

library(camcorder)

gg_record(
  device = "png",
  width = 8,
  height = 9,
  units = "in",
  dpi = 300
)

karka_map <- ggplot() +
  geom_sf(data = park_area, fill = park_col, colour = NA) +
  geom_sf(data = roads, colour = road_col, linewidth = 1) +
  geom_sf(data = streams, colour = stream_col, linewidth = 0.8) +
  geom_sf(
    data = patches,
    fill = patch_col,
    colour = NA
  ) +
  coord_sf(
    xlim = c(map_box["xmin"], map_box["xmax"]),
    ylim = c(map_box["ymin"], map_box["ymax"]),
    crs = 2193,
    default_crs = 2193,
    expand = FALSE
  ) +
  theme_void(base_family = font) +
  theme(panel.background = element_rect(fill = land_col, colour = NA))

karka_map

karka_composite <- karka_map /
  spread_graphic +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    title = plot_title,
    caption = social_caption,
    theme = theme(
      plot.title = element_textbox_simple(
        family = font,
        face = "bold",
        size = rel(1.2),
        colour = text_col
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        margin = margin(t = 10)
      ),
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = bg_col, colour = bg_col)
    )
  )

karka_composite

gg_stop_recording()

ggsave(
  filename = "2026/Day4/output/day04-karka-clusters.png",
  plot = karka_composite,
  device = ragg::agg_png,
  width = 8,
  height = 9,
  units = "in",
  dpi = 300,
  bg = bg_col
)
