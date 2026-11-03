library(tidyverse)
library(sf)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(maptiles)
library(tidyterra)
library(magick)
library(patchwork)
library(sbBrand)

patches <- st_read("2026/Day4/data/karka_patches.gpkg") |>
  st_transform(2193)

cluster_distance <- 30

clusters <- patches |>
  st_buffer(cluster_distance / 2) |>
  st_union() |>
  st_cast("POLYGON") |>
  st_as_sf() |>
  mutate(cluster_id = row_number())

n_patches <- nrow(patches)
n_clusters <- nrow(clusters)

map_area <- patches |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(100)

base_tiles <- get_tiles(
  map_area,
  provider = "CartoDB.PositronNoLabels",
  zoom = 17,
  crop = TRUE
)

patches_plot <- st_transform(patches, 4326)
clusters_plot <- st_transform(clusters, 4326)

spread_graphic <- image_read("2026/Day4/data/karka_spread.png") |>
  image_ggplot()

systemfonts::require_font("Lato")
systemfonts::require_font("Baloo 2")

font <- "Lato"
brand_font <- "Baloo 2"

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
patch_col <- "#C0562F"
cluster_fill <- scales::alpha(patch_col, 0.12)

plot_title <- glue(
  "<i>P. karka</i> has spread into {n_clusters} separate clusters across Kitchener Park"
)

subtitle <- glue(
  "<span style='color:{patch_col}'>**Patches**</span> within {cluster_distance} m ",
  "of each other are grouped into a cluster (dashed outline)"
)

social_caption <- sb_social_caption(
  data_source = "OpenStreetMap contributors, CARTO"
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
  geom_spatraster_rgb(data = base_tiles) +
  geom_sf(
    data = clusters_plot,
    fill = cluster_fill,
    colour = patch_col,
    linewidth = 0.3,
    linetype = "dashed"
  ) +
  geom_sf(
    data = patches_plot,
    fill = patch_col,
    colour = NA
  ) +
  coord_sf(default_crs = 4326, expand = FALSE) +
  theme_void(base_family = font)

karka_composite <- karka_map /
  spread_graphic +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    title = plot_title,
    subtitle = subtitle,
    caption = social_caption,
    theme = theme(
      plot.title = element_textbox_simple(
        family = font,
        face = "bold",
        size = rel(1.2),
        colour = text_col
      ),
      plot.subtitle = element_textbox_simple(
        family = font,
        colour = text_col,
        margin = margin(t = 5, b = 10)
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
