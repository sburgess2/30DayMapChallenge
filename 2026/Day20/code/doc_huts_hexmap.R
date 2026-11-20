###################
## Load libraries ##
###################

library(tidyverse)
library(sf)
library(systemfonts)
library(ragg)
library(ggtext)
library(glue)
library(paletteer)
library(sbBrand)

###################
## Load data     ##
###################

# Download from the DOC Open Spatial Data portal and save to data/:
#   DOC Huts -> data/doc_huts.geojson
#   https://doc-deptconservation.opendata.arcgis.com/datasets/7f7321caf77b4101b9573db4575dd794_0/explore
#   (dataset page > "Download" or "I want to use this data" > GeoJSON)
# NZ coastline polygon (Stats NZ / LINZ, any NZ land polygon) -> data/nz_land.gpkg

huts_raw <- read_sf("2026/Day20/data/doc_huts.geojson")
land_raw <- read_sf("2026/Day20/data/nz_land.gpkg")

###################
## Look at data  ##
###################

glimpse(huts_raw)
#Retrieve coordinate reference system from object
st_crs(huts_raw)
nrow(huts_raw)


###################
## Clean data    ##
###################

nztm <- 2193 # NZ Transverse Mercator, metres

# Main islands + Stewart Island only (drops Chatham, sub-Antarctic, Kermadec)
main_islands <- st_bbox(
  c(xmin = 1000000, xmax = 2250000, ymin = 4600000, ymax = 6300000), #values in metres because they tied to the nztm cordinate system
  crs = st_crs(nztm)
) |>
  st_as_sfc() #converts box into polygon geometry

#Check if all polygons are valid
st_is_valid(land_raw)
table(st_is_valid(land_raw))
all(st_is_valid(land_raw))

land <- land_raw |>
  st_transform(nztm) # match the CRS of main_islands (metres)
#st_make_valid() |> # repair any invalid polygon geometry
#st_union()  # merge all polygons into one land shape
#st_intersection(main_islands) |> # crop to the main islands
#st_as_sf() # convert back to an sf object

huts <- huts_raw |>
  #st_zm(drop = TRUE) |> # drop any Z/M dimensions
  st_transform(nztm) |>
  distinct(TechObjectName, .keep_all = TRUE) |> # lon/lat to NZTM metres
  filter(CharValue14 == "Yes") |>
  drop_na(CharValue4) |>
  mutate(Area = str_split_i(FlocID, "-", 1))
#st_filter(main_islands) # keep huts inside the main islands box

# Duplicate records at identical locations would inflate counts; check
huts |>
  mutate(coords = st_as_text(geometry)) |>
  group_by(coords) |>
  filter(n() > 1) |>
  ungroup() |>
  st_drop_geometry() |>
  select(coords, TechObjectName, SubObjectType, CharValue2, OBJECTID) |>
  arrange(coords)

#Check distribution of huts
huts |>
  st_drop_geometry() |>
  count(Area) |>
  ggplot(aes(x = n, y = fct_reorder(Area, n))) +
  geom_col() +
  labs(x = "Number of huts", y = NULL)

huts |>
  st_drop_geometry() |>
  count(Area) |>
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Huts per area", y = "Number of areas")

huts_xy <- huts |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

ggplot() +
  geom_sf(data = land, fill = "grey95", colour = "grey60") +
  geom_hex(data = huts_xy, aes(x, y), binwidth = 30000, alpha = 0.8) +
  coord_sf()

###################
## Build hex grid ##
###################

# Larger hexes than the track map: with <1,000 huts nationally, small hexes
# would be almost all 0 or 1.
hex_width <- 30000 # flat-to-flat distance in metres (~780 km2 per hex)

#24
hex_grid <- st_make_grid(
  land,
  cellsize = units::set_units(10, km),
  square = FALSE
) |>
  st_as_sf() |>
  rename(geometry = x) |>
  mutate(hex_id = row_number()) |>
  st_filter(land)

###################
## Huts per hex  ##
###################

hex_data <- hex_grid |>
  mutate(n_huts = lengths(st_intersects(geometry, huts)))

n_huts_total <- sum(hex_data$n_huts)
n_zero <- sum(hex_data$n_huts == 0)

n_hex <- nrow(hex_data)

count(st_drop_geometry(hex_data), n_huts)
hex_data |> st_drop_geometry() |> slice_max(n_huts, n = 5)

# Huts are counts with a long right tail, so use discrete bins rather than
# a continuous scale. 0 gets its own class: "no DOC hut" is informative.
hex_data <- hex_data |>
  mutate(
    hut_class = cut(
      n_huts,
      breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
      labels = c("0", "1", "2", "3", "4", "5", "6+")
    )
  )

###################
## Load fonts    ##
###################

require_font("Roboto")
require_font("Roboto Slab")
require_font("Baloo 2")

title_font <- "Roboto Slab"
text_font <- "Roboto"
brand_font <- "Baloo 2"

###################
## Define colours ##
###################

bg_col <- "#f6f4ec"
text_col <- "#3b3b3b"

pal <- colorRampPalette(c("#56B1F7", "#132B43"))(6)
hut_cols <- c(
  "0" = "#e2ddcb",
  "1" = "#48cae4",
  "2" = "#00b4d8",
  "3" = "#0096c7",
  "4" = "#0077b6",
  "5" = "#023e8a",
  "6+" = "#03045e"
)

###################
## Titles etc.    ##
###################

plot_title <- "Where are New Zealand's backcountry huts located?"

subtitle <- glue(
  "Each hexagon covers about 780 km². Colour shows the number of Department of Conservation huts within each hexagon. ",
  "and bivvies within it. There are {format(n_huts_total, big.mark = ',')} ",
  "that are publicly availalble have at least one bunk.",
  "have none."
)

social_caption <- sb_social_caption(
  "DOC Open Spatial Data portal, DOC Huts.",
  font_family = text_font
)

###################
## Plot          ##
###################

library(camcorder)

gg_record(
  device = "png",
  width = 7,
  height = 9,
  unit = "in",
  dpi = 300
)

huts_hex_map <- ggplot() +
  geom_sf(
    data = hex_data,
    aes(fill = hut_class),
    colour = bg_col,
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = hut_cols,
    name = "Huts per hexagon",
    guide = guide_legend(nrow = 1, title.position = "top")
  ) +
  labs(
    title = plot_title,
    subtitle = subtitle,
    #caption = social_caption
  ) +
  theme_void(base_family = text_font, base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = NA),
    panel.background = element_rect(fill = bg_col, colour = NA),
    plot.margin = margin(15, 15, 15, 15),
    legend.position = "inside",
    legend.position.inside = c(0.78, 0.12),
    legend.title = element_text(family = text_font, colour = text_col),
    legend.text = element_text(family = text_font, colour = text_col),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = title_font,
      face = "bold",
      colour = text_col,
      size = 16,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      family = text_font,
      colour = text_col,
      size = 11,
      lineheight = 1.1,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = brand_font,
      colour = text_col,
      size = 10,
      margin = margin(t = 10)
    )
  )
huts_hex_map
record_polaroid()
gg_stop_recording()

###################
## Save png      ##
###################

ggsave(
  filename = "output/doc_huts_hexmap.png",
  plot = huts_hex_map,
  width = 7,
  height = 9,
  unit = "in",
  bg = bg_col,
  dpi = 300
)

huts |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  ggplot(aes(x, y)) +
  geom_hex(binwidth = 30000) +
  coord_equal()
