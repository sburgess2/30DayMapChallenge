###############################################################################
## Data origin                                                               ##
###############################################################################

# Dark sky places: Dark Sky Network New Zealand (DSNNZ), scraped from
# https://dsnnz.org/dark-sky-places/
#
# Locations: geocoded from place names with OpenStreetMap Nominatim
# via tidygeocoder (c) OpenStreetMap contributors
#
# Land: Natural Earth, via rnaturalearth

###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(rvest)
library(tidygeocoder)
library(sf)
library(rnaturalearth)

###############################################################################
## Settings                                                                  ##
###############################################################################

url_places <- "https://dsnnz.org/dark-sky-places/"
file_places <- "2026/Day17/data/dark_sky_places.csv"
file_coords <- "2026/Day17/data/dark_sky_places_geocoded.csv"

bg_col <- "#000000"
land_col <- "#14161c"
certified_col <- "#ffd45e"
aspiring_col <- "#5eb8ff"

# A custom projection string: Lambert azimuthal equal-area centred on NZ.
# Unlike NZTM (EPSG:2193) it keeps the Chatham Islands on the same map.
nz_crs <- "+proj=laea +lat_0=-41.5 +lon_0=173 +datum=WGS84 +units=m"

###############################################################################
## Scrape places                                                             ##
###############################################################################

dir.create("2026/Day17/data", showWarnings = FALSE)

if (!file.exists(file_places)) {
  # read_html() downloads the page. html_elements(xpath = ...) picks nodes
  # with XPath, a query language for HTML. The "|" means "or", so this returns
  # the two section headings and every "Read More" link in page order.
  nodes <- read_html(url_places) |>
    html_elements(
      xpath = "//h2[contains(., 'Places')] | //a[contains(@class, 'elementor-post__read-more')]"
    )

  # html_name() gives the tag ("h2" or "a"); html_attr() reads an attribute.
  # The aria-label holds the place name, e.g. "Read more about Akaroa".
  places_raw <- tibble(
    tag = html_name(nodes),
    heading = if_else(tag == "h2", html_text2(nodes), NA_character_),
    label = html_attr(nodes, "aria-label"),
    link = html_attr(nodes, "href")
  )

  places <- places_raw |>
    mutate(
      status = case_when(
        str_detect(heading, "Certified") ~ "Certified",
        str_detect(heading, "Aspiring") ~ "Aspiring"
      )
    ) |>
    # fill() copies the last non-missing status down the rows, so each link
    # inherits the status of the heading above it.
    fill(status) |>
    filter(tag == "a") |>
    transmute(
      place = label |>
        str_remove("^Read more about ") |>
        str_squish(),
      status,
      link
    )

  write_csv(places, file_places)
}

places <- read_csv(file_places, show_col_types = FALSE)

count(places, status)

###############################################################################
## Geocode places                                                            ##
###############################################################################

# tribble() builds a small tibble row by row. Used here as a lookup table of
# better search terms for places that would not geocode well by name alone.
search_terms <- tribble(
  ~place, ~search,
  "Aoraki MacKenzie", "Aoraki Mount Cook Village, New Zealand",
  "Aotea / Great Barrier Island", "Great Barrier Island, New Zealand",
  "Glenorchy / Tāhuna", "Glenorchy, Otago, New Zealand",
  "Kaikoura Dark Sky Sanctuary", "Kaikoura, New Zealand",
  "Kawarau-Gibbston Dark Sky Park", "Gibbston, Otago, New Zealand",
  "Naseby International Dark Sky Community", "Naseby, Otago, New Zealand",
  "Oxford Forest Conservation Area", "Oxford, Canterbury, New Zealand",
  "Rakiura / Stewart Island", "Stewart Island, New Zealand",
  "Wai-iti Recreational Reserve and Tuncliffe Forest", "Wai-iti, Tasman, New Zealand",
  "Wairarapa", "Masterton, New Zealand",
  "Central Otago", "Alexandra, Otago, New Zealand",
  "Far North District", "Kaitaia, New Zealand",
  "Fiordland", "Fiordland National Park, New Zealand",
  "Kahurangi", "Kahurangi National Park, New Zealand",
  "Rekohu / Chatham Islands", "Chatham Island, New Zealand",
  "Selwyn District", "Darfield, Canterbury, New Zealand",
  "Tararo, Near Te Anau", "Te Anau, New Zealand",
  "Wainuiomata-Orongorongo", "Wainuiomata, New Zealand",
  "Waitakere Ranges", "Waitākere Ranges Regional Park, New Zealand",
  "Whakatane and Eastern Bay of Plenty", "Whakatāne, New Zealand",
  "Winterstellar", "Winterstellar, New Zealand"
)

if (!file.exists(file_coords)) {
  places_geo <- places |>
    left_join(search_terms, by = "place") |>
    # coalesce() takes the first non-missing value: the lookup term if there
    # is one, otherwise the place name plus ", New Zealand".
    mutate(search = coalesce(search, str_c(place, ", New Zealand"))) |>
    # geocode() sends each address to OpenStreetMap's Nominatim service
    # (about one request per second) and adds lat and lon columns.
    geocode(address = search, method = "osm", lat = lat, long = lon)

  write_csv(places_geo, file_coords)
}

places_geo <- read_csv(file_coords, show_col_types = FALSE)

filter(places_geo, is.na(lat))

###############################################################################
## Prepare map data                                                          ##
###############################################################################

# st_as_sf() turns the lon/lat columns into point geometries. crs = 4326 says
# the coordinates are plain WGS84 degrees; st_transform() then reprojects them.
places_sf <- places_geo |>
  drop_na(lat, lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(nz_crs) |>
  mutate(status = factor(status, levels = c("Certified", "Aspiring")))

# ne_countries() returns Natural Earth country polygons as an sf object.
# scale = 50 is the 1:50m version and needs no extra data package.
nz_land <- ne_countries(
  scale = 50,
  country = "New Zealand",
  returnclass = "sf"
) |>
  st_transform(nz_crs)

pad <- 120000

# st_bbox() returns the bounding box (xmin, ymin, xmax, ymax) of the points.
# Padding it and using it as the plot limits crops out distant NZ islands.
# map_ratio (height / width) sets the PNG height when saving.
map_box <- st_bbox(places_sf)

map_xlim <- c(map_box["xmin"] - pad, map_box["xmax"] + pad)
map_ylim <- c(map_box["ymin"] - pad, map_box["ymax"] + pad)

map_ratio <- diff(map_ylim) / diff(map_xlim)

# Each row is one glow layer: big, faint circles underneath and a small,
# solid dot on top. Drawing them in order produces the glow effect.
glow_layers <- tribble(
  ~size, ~alpha,
  14, 0.04,
  10, 0.07,
  7, 0.12,
  4.5, 0.25,
  2.5, 0.6,
  1.2, 1
)

###############################################################################
## Plot                                                                      ##
###############################################################################

p_map <- ggplot() +
  geom_sf(data = nz_land, fill = land_col, colour = "#262932", linewidth = 0.2)

# A for loop adds one geom_sf() layer per glow layer. The legend is only
# shown for the last (smallest, solid) layer so it has a single clean key.
for (i in seq_len(nrow(glow_layers))) {
  p_map <- p_map +
    geom_sf(
      data = places_sf,
      aes(colour = status),
      size = glow_layers$size[i],
      alpha = glow_layers$alpha[i],
      show.legend = i == nrow(glow_layers)
    )
}

p_map <- p_map +
  scale_colour_manual(
    values = c(Certified = certified_col, Aspiring = aspiring_col),
    name = NULL,
    guide = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  # coord_sf() sets the map extent and keeps the aspect ratio fixed.
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  theme_void(base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = NA),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.85),
    legend.text = element_text(colour = "white", size = 10),
    legend.key = element_rect(fill = NA, colour = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

p_map

###############################################################################
## Save png                                                                  ##
###############################################################################

dir.create("2026/Day17/output", showWarnings = FALSE)

ggsave(
  filename = "2026/Day17/output/nz_dark_sky_places.png",
  plot = p_map,
  width = 8,
  height = 8 * map_ratio,
  dpi = 300,
  device = ragg::agg_png
)
