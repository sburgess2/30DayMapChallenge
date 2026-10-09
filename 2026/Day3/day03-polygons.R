library(tidyverse)
library(sf)
library(wdpar)
library(rnaturalearth)

wdpa_zip <- "data/wdpa/WDPA_Oct2026_BTN-shapefile.zip"

# To fetch data from Protected Planet. wdpar users a browser to fetch the file
#WDPA is a global protected-areas database
if (!file.exists(wdpa_zip)) {
  wdpa_raw <- wdpa_fetch(
    x = "Bhutan",
    wait = TRUE,
    download_dir = "data/wdpa",
    page_wait = 3
  )
}

wdpa_raw <- wdpa_read(wdpa_zip)

bhutan <- ne_countries(country = "Bhutan", scale = "large", returnclass = "sf")

protected_areas <- wdpa_raw |>
  st_make_valid() |>
  st_transform(st_crs(bhutan))

protected_areas |>
  st_drop_geometry() |>
  count(DESIG_ENG, IUCN_CAT, STATUS)

designation_levels <- c(
  "Other protected area",
  "Biological corridor",
  "Wildlife sanctuary",
  "Strict nature reserve",
  "National park"
)

designation_colours <- c(
  "National park" = "#009E73",
  "Wildlife sanctuary" = "#E69F00",
  "Strict nature reserve" = "#CC79A7",
  "Biological corridor" = "#56B4E9",
  "Other protected area" = "#BBBBBB"
)

protected_designated <- protected_areas |>
  group_by(NAME) |>
  summarise(area_km2 = sum(REP_AREA, na.rm = TRUE), .groups = "drop") |>
  mutate(
    designation = case_when(
      str_detect(NAME, "National Park") ~ "National park",
      str_detect(NAME, "Wildlife Sanctuary") ~ "Wildlife sanctuary",
      str_detect(NAME, "Strict Nature Reserve") ~ "Strict nature reserve",
      str_detect(NAME, "Biological Corridor") ~ "Biological corridor",
      TRUE ~ "Other protected area"
    ),
    designation = factor(designation, levels = designation_levels)
  ) |>
  arrange(designation)

park_labels <- protected_designated |>
  filter(designation %in% c("National park", "Wildlife sanctuary", "Strict nature reserve")) |>
  mutate(
    label = NAME |>
      str_remove(" National Park| Wildlife Sanctuary| Strict Nature Reserve")
  ) |>
  st_point_on_surface()

parks_map <- ggplot() +
  geom_sf(data = bhutan, fill = "grey97", colour = "grey60") +
  geom_sf(
    data = protected_designated,
    mapping = aes(fill = designation),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf_text(
    data = park_labels,
    mapping = aes(label = label),
    size = 2.8,
    colour = "grey10",
    fontface = "bold",
    check_overlap = TRUE
  ) +
  scale_fill_manual(
    values = designation_colours,
    breaks = rev(designation_levels),
    name = NULL
  ) +
  theme_void() +
  theme(legend.position = "bottom")

parks_map

ggsave(
  "output/day03-bhutan-protected-areas.png",
  parks_map,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
