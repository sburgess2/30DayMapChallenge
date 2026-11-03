library(tidyverse)
library(sf)
library(wdpar)
library(rnaturalearth)
library(ggtext)
library(sbBrand)

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
  "National park" = "#DBB6AE",
  "Wildlife sanctuary" = "#D6D7B8",
  "Strict nature reserve" = "#5e6d3f",
  "Biological corridor" = "#AEB7C5",
  "Other protected area" = "#928B8F"
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
  filter(
    designation %in%
      c("National park", "Wildlife sanctuary", "Strict nature reserve")
  ) |>
  mutate(
    label = NAME |>
      str_remove(" National Park| Wildlife Sanctuary| Strict Nature Reserve")
  ) |>
  st_point_on_surface()

protected_pct <- round(
  as.numeric(sum(st_area(protected_areas))) /
    as.numeric(sum(st_area(bhutan))) *
    100
)

plot_title <- str_glue(
  "{protected_pct}% of Bhutan is a designated protected area"
)

social_caption <- sb_social_caption(
  data_source = "The World Database on Protected Areas"
)

library(camcorder)
gg_record(
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

parks_map <- ggplot() +
  geom_sf(data = bhutan, fill = "grey95", colour = NA) +
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
  labs(title = plot_title, caption = social_caption) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.location = "plot",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_textbox_simple(hjust = 0, margin = margin(t = 10))
  )

parks_map

record_polaroid()

ggsave(
  "output/day03-bhutan-protected-areas.png",
  parks_map,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
