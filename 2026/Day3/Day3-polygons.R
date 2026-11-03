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
  #"Other protected area",
  "Biological corridor",
  "Wildlife sanctuary",
  "Strict nature reserve",
  "National park"
)

designation_colours <- c(
  "National park" = "#DBB6AE",
  "Wildlife sanctuary" = "#D6D7B8",
  "Strict nature reserve" = "#AEB7C5",
  "Biological corridor" = "#928B8F"
  #"Other protected area" = "#928B8F"
)

extra_corridor <- "Royal Botanical Park"

protected_designated <- protected_areas |>
  group_by(NAME) |>
  summarise(area_km2 = sum(REP_AREA, na.rm = TRUE), .groups = "drop") |>
  mutate(
    designation = case_when(
      str_detect(NAME, "National Park") ~ "National park",
      str_detect(NAME, "Wildlife Sanctuary") ~ "Wildlife sanctuary",
      str_detect(NAME, "Strict Nature Reserve") ~ "Strict nature reserve",
      str_detect(NAME, "Biological Corridor") ~ "Biological corridor",
      NAME %in% extra_corridor ~ "Biological corridor",
      TRUE ~ NA_character_
    ),
    designation = factor(designation, levels = designation_levels)
  ) |>
  filter(!is.na(designation)) |>
  arrange(designation)

protected_pct <- round(
  as.numeric(sum(st_area(protected_areas))) /
    as.numeric(sum(st_area(bhutan))) *
    100
)

plot_title <- str_glue(
  "Half of Bhutan is protected land"
)

social_caption <- sb_social_caption(
  data_source = "The World Database on Protected Areas"
)

systemfonts::require_font("Readex Pro")

font <- "Readex Pro"
text_col <- "grey20"
land_col <- "grey95"
border_col <- "white"
bg_col <- "white"

library(camcorder)
gg_record(
  device = ragg::agg_png,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

parks_map <- ggplot() +
  geom_sf(data = bhutan, fill = land_col, colour = NA) +
  geom_sf(
    data = protected_designated,
    mapping = aes(fill = designation),
    colour = border_col,
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = designation_colours,
    breaks = rev(designation_levels),
    name = NULL
  ) +
  labs(title = plot_title, caption = social_caption) +
  theme_void(base_family = font) +
  theme(
    text = element_text(colour = text_col),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "top",
    legend.justification = "left",
    legend.location = "plot",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      #face = "bold",
      hjust = 0,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(hjust = 0, margin = margin(t = 10))
  )

parks_map

record_polaroid()

ggsave(
  "2026/Day3/output/day03-bhutan-protected-areas.png",
  parks_map,
  width = 10,
  height = 6,
  dpi = 300,
  bg = bg_col,
  device = ragg::agg_png
)

ggsave(
  filename = "2026/Day3/output/day03_bhutan_insta.png",
  plot = parks_map,
  device = ragg::agg_png,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)
