###############################################################################
## Data origin                                                               ##
###############################################################################

# Solar installations: Electricity Authority (New Zealand), Solar Installations
# dataset built from the Electricity Registry. SolarInstallationsByStreet.csv
# https://www.ea.govt.nz/data-and-insights/datasets/Datasets/Retail/SolarInstallations/
#
# Dwellings: Stats NZ, 2023 Census, occupied private dwellings by SA2
# (CEN23_TBT_005_WIDE_subnational.csv), Aotearoa Data Explorer
# https://explore.data.stats.govt.nz/
#
# Boundaries: Stats NZ, Statistical Area 2 2026 (CC BY 4.0)
# https://datafinder.stats.govt.nz/layer/123515-statistical-area-2-2026/

###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(sf)
library(scales)

###############################################################################
## Settings                                                                  ##
###############################################################################

min_dwellings <- 20
suppressed_icps <- 2

bg_col <- "#fbfaf7"
na_col <- "#e3e1dc"

###############################################################################
## Load data                                                                 ##
###############################################################################

solar_raw <- read_csv(
  "2026/Day12/data/SolarInstallationsByStreet.csv",
  col_types = cols(.default = "c")
)

dwellings_raw <- read_csv(
  "2026/Day12/data/CEN23_TBT_005_WIDE_subnational.csv",
  skip = 5,
  col_names = FALSE,
  col_select = c(1, 2, 7),
  col_types = cols(.default = "c")
)

sa2_raw <- st_read(
  "2026/Day12/data/statsnz-statistical-area-2-2026-SHP/statistical-area-2-2026.shp",
  quiet = TRUE
)

###############################################################################
## Prepare data                                                              ##
###############################################################################

solar_sa2 <- solar_raw |>
  filter(MarketSegment == "Res") |>
  mutate(
    icps = if_else(
      ICPs == "3 or less",
      suppressed_icps,
      parse_number(ICPs)
    )
  ) |>
  summarise(
    solar_icps = sum(icps, na.rm = TRUE),
    .by = c(sa2_code = StatisticalArea2Code)
  )

dwellings_sa2 <- dwellings_raw |>
  rename(area_type = X1, sa2_code = X2, dwellings = X7) |>
  filter(area_type == "SA2") |>
  mutate(dwellings = parse_number(dwellings)) |>
  select(sa2_code, dwellings)

sa2_map <- sa2_raw |>
  rename(sa2_code = SA22026_V1, sa2_name = SA22026__1) |>
  mutate(sa2_code = as.character(sa2_code)) |>
  left_join(dwellings_sa2, by = "sa2_code") |>
  left_join(solar_sa2, by = "sa2_code") |>
  mutate(
    solar_icps = replace_na(solar_icps, 0),
    per_100 = if_else(
      dwellings >= min_dwellings,
      solar_icps / dwellings * 100,
      NA_real_
    )
  )

anti_join(solar_sa2, dwellings_sa2, by = "sa2_code")
anti_join(solar_sa2, st_drop_geometry(sa2_raw) |>
  transmute(sa2_code = as.character(SA22026_V1)), by = "sa2_code")

summary(sa2_map$per_100)

###############################################################################
## Plot                                                                      ##
###############################################################################

mainland_box <- st_bbox(
  c(xmin = 1000000, xmax = 2200000, ymin = 4700000, ymax = 6250000),
  crs = st_crs(2193)
)

sa2_plot <- sa2_map |>
  st_transform(2193) |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  st_crop(mainland_box) |>
  filter(!is.na(per_100))

upper_limit <- quantile(sa2_plot$per_100, 0.99, na.rm = TRUE)

p_map <- ggplot(sa2_plot) +
  geom_sf(aes(fill = per_100), colour = NA) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    limits = c(0, upper_limit),
    oob = squish,
    labels = label_number(accuracy = 1),
    name = "Residential solar installations\nper 100 occupied dwellings",
    guide = guide_colourbar(
      barwidth = unit(5, "cm"),
      barheight = unit(0.35, "cm"),
      title.position = "top"
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    caption = paste0(
      "Source: Electricity Authority, Electricity Registry solar installations by street (residential connections); ",
      "Stats NZ, 2023 Census occupied private dwellings and Statistical Area 2 2026 boundaries (CC BY 4.0).\n",
      "Streets with 3 or fewer installations are counted as 2. SA2s with fewer than ",
      min_dwellings,
      " dwellings are not shown. Colour scale capped at the 99th percentile."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = NA),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.9),
    legend.title = element_text(size = 10),
    plot.caption = element_text(
      size = 7.5,
      hjust = 0,
      colour = "#555555",
      margin = margin(t = 10)
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

p_map

###############################################################################
## Save png                                                                  ##
###############################################################################

dir.create("2026/Day12/output", showWarnings = FALSE)

ggsave(
  filename = "2026/Day12/output/nz_solar_installations_sa2.png",
  plot = p_map,
  width = 7,
  height = 9,
  dpi = 300,
  device = ragg::agg_png
)
