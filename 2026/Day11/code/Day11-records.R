library(tidyverse)
library(sf)
library(rnaturalearth)
library(tidygeocoder)

equal_earth <- "ESRI:54035"
size_scale <- 380000

#function to clean artist names
clean_name <- function(x) {
  x |>
    str_to_lower() |> #convert to lower case
    str_squish() #trim spaces at start and end
}

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name != "Antarctica") |>
  st_transform(equal_earth)

song_files <- c(
  "2026/Day11/data/Sara.csv",
  "2026/Day11/data/sara80s.csv",
  "2026/Day11/data/saraeighties.csv"
)

songs <- song_files |>
  map(\(f) read_csv(f, col_types = cols(.default = "c"))) |>
  list_rbind() |>
  distinct(`Track URI`, .keep_all = TRUE)

songs_wide <- songs |>
  select(`Track URI`, artist = `Artist Name(s)`) |>
  separate_wider_delim(
    artist,
    delim = ";",
    names_sep = "_",
    too_few = "align_start"
  )

artists <- read_csv(
  "2026/Day11/data/artist.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
) |>
  select(Artist, Town, Region, Country) |>
  drop_na(Artist) |>
  mutate(
    Artist = case_match(Artist, "Ed Sheerin" ~ "Ed Sheeran", .default = Artist),
    artist_key = clean_name(Artist)
  )

song_counts <- songs_wide |>
  pivot_longer(
    starts_with("artist_"),
    names_to = NULL,
    values_to = "artist",
    values_drop_na = TRUE
  ) |>
  mutate(artist_key = clean_name(artist)) |>
  filter(artist_key %in% artists$artist_key) |>
  distinct(`Track URI`, artist_key) |>
  count(artist_key, name = "songs") |>
  bind_rows(tibble(artist_key = "crowded house", songs = 4)) |>
  summarise(songs = sum(songs), .by = artist_key)


#arcgis uses Esri's commercial adress and place data. It's more forgving than  OSM and the number of requests aren't limited.
locations <- artists |>
  distinct(Town, Region, Country) |>
  unite(
    "full_address",
    Town,
    Region,
    Country,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  geocode(address = full_address, method = "arcgis")

records <- artists |>
  distinct(Artist, artist_key, Town, Region, Country) |>
  inner_join(song_counts, by = "artist_key") |>
  left_join(locations, by = c("Town", "Region", "Country")) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(equal_earth) |>
  arrange(desc(songs)) |>
  mutate(rank = row_number(), radius = sqrt(songs) * size_scale)

piece_spec <- bind_rows(
  tibble(frac = 1, fill = "#1b1b1b", line = "#000000", lw = 0.1),
  tibble(
    frac = seq(0.42, 0.95, length.out = 8),
    fill = "#00000000",
    line = "#4a4a4a",
    lw = 0.08
  ),
  tibble(frac = 0.33, fill = "#d9482b", line = "#00000000", lw = 0),
  tibble(frac = 0.04, fill = "#ffffff", line = "#00000000", lw = 0)
) |>
  mutate(piece = row_number())

record_pieces <- piece_spec |>
  pmap(\(frac, fill, line, lw, piece) {
    records |>
      mutate(
        geometry = st_buffer(geometry, radius * frac),
        fill = fill,
        line = line,
        lw = lw,
        piece = piece
      )
  }) |>
  bind_rows() |>
  arrange(rank, piece)

p <- ggplot() +
  geom_sf(data = world, fill = "grey93", colour = "white", linewidth = 0.15) +
  geom_sf(
    data = record_pieces,
    aes(fill = fill, colour = line, linewidth = lw)
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_linewidth_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))

p
ggsave(
  "2026/Day11/output/day11_records.png",
  p,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)


library(tidyverse)
library(sf)
library(rnaturalearth)
library(tidygeocoder)
library(ggforce)
library(ggfittext)

equal_earth <- "ESRI:54035"
size_scale <- 380000

clean_name <- function(x) {
  x |>
    str_to_lower() |>
    str_squish()
}

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name != "Antarctica") |>
  st_transform(equal_earth)

song_files <- c(
  "2026/Day11/data/Sara.csv",
  "2026/Day11/data/sara80s.csv",
  "2026/Day11/data/saraeighties.csv"
)

songs <- song_files |>
  map(\(f) read_csv(f, col_types = cols(.default = "c"))) |>
  list_rbind() |>
  distinct(`Track URI`, .keep_all = TRUE)

songs_wide <- songs |>
  select(`Track URI`, artist = `Artist Name(s)`) |>
  separate_wider_delim(
    artist,
    delim = ";",
    names_sep = "_",
    too_few = "align_start"
  )

artists <- read_csv(
  "2026/Day11/data/artist.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
) |>
  select(Artist, Town, Region, Country) |>
  drop_na(Artist) |>
  mutate(
    Artist = case_match(Artist, "Ed Sheerin" ~ "Ed Sheeran", .default = Artist),
    artist_key = clean_name(Artist)
  )

song_counts <- songs_wide |>
  pivot_longer(
    starts_with("artist_"),
    names_to = NULL,
    values_to = "artist",
    values_drop_na = TRUE
  ) |>
  mutate(artist_key = clean_name(artist)) |>
  filter(artist_key %in% artists$artist_key) |>
  distinct(`Track URI`, artist_key) |>
  count(artist_key, name = "songs") |>
  bind_rows(tibble(artist_key = "crowded house", songs = 4)) |>
  summarise(songs = sum(songs), .by = artist_key)

locations <- artists |>
  distinct(Town, Region, Country) |>
  unite(
    "full_address",
    Town,
    Region,
    Country,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  geocode(address = full_address, method = "arcgis")

genre_colours <- c(
  "Pop" = "#b5179e",
  "Rock" = "#1d4ed8",
  "R&B / Soul" = "#6d28d9",
  "Dance / Electronic" = "#0f766e",
  "Country / Folk" = "#a85a00",
  "K-pop" = "#15803d"
)

genre_lookup <- tribble(
  ~Artist           , ~genre               ,
  "Ed Sheeran"      , "Pop"                ,
  "Whitney Houston" , "R&B / Soul"         ,
  "Michael Jackson" , "R&B / Soul"         ,
  "Clean Bandit"    , "Dance / Electronic" ,
  "Teddy Swims"     , "R&B / Soul"         ,
  "Céline Dion"     , "Pop"                ,
  "Imagine Dragons" , "Rock"               ,
  "HUNTR/X"         , "K-pop"              ,
  "Dean Lewis"      , "Pop"                ,
  "Gracie Abrams"   , "Pop"                ,
  "Fleetwood Mac"   , "Rock"               ,
  "Cyndi Lauper"    , "Pop"                ,
  "Roxette"         , "Pop"                ,
  "Madonna"         , "Pop"                ,
  "Crowded House"   , "Rock"               ,
  "Lady Gaga"       , "Pop"                ,
  "Sting"           , "Rock"               ,
  "Alex & Sierra"   , "Country / Folk"     ,
  "Kasey Chambers"  , "Country / Folk"     ,
  "Gin Wigmore"     , "Rock"
)

records <- artists |>
  distinct(Artist, artist_key, Town, Region, Country) |>
  inner_join(song_counts, by = "artist_key") |>
  left_join(locations, by = c("Town", "Region", "Country")) |>
  left_join(genre_lookup, by = "Artist") |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(equal_earth) |>
  arrange(desc(songs)) |>
  mutate(
    rank = row_number(),
    radius = sqrt(songs) * size_scale,
    label_fill = unname(genre_colours[genre]),
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

record_layers <- function(rec) {
  grooves <- rec |>
    crossing(frac = seq(0.42, 0.92, length.out = 9)) |>
    mutate(groove_r = radius * frac)

  sheen <- rec |>
    crossing(start = c(0.12 * pi, 1.12 * pi)) |>
    mutate(end = start + 0.28 * pi)

  name_box <- rec |>
    mutate(
      label_r = 0.36 * radius,
      xmin = x - 0.78 * label_r,
      xmax = x + 0.78 * label_r,
      ymin = y + 0.18 * label_r,
      ymax = y + 0.88 * label_r
    )

  list(
    geom_circle(
      data = rec,
      aes(x0 = x, y0 = y, r = radius * 0.96),
      fill = "#1b1b1b",
      colour = "#000000",
      linewidth = 0.1
    ),
    geom_circle(
      data = grooves,
      aes(x0 = x, y0 = y, r = groove_r),
      fill = NA,
      colour = "#5a5a5a",
      linewidth = 0.08
    ),
    geom_arc_bar(
      data = sheen,
      aes(
        x0 = x,
        y0 = y,
        r0 = radius * 0.38,
        r = radius * 0.92,
        start = start,
        end = end
      ),
      fill = "white",
      alpha = 0.12,
      colour = NA
    ),
    geom_circle(
      data = rec,
      aes(x0 = x, y0 = y, r = radius * 0.36, fill = label_fill),
      colour = NA
    ),
    geom_fit_text(
      data = name_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = Artist),
      colour = "white",
      grow = TRUE,
      reflow = TRUE,
      min.size = 0
    ),
    geom_circle(
      data = rec,
      aes(x0 = x, y0 = y, r = radius * 0.04),
      fill = "white",
      colour = NA
    )
  )
}

layers <- records |>
  group_split(rank) |>
  map(record_layers) |>
  list_flatten()

p <- ggplot() +
  geom_sf(data = world, fill = "grey93", colour = "white", linewidth = 0.15) +
  layers +
  scale_fill_identity(
    guide = "legend",
    breaks = unname(genre_colours),
    labels = names(genre_colours),
    name = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = NA)
  )

p
ggsave(
  "2026/Day11/output/day11_records_genre.png",
  p,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)
