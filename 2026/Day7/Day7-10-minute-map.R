###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(rinat)

###############################################################################
## Load data                                                                  ##
###############################################################################

korth_raw <- get_inat_obs(
  query = "Korthalsella salicornioides",
  geo = TRUE,
  maxresults = 2000,
  place_id = 6803
) |>
  filter_out(license == "") |>
  select(
    scientific_name,
    datetime,
    place_guess,
    latitude,
    longitude,
    common_name,
    taxon_id,
    quality_grade,
    license
  )

write_csv(korth_raw, "2026/Day7/data/korthalsella.inat.csv")
