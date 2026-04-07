# ============================================================
# What's In Your Water? - Assign Neighborhoods to GPS Points
#
# Uses reverse geocoding via Nominatim (OpenStreetMap) to map
# each sample's GPS coordinates to a neighborhood name.
# Results are cached to a CSV so the API is only called once
# (or when new samples appear).
#
# Nominatim returns these address fields (most → least specific):
#   neighbourhood → suburb → city_district → quarter → county
# The most specific non-NA field is used as `neighborhood`.
#
# Author: Nyssa Silbiger
# Date:   2026-04-01
# ============================================================

library(tidyverse)
library(tidygeocoder)

# Path to cache file — prevents re-running ~733 API calls each session
cache_path <- "data/neighborhood_cache.csv"

# Nominatim address fields to try, in order of specificity
neighborhood_cols <- c("neighbourhood", "suburb", "city_district", "quarter", "county")

# ------------------------------------------------------------------
# 1. Load or initialize the reverse-geocoding cache
# ------------------------------------------------------------------

if (file.exists(cache_path)) {
  geo_cache <- read_csv(cache_path, show_col_types = FALSE)
} else {
  geo_cache <- tibble(sample_id = character(), neighborhood = character())
}

# Find any samples not yet in the cache
to_geocode <- df_clean |>
  select(sample_id, latitude, longitude) |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  anti_join(geo_cache, by = "sample_id")

# ------------------------------------------------------------------
# 2. Reverse geocode new samples (Nominatim, ~1 req/sec)
#    Note: 733 samples ≈ 12 minutes on first run
# ------------------------------------------------------------------

if (nrow(to_geocode) > 0) {
  message("Reverse geocoding ", nrow(to_geocode), " new sample(s) — this may take a few minutes...")

  new_geo <- to_geocode |>
    reverse_geocode(
      lat          = latitude,
      long         = longitude,
      method       = "osm",       # Nominatim — free, no API key required
      full_results = TRUE,        # return all address components
      custom_query = list(zoom = 14)  # zoom 14 = neighbourhood level
    )

  # Ensure all expected address columns exist (Nominatim omits some for ocean points)
  for (col in neighborhood_cols) {
    if (!col %in% names(new_geo)) new_geo[[col]] <- NA_character_
  }

  new_geo <- new_geo |>
    mutate(
      # Resolve to the most specific available place name
      neighborhood = coalesce(
        neighbourhood, suburb, city_district, quarter, county
      )
    ) |>
    select(sample_id, neighborhood)

  # Append to cache and save
  geo_cache <- bind_rows(geo_cache, new_geo)
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(geo_cache, cache_path)
  message("Cache saved to: ", cache_path)

} else {
  message("All samples already cached — no API calls needed.")
}

# ------------------------------------------------------------------
# 3. Join neighborhood back to df_clean
# ------------------------------------------------------------------

df_clean <- df_clean |>
  left_join(geo_cache |> select(sample_id, neighborhood), by = "sample_id")

# Quick summary of results
df_clean |>
  count(neighborhood, sort = TRUE) |>
  print(n = 25)
