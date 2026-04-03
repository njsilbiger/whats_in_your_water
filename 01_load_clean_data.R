# ============================================================
# What's In Your Water? - Ocean Sample Submission Data
# Load and clean submission form data from Google Sheets
#
# Author: Nyssa Silbiger
# Date:   2026-03-28
# ============================================================

library(tidyverse)
library(googlesheets4)
library(sf)


# --------------------------------------------------------------
# 1. Authenticate with Google and load data
# --------------------------------------------------------------

# Authenticate using cached OAuth token.
# For local use, the token is stored in the default gargle cache.
# For shinyapps.io deployment, a token is cached in .secrets/ and
# deployed with the app (add .secrets/ to .gitignore, not .rscignore).
options(gargle_oauth_cache = ".secrets")
gs4_auth(email = "nyssa.silbiger@gmail.com")

sheet_url <- "https://docs.google.com/spreadsheets/d/1iWRGC9ciIXbwNTaAtEbVjTVLGIQ2pgk2RGrgAApPVRs/edit?gid=239003339"

df <- read_sheet(sheet_url)


# --------------------------------------------------------------
# 2. Define coordinate parser
# --------------------------------------------------------------

# Respondents entered GPS coordinates in mixed formats:
#   - Decimal degrees with direction suffix  e.g. "21.47309° N", "158.21960° W"
#   - Decimal degrees with negative sign     e.g. "-157.940574"
#   - Degrees-minutes-seconds (DMS)          e.g. "21°17'54\"", "21 42'19\""
#
# Some DMS entries use Unicode curly quotes (U+2019, U+201D) instead of
# straight ASCII apostrophe/quote — a common artifact of mobile keyboards.
# This function handles all observed variants and returns decimal degrees.

parse_coord <- function(x, is_lon = FALSE) {
  x <- trimws(as.character(x))

  # Detect an explicit negative sign (already-negative decimal degrees)
  has_neg <- grepl("^-", x)

  # Replace degree symbols and all quote variants (ASCII + Unicode curly)
  # with spaces so we can split on whitespace
  x_clean <- gsub("[\u00b0\u00ba\u2018\u2019\u201c\u201d'\"]+", " ", x)

  # Remove cardinal direction letters
  x_clean <- gsub("[NnSsEeWw]", "", x_clean)

  # Collapse multiple spaces and trim
  x_clean <- trimws(gsub("\\s+", " ", x_clean))

  # Split into numeric components
  parts <- suppressWarnings(as.numeric(strsplit(x_clean, " ")[[1]]))
  parts <- parts[!is.na(parts)]

  if (length(parts) == 0) return(NA_real_)

  if (length(parts) == 1) {
    # Already decimal degrees
    val <- parts[1]
  } else {
    # Convert DMS → decimal degrees
    deg <- parts[1]
    min <- parts[2]
    sec <- if (length(parts) >= 3) parts[3] else 0
    val <- deg + min / 60 + sec / 3600
  }

  # All sample sites are in Hawaii (West longitude), so negate longitude
  # values that don't already carry an explicit negative sign
  if (is_lon && !has_neg) val <- -val

  val
}


# --------------------------------------------------------------
# 3. Parse and clean coordinates
# --------------------------------------------------------------

df_clean <- df |>
  mutate(
    # Unlist the list-columns returned by googlesheets4 for mixed-type columns
    lat_raw = sapply(`Latitude (From GPS Coordinates - Example: 21 18'27")`, as.character),
    lon_raw = sapply(`Longitude (From GPS Coordinates  - Example: 157  49'36"  )`, as.character),

    # M268 entered "1571'26"" — missing separator between degrees and minutes;
    # corrected to "157 1'26"" (157°1'26")
    lon_raw = if_else(
      trimws(`Sample Bottle ID from the label (example OA1 for O'ahu, M1 for Maui)`) == "M268",
      "157 1'26\"",
      lon_raw
    ),

    # Parse to numeric decimal degrees
    latitude  = sapply(lat_raw, parse_coord, is_lon = FALSE),
    longitude = sapply(lon_raw, parse_coord, is_lon = TRUE)
  )


# --------------------------------------------------------------
# 4. Clean remaining columns and rename
# --------------------------------------------------------------

df_clean <- df_clean |>
  mutate(
    # Assign Hawaii Standard Time (UTC-10) to the naive datetimes returned
    # by googlesheets4; the sheet does not store timezone information
    timestamp_hst  = force_tz(Timestamp, tzone = "Pacific/Honolulu"),
    collected_hst  = force_tz(
      `Date and Time Seawater Sample was Collected`,
      tzone = "Pacific/Honolulu"
    ),

    # Trim any accidental whitespace from text fields
    island    = trimws(`Which island are you submitting this sample from?`),
    sample_id = toupper(trimws(`Sample Bottle ID from the label (example OA1 for O'ahu, M1 for Maui)`)),

    # Extract the numeric portion of the sample ID for sorting/joining
    sample_num = as.integer(gsub("[^0-9]", "", sample_id)),

    notes = trimws(`Any notes you want to share about the collection process or the sample site? (Optional)`)
  ) |>
  # Keep only the cleaned columns in a tidy order; drop raw form columns
  select(
    timestamp_hst,
    name = `Your Name (First and Last)`,
    island,
    sample_id,
    sample_num,
    collected_hst,
    latitude,
    longitude,
    lat_raw,
    lon_raw,
    notes
  )

# --------------------------------------------------------------
# 5. Data quality flags
# --------------------------------------------------------------

# OA324 has a collection time of 00:04 HST but was submitted at 12:05 HST —
# corrected to 12:04 HST (noon/midnight mix-up during manual entry)
df_clean <- df_clean |>
  mutate(collected_hst = if_else(
    sample_id == "OA324",
    collected_hst + hours(12),
    collected_hst
  ))


# --------------------------------------------------------------
# 6. Deduplicate: keep only the most recent submission per sample ID
# --------------------------------------------------------------

df_clean <- df_clean |>
  arrange(sample_id, desc(timestamp_hst)) |>
  distinct(sample_id, .keep_all = TRUE)


# --------------------------------------------------------------
# 7. Assign ahupuaʻa to each sample via spatial join
# --------------------------------------------------------------
#
# Ahupuaʻa boundaries come from the Hawaii Statewide GIS Program
# (Office of Hawaiian Affairs / DLNR). The boundary file is
# downloaded once and cached locally. Assignments are cached
# per sample so only new samples require a spatial join.

ahupuaa_bounds_path <- "data/ahupuaa_boundaries.geojson"
ahupuaa_cache_path  <- "data/ahupuaa_cache.csv"

ahupuaa_url <- paste0(
  "https://geodata.hawaii.gov/arcgis/rest/services/HistoricCultural/MapServer/1/query",
  "?where=1%3D1&outFields=ahupuaa,moku,mokupuni&f=geojson&outSR=4326"
)

# Download boundary file once; reload from disk on subsequent runs
if (!file.exists(ahupuaa_bounds_path)) {
  message("Downloading ahupuaʻa boundaries from Hawaii GIS portal...")
  ahupuaa_sf <- st_read(ahupuaa_url, quiet = TRUE) |>
    st_transform(crs = 4326)
  st_write(ahupuaa_sf, ahupuaa_bounds_path, delete_dsn = TRUE, quiet = TRUE)
  message("Boundaries saved to: ", ahupuaa_bounds_path)
} else {
  ahupuaa_sf <- st_read(ahupuaa_bounds_path, quiet = TRUE)
}

# Pre-compute a 5 km buffer around all ahupuaʻa polygons merged into one shape.
# Used instead of st_distance (which has s2 winding-order issues) to decide
# whether an unmatched sample is coastal (within 5 km of any ahupuaʻa) or
# truly open ocean.  Uses UTM Zone 4N (EPSG 32604) for reliable metric units.
ahupuaa_coastal_zone <- ahupuaa_sf |>
  st_transform(crs = 32604) |>
  st_buffer(dist = 5000) |>
  st_union() |>
  st_transform(crs = 4326)

# Load or initialize the per-sample assignment cache
if (file.exists(ahupuaa_cache_path)) {
  ahupuaa_cache <- read_csv(ahupuaa_cache_path, show_col_types = FALSE)
} else {
  ahupuaa_cache <- tibble(
    sample_id = character(),
    ahupuaa   = character(),
    moku      = character(),
    mokupuni  = character()
  )
}

# Identify samples not yet assigned
to_assign <- df_clean |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  anti_join(ahupuaa_cache, by = "sample_id") |>
  select(sample_id, latitude, longitude)

# Spatial join for any new samples
if (nrow(to_assign) > 0) {
  message("Assigning ahupuaʻa to ", nrow(to_assign), " new sample(s)...")

  pts <- st_as_sf(to_assign, coords = c("longitude", "latitude"),
                  crs = 4326, remove = FALSE)

  # Pass 1: exact spatial containment (sample within ahupuaʻa polygon)
  # Note: one pair of overlapping polygons exists in the boundary file.
  # arrange + distinct keeps the non-NA ahupuaa when a point falls in the
  # overlap zone, preventing rows_update from erroring on duplicate keys.
  within_join <- st_join(
    pts,
    ahupuaa_sf |> select(ahupuaa, moku, mokupuni),
    join = st_within
  ) |>
    st_drop_geometry() |>
    arrange(sample_id, is.na(ahupuaa)) |>
    distinct(sample_id, .keep_all = TRUE)

  # Pass 2: nearest-feature fallback for offshore/open-water samples
  unmatched <- within_join |> filter(is.na(ahupuaa)) |> pull(sample_id)

  if (length(unmatched) > 0) {
    pts_unmatched <- pts |> filter(sample_id %in% unmatched)
    nearest_idx   <- st_nearest_feature(pts_unmatched, ahupuaa_sf)
    nearest_vals  <- ahupuaa_sf |>
      st_drop_geometry() |>
      select(ahupuaa, moku, mokupuni) |>
      slice(nearest_idx)

    # Determine which unmatched samples are within the 5 km coastal zone.
    # Buffer containment is more reliable than st_distance with s2 geometry,
    # which can return inflated values due to polygon winding-order issues.
    in_coastal_zone <- lengths(
      st_within(pts_unmatched, ahupuaa_coastal_zone)
    ) > 0

    nearest_join <- bind_cols(
      pts_unmatched |> st_drop_geometry() |> select(sample_id),
      nearest_vals,
      tibble(in_coastal_zone = in_coastal_zone)
    ) |>
      mutate(
        ahupuaa = if_else(!in_coastal_zone, "Open Ocean", ahupuaa),
        moku    = if_else(!in_coastal_zone, "Open Ocean", moku)
      ) |>
      select(-in_coastal_zone)

    within_join <- within_join |>
      rows_update(nearest_join, by = "sample_id")
  }

  new_assignments <- within_join |>
    select(sample_id, ahupuaa, moku, mokupuni)

  ahupuaa_cache <- bind_rows(ahupuaa_cache, new_assignments)
  write_csv(ahupuaa_cache, ahupuaa_cache_path)
  message("Ahupuaʻa cache updated.")
}

# Join ahupuaʻa fields back to df_clean
df_clean <- df_clean |>
  left_join(
    ahupuaa_cache |> select(sample_id, ahupuaa, moku, mokupuni),
    by = "sample_id"
  )


# --------------------------------------------------------------
# 8. Read config values from Config sheet tab
# --------------------------------------------------------------
#
# In your Google Sheet, add a tab named "Config" with two columns:
#   key                 | value
#   samples_analyzed    | 0
#   nutrients_analyzed  | 0
#
# Update values whenever a new batch of results comes back from
# the lab — the app will pick them up on the next data refresh.

config_df <- tryCatch(
  read_sheet(sheet_url, sheet = "Config", col_names = TRUE),
  error = function(e) tibble(key = character(), value = character())
)

get_config <- function(key, default = 0L) {
  val <- config_df$value[config_df$key == key]
  if (length(val) == 0 || is.na(val)) return(default)
  as.integer(val)
}

samples_analyzed   <- get_config("samples_analyzed")
nutrients_analyzed <- get_config("nutrients_analyzed")


# --------------------------------------------------------------
# 9. Interactive map for visual coordinate verification
#    (only runs in interactive sessions, not when sourced by the app)
# --------------------------------------------------------------

if (interactive()) {
  library(leaflet)
  leaflet(df_clean) |>
    addProviderTiles(providers$Esri.WorldImagery) |>
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      label = ~sample_id,
      radius = 6,
      color = "white",
      fillColor = "cyan",
      fillOpacity = 0.8,
      weight = 1.5
    )
}
