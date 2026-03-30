# ============================================================
# What's In Your Water? - Ocean Sample Submission Data
# Load and clean submission form data from Google Sheets
#
# Author: Nyssa Silbiger
# Date:   2026-03-28
# ============================================================

library(tidyverse)
library(googlesheets4)


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
    sample_id = trimws(`Sample Bottle ID from the label (example OA1 for O'ahu, M1 for Maui)`),

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
# 6. Interactive map for visual coordinate verification
# --------------------------------------------------------------

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
