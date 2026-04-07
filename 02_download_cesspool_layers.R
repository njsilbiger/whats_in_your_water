# ============================================================
# Download & cache Hawaiʻi Cesspool Prioritization Tool (HCPT) layers
#
# Source: Hawaiʻi Statewide GIS Program
#   https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer
#
# Run this script once locally to cache the layers in data/.
# The app loads the cached files directly — no live API calls at runtime.
#
# Layers downloaded:
#   - cesspool_priority_tracts.geojson    (census tracts,      ~300 features)
#   - cesspool_priority_blockgroups.geojson (census block groups, ~800 features)
#
# Author: Nyssa Silbiger
# Date:   2026-04-06
# ============================================================

library(httr)
library(jsonlite)
library(sf)
library(dplyr)

BASE_URL <- "https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer"

# ----------------------------------------------------------------
# Helper: discover layer list from the MapServer
# ----------------------------------------------------------------

get_layer_list <- function(base_url) {
  resp <- GET(paste0(base_url, "?f=json"), timeout(30))
  stop_for_status(resp)
  parsed <- fromJSON(rawToChar(resp$content))
  parsed$layers[, c("id", "name")]
}

# ----------------------------------------------------------------
# Helper: query total feature count for a layer
# ----------------------------------------------------------------

get_feature_count <- function(base_url, layer_id) {
  url <- paste0(
    base_url, "/", layer_id, "/query",
    "?where=1%3D1&returnCountOnly=true&f=json"
  )
  resp <- GET(url, timeout(30))
  stop_for_status(resp)
  fromJSON(rawToChar(resp$content))$count
}

# ----------------------------------------------------------------
# Helper: fetch all features for a layer, paging as needed.
#
# ArcGIS REST services cap results per request (often 1000).
# This function pages through until all features are collected.
# ----------------------------------------------------------------

fetch_all_features <- function(base_url, layer_id, page_size = 1000) {

  # 1. Get total count
  total <- get_feature_count(base_url, layer_id)
  message("  Total features: ", total)

  all_features <- vector("list", ceiling(total / page_size))
  offset <- 0
  page   <- 1

  repeat {
    message("  Fetching records ", offset + 1, "–", min(offset + page_size, total), " ...")

    url <- paste0(
      base_url, "/", layer_id, "/query",
      "?where=1%3D1",
      "&outFields=*",
      "&returnGeometry=true",
      "&resultOffset=",      offset,
      "&resultRecordCount=", page_size,
      "&f=geojson"
    )

    resp <- GET(url, timeout(60))
    stop_for_status(resp)

    chunk <- tryCatch(
      read_sf(rawToChar(resp$content)),
      error = function(e) stop("Failed to parse GeoJSON chunk at offset ", offset, ": ", e$message)
    )

    if (nrow(chunk) == 0) break

    all_features[[page]] <- chunk
    offset <- offset + nrow(chunk)
    page   <- page + 1

    if (offset >= total) break
  }

  # Combine pages and ensure consistent CRS
  combined <- bind_rows(Filter(Negate(is.null), all_features))
  st_crs(combined) <- 4326
  combined
}

# ----------------------------------------------------------------
# Step 1: Discover available layers (run once to orient yourself)
# ----------------------------------------------------------------

message("Fetching MapServer layer list ...")
layers <- get_layer_list(BASE_URL)
print(layers)

# Look for cesspool-related layers
cesspool_layers <- layers[grepl("HCPT|[Cc]esspool", layers$name), ]
message("\nCesspool-related layers found:")
print(cesspool_layers)

# ----------------------------------------------------------------
# Step 2: Download the polygon aggregate layers
#
# Adjust layer IDs below if the service has changed.
# Confirm IDs from the printed table above or browse:
#   https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer
# ----------------------------------------------------------------

# Expected: something like "HCPT Priority Census Tracts"       → tracts
#           something like "HCPT Priority Census Block Groups"  → block groups
# Layer 34 confirmed as block groups from service metadata.
# Adjust TRACT_ID if needed after checking the layer list above.

BLOCKGROUP_ID <- 34   # HCPT Priority Census Block Groups
TRACT_ID      <- 33   # HCPT Priority Census Tracts (adjust if needed)

# ---- Block groups ----
message("\nDownloading census block group layer (ID: ", BLOCKGROUP_ID, ") ...")
blockgroups <- fetch_all_features(BASE_URL, BLOCKGROUP_ID)

message("Block group columns: ", paste(names(blockgroups), collapse = ", "))
message("Block group preview:")
print(head(as.data.frame(blockgroups)[, setdiff(names(blockgroups), "geometry")], 5))

write_sf(blockgroups, "data/cesspool_priority_blockgroups.geojson", delete_dsn = TRUE)
message("Saved → data/cesspool_priority_blockgroups.geojson")

# ---- Census tracts ----
message("\nDownloading census tract layer (ID: ", TRACT_ID, ") ...")
tracts <- fetch_all_features(BASE_URL, TRACT_ID)

message("Tract columns: ", paste(names(tracts), collapse = ", "))
message("Tract preview:")
print(head(as.data.frame(tracts)[, setdiff(names(tracts), "geometry")], 5))

write_sf(tracts, "data/cesspool_priority_tracts.geojson", delete_dsn = TRUE)
message("Saved → data/cesspool_priority_tracts.geojson")

# ----------------------------------------------------------------
# Step 3: Verify what was saved
# ----------------------------------------------------------------

message("\n=== Download summary ===")
message("Block groups: ", nrow(blockgroups), " features | ",
        round(file.size("data/cesspool_priority_blockgroups.geojson") / 1024), " KB")
message("Tracts:       ", nrow(tracts),      " features | ",
        round(file.size("data/cesspool_priority_tracts.geojson")      / 1024), " KB")

message("\nBlock group field names:\n  ", paste(names(blockgroups), collapse = "\n  "))
message("\nPriority level distribution (block groups):")
print(table(blockgroups$PRIORITY_LEVEL, useNA = "ifany"))
