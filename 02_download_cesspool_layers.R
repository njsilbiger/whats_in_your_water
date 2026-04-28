# ============================================================
# Download & process Hawaiʻi HCPT Cesspool point data
#
# Source: Hawaiʻi Statewide GIS Program
#   https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer
#   Layer 32: HCPT Cesspools (individual point locations, n ≈ 82,141)
#
# Outputs saved to data/:
#   cesspool_points.rds             — raw sf point layer (cached)
#   sample_cesspool_distances.csv   — distance (m) to nearest cesspool per sample
#   cesspool_by_ahupuaa.csv         — cesspool count per ahupuaʻa / moku / island
#
# Run once locally; the app and analysis scripts load the cached CSVs.
#
# Author: Nyssa Silbiger
# Date:   2026-04-28
# ============================================================

library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(readr)

BASE_URL  <- "https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer"
LAYER_ID  <- 32          # HCPT Cesspools (individual points)
PAGE_SIZE <- 1000        # ArcGIS REST max per request

# ----------------------------------------------------------------
# Helper: fetch all features with automatic pagination
# ----------------------------------------------------------------

fetch_all_features <- function(base_url, layer_id, page_size = PAGE_SIZE) {
  # Total count
  count_url <- paste0(
    base_url, "/", layer_id,
    "/query?where=1%3D1&returnCountOnly=true&f=json"
  )
  total <- fromJSON(rawToChar(GET(count_url, timeout(30))$content))$count
  message("  Layer ", layer_id, " — total features: ", total)

  pages   <- vector("list", ceiling(total / page_size))
  offset  <- 0
  page    <- 1

  repeat {
    end <- min(offset + page_size, total)
    message("  Fetching records ", offset + 1, "\u2013", end, " ...")

    url <- paste0(
      base_url, "/", layer_id, "/query",
      "?where=1%3D1",
      "&outFields=objectid,uid,x,y,island",   # only fields we need
      "&returnGeometry=true",
      "&resultOffset=",      offset,
      "&resultRecordCount=", page_size,
      "&f=geojson"
    )

    resp  <- GET(url, timeout(90))
    stop_for_status(resp)
    chunk <- tryCatch(
      read_sf(rawToChar(resp$content)),
      error = function(e) stop("GeoJSON parse failed at offset ", offset, ": ", e$message)
    )

    if (nrow(chunk) == 0) break
    pages[[page]] <- chunk
    offset <- offset + nrow(chunk)
    page   <- page + 1
    if (offset >= total) break
  }

  out <- bind_rows(Filter(Negate(is.null), pages))
  st_crs(out) <- 4326
  out
}

# ================================================================
# Step 1: Download (or load from cache) cesspool points
#
# NOTE: Layer 32 (HCPT Cesspools) covers BI, Kauaʻi, Maui, Oʻahu only.
# Molokaʻi is supplemented from Layer 23 (OSDS Molokaʻi), filtered to
# Class I systems (cesspools / direct-discharge pits).
# Lānaʻi has no cesspool layer available in this GIS service.
# ================================================================

cesspool_cache <- "data/cesspool_points.rds"

if (file.exists(cesspool_cache)) {
  message("Loading cesspool points from cache: ", cesspool_cache)
  cesspools <- readRDS(cesspool_cache)
  message("  ", nrow(cesspools), " features loaded.")
} else {
  message("Downloading HCPT Cesspool points (layer ", LAYER_ID, ") ...")
  cesspools <- fetch_all_features(BASE_URL, LAYER_ID)
  saveRDS(cesspools, cesspool_cache)
  message("Cached to: ", cesspool_cache)
}

# ── Supplement with Molokaʻi (layer 23, Class I OSDS = cesspools) ──────
molokai_cache <- "data/molokai_cesspools.rds"

if (file.exists(molokai_cache)) {
  message("Loading Molokaʻi cesspools from cache: ", molokai_cache)
  molokai_cp <- readRDS(molokai_cache)
} else {
  message("Downloading Molokaʻi OSDS (layer 23) ...")
  molokai_all <- fetch_all_features(BASE_URL, 23)
  molokai_cp  <- molokai_all |>
    filter(class_i > 0) |>
    select(geometry)
  saveRDS(molokai_cp, molokai_cache)
  message("Molokaʻi Class-I cesspools: ", nrow(molokai_cp))
}

# Combined layer used for all distance and summary calculations
all_cesspools <- bind_rows(
  cesspools |> select(geometry),
  molokai_cp
)
message("Combined cesspool points: ", nrow(all_cesspools),
        "  (layer 32: ", nrow(cesspools), " + Molokaʻi: ", nrow(molokai_cp), ")")

# ================================================================
# Step 2: Spatial join cesspools → ahupuaʻa
#
# We re-use the ahupuaʻa boundaries downloaded by 01_load_clean_data.R.
# Each cesspool gets ahupuaa / moku / mokupuni (island) labels.
# ================================================================

ahupuaa_bounds_path <- "data/ahupuaa_boundaries.geojson"
if (!exists("ahupuaa_sf")) {
  message("Loading ahupuaʻa boundaries ...")
  ahupuaa_sf <- st_read(ahupuaa_bounds_path, quiet = TRUE) |>
    st_transform(crs = 4326)
}

message("Joining cesspools to ahupuaʻa polygons ...")
cesspools_4326 <- st_transform(all_cesspools, 4326)
ahupuaa_simple <- ahupuaa_sf |> select(ahupuaa, moku, mokupuni)

# Pass 1: exact containment
cesspool_ahupuaa <- st_join(
  cesspools_4326,
  ahupuaa_simple,
  join = st_within
) |>
  st_drop_geometry() |>
  # If a point falls in an overlap zone it may get two rows; keep non-NA
  arrange(objectid, is.na(ahupuaa)) |>
  distinct(objectid, .keep_all = TRUE)

# Pass 2: nearest-feature fallback for any unmatched points
unmatched_ids <- cesspool_ahupuaa |> filter(is.na(ahupuaa)) |> pull(objectid)
message("  Unmatched cesspools (fallback to nearest ahupuaʻa): ", length(unmatched_ids))

if (length(unmatched_ids) > 0) {
  unmatched_pts <- cesspools_4326 |> filter(objectid %in% unmatched_ids)
  nearest_idx   <- st_nearest_feature(unmatched_pts, ahupuaa_sf)
  nearest_vals  <- ahupuaa_sf |>
    st_drop_geometry() |>
    select(ahupuaa, moku, mokupuni) |>
    slice(nearest_idx)

  fallback <- bind_cols(
    unmatched_pts |> st_drop_geometry() |> select(objectid, uid, island),
    nearest_vals
  )

  cesspool_ahupuaa <- cesspool_ahupuaa |>
    rows_update(fallback, by = "objectid")
}

# ================================================================
# Step 3: Summary — cesspool count per ahupuaʻa
# ================================================================

message("Summarising cesspools by ahupuaʻa ...")

ahupuaa_area <- ahupuaa_sf |>
  st_transform(32604) |>   # UTM zone 4N for accurate m² areas
  mutate(area_m2 = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  select(ahupuaa, moku, area_m2) |>
  # Some ahupuaʻa have multiple polygon features (main body + tiny slivers).
  # Sum all parts so the join doesn't fan out and inflate density values.
  group_by(ahupuaa, moku) |>
  summarise(area_m2 = sum(area_m2), .groups = "drop")

cesspool_by_ahupuaa <- cesspool_ahupuaa |>
  filter(!is.na(ahupuaa)) |>
  group_by(ahupuaa, moku, island = mokupuni) |>
  summarise(n_cesspools = n(), .groups = "drop") |>
  left_join(ahupuaa_area, by = c("ahupuaa", "moku")) |>
  mutate(cesspool_density_per_km2 = n_cesspools / (area_m2 / 1e6)) |>
  arrange(desc(n_cesspools))

write_csv(cesspool_by_ahupuaa, "data/cesspool_by_ahupuaa.csv")
message("Saved → data/cesspool_by_ahupuaa.csv  (",
        nrow(cesspool_by_ahupuaa), " ahupuaʻa)")
print(head(cesspool_by_ahupuaa, 10))

# ================================================================
# Step 4: Distance to nearest cesspool for each sample bottle
#
# Uses UTM Zone 4N (EPSG:32604) for accurate metre distances.
# Requires df_clean (from 01_load_clean_data.R) or loads it fresh.
# ================================================================

if (!exists("df_clean")) {
  message("Loading sample data via 01_load_clean_data.R ...")
  source("01_load_clean_data.R", local = TRUE)
}

samples_with_coords <- df_clean |>
  filter(!is.na(latitude), !is.na(longitude))

message("Computing distance to nearest cesspool for ",
        nrow(samples_with_coords), " samples ...")

# Project both layers to UTM Zone 4N (metres)
UTM4N <- 32604

samples_utm <- st_as_sf(
  samples_with_coords,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
) |>
  st_transform(UTM4N)

cesspools_utm <- st_transform(all_cesspools, UTM4N)

# Find nearest cesspool index and compute the actual distance
nearest_idx  <- st_nearest_feature(samples_utm, cesspools_utm)
nearest_geom <- st_geometry(cesspools_utm)[nearest_idx]
distances_m  <- as.numeric(st_distance(
  st_geometry(samples_utm),
  nearest_geom,
  by_element = TRUE
))

sample_cesspool_distances <- tibble(
  sample_id                  = samples_with_coords$sample_id,
  dist_to_nearest_cesspool_m = round(distances_m, 1)
)

write_csv(sample_cesspool_distances, "data/sample_cesspool_distances.csv")
message("Saved → data/sample_cesspool_distances.csv  (",
        nrow(sample_cesspool_distances), " samples)")

# ================================================================
# Summary
# ================================================================

message("\n=== Processing complete ===")
message("Cesspool points:              ", nrow(all_cesspools),
        "  (layer 32: ", nrow(cesspools), " + Molokaʻi: ", nrow(molokai_cp), ")")
message("Ahupuaʻa summary:             data/cesspool_by_ahupuaa.csv")
message("Sample distances:             data/sample_cesspool_distances.csv")
message("\nDistance summary (metres):")
print(summary(sample_cesspool_distances$dist_to_nearest_cesspool_m))
