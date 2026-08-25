# ============================================================
# Wave fetch index + population density per ahupuaʻa
#
# Outputs saved to data/:
#   wave_fetch.csv          — mean & directional fetch per sample (km)
#   pop_density_ahupuaa.csv — population per km² per ahupuaʻa (Census ACS)
#
# Wave fetch: 8-direction ray casting against Hawaii land polygon
#   (union of ahupuaa_raw boundaries) in UTM Zone 4N.
#   Fetch in each direction = distance to nearest land (capped at 300 km
#   for open-ocean directions). fetch_mean_km = mean across 8 directions.
#
# Population: requires a free Census API key:
#   https://api.census.gov/data/key_signup.html
#   census_api_key("YOUR_KEY", install = TRUE)   # run once
#
# Author: Nyssa Silbiger
# Date:   2026-08-24
# ============================================================

library(sf)
library(tidyverse)
library(here)

# ── Shared data ───────────────────────────────────────────────────────────────
if (!exists("ahupuaa_raw")) {
  ahupuaa_raw <- st_read("data/ahupuaa_boundaries.geojson", quiet = TRUE) |>
    st_transform(4326)
}

if (!exists("chem")) source("01_load_clean_data.R", local = TRUE)

# ================================================================
# Part 1: Wave fetch index
# ================================================================

# ── Hawaii land polygon ───────────────────────────────────────────────────────
hi_land <- ahupuaa_raw |>
  st_transform(32604) |>
  st_union() |>
  st_simplify(dTolerance = 200)  # 200 m tolerance for speed

land_geom <- hi_land[[1]]          # bare sfg for st_intersection

# ── Sample points in UTM ─────────────────────────────────────────────────────
samples_fetch <- chem |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  distinct(sample_id, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_transform(32604)

message("Computing fetch for ", nrow(samples_fetch), " samples ...")

# ── Core function: fetch in one compass direction ─────────────────────────────
fetch_one_direction <- function(pts_sfc, land_sfg, bearing_deg, max_dist = 300000) {
  coords   <- st_coordinates(pts_sfc)
  n        <- nrow(coords)
  land_sfc <- st_sfc(land_sfg, crs = st_crs(pts_sfc))

  dx <- max_dist * sin(bearing_deg * pi / 180)
  dy <- max_dist * cos(bearing_deg * pi / 180)

  rays <- st_sfc(
    lapply(seq_len(n), function(i)
      st_linestring(matrix(c(coords[i,1], coords[i,2],
                             coords[i,1]+dx, coords[i,2]+dy),
                           2, 2, byrow = TRUE))),
    crs = st_crs(pts_sfc)
  )

  hits  <- lengths(st_intersects(rays, land_sfc)) > 0
  fetch <- rep(max_dist, n)

  for (i in which(hits)) {
    g <- tryCatch(
      st_intersection(rays[[i]], land_sfg),
      error = function(e) st_geometrycollection()
    )
    if (!st_is_empty(g)) {
      pts_i <- suppressWarnings(st_cast(st_sfc(g, crs = st_crs(pts_sfc)), "POINT"))
      if (length(pts_i) > 0) {
        d <- as.numeric(st_distance(pts_sfc[i, ], pts_i))
        d <- d[d > 100]
        if (length(d) > 0) fetch[i] <- min(d)
      }
    }
  }
  fetch
}

# ── Run 8 directions ──────────────────────────────────────────────────────────
directions <- c(0, 45, 90, 135, 180, 225, 270, 315)
dir_names  <- c("N","NE","E","SE","S","SW","W","NW")

fetch_list <- map2(directions, dir_names, function(bear, nm) {
  message("  ", nm, " (", bear, " deg)...")
  fetch_one_direction(samples_fetch, land_geom, bear)
})

fetch_mat_km <- do.call(cbind, fetch_list) / 1000
colnames(fetch_mat_km) <- paste0("fetch_", dir_names, "_km")

wave_fetch <- tibble(
  sample_id     = samples_fetch$sample_id,
  fetch_mean_km = rowMeans(fetch_mat_km),
  fetch_min_km  = apply(fetch_mat_km, 1, min)
) |>
  bind_cols(as_tibble(fetch_mat_km))

write_csv(wave_fetch, here("data", "wave_fetch.csv"))
message("Saved → data/wave_fetch.csv  (", nrow(wave_fetch), " samples)")
message("  fetch_mean_km: ", round(min(wave_fetch$fetch_mean_km), 1),
        " – ", round(max(wave_fetch$fetch_mean_km), 1), " km")

# ================================================================
# Part 2: Population density per ahupuaʻa (requires Census API key)
# ================================================================
# Run: census_api_key("YOUR_KEY", install = TRUE)   then restart R
# ─────────────────────────────────────────────────────────────────

if (requireNamespace("tidycensus", quietly = TRUE) &&
    Sys.getenv("CENSUS_API_KEY") != "") {

  library(tidycensus)

  message("Downloading ACS 2022 5-year block group population for Hawaii ...")
  hi_bg <- get_acs(
    geography = "block group",
    variables = "B01003_001",
    state     = "HI",
    year      = 2022,
    geometry  = TRUE,
    progress  = FALSE
  ) |>
    st_transform(32604) |>
    rename(population = estimate) |>
    mutate(bg_area_m2 = as.numeric(st_area(geometry)))

  # ── Areal interpolation: apportion block group population to ahupuaʻa ─────
  message("Apportioning population to ahupuaʻa boundaries ...")
  ahupuaa_utm <- ahupuaa_raw |> st_transform(32604) |>
    select(ahupuaa, mokupuni) |>
    rename(island = mokupuni)

  pop_int <- st_intersection(ahupuaa_utm, hi_bg) |>
    mutate(
      int_area_m2  = as.numeric(st_area(geometry)),
      pop_in_int   = population * (int_area_m2 / bg_area_m2)
    ) |>
    st_drop_geometry()

  ahupuaa_area <- ahupuaa_utm |>
    mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
    st_drop_geometry() |>
    group_by(ahupuaa, island) |>
    summarise(area_km2 = sum(area_km2), .groups = "drop")

  pop_density_ahupuaa <- pop_int |>
    group_by(ahupuaa, island) |>
    summarise(total_pop = sum(pop_in_int, na.rm = TRUE), .groups = "drop") |>
    left_join(ahupuaa_area, by = c("ahupuaa", "island")) |>
    mutate(pop_density_km2 = total_pop / area_km2)

  write_csv(pop_density_ahupuaa, here("data", "pop_density_ahupuaa.csv"))
  message("Saved → data/pop_density_ahupuaa.csv  (",
          nrow(pop_density_ahupuaa), " ahupuaʻa)")

} else {
  message("Census API key not found — skipping population density.")
  message("To enable: census_api_key('YOUR_KEY', install = TRUE) then re-run.")
}
