# Isotope analysis, step 1: build water_all / water_iso and audit the isotope join.
#
# Inputs : data/chem_master.csv (written by the Data Assembly section of
#          kona_low_nutrient_analysis.qmd; already carries N15/O18, cess_exposure_idx and
#          the fDOM columns), data/isotope.csv (used only for the join audit)
# Outputs: output/isotope/data/water_all.{rds,csv}, water_iso.{rds,csv},
#          output/isotope/tables/*.csv
# Does not modify data/chem_master.csv or data/isotope.csv.
# To pick up new isotope values: add them to data/isotope.csv, re-run the Data Assembly
# section of the main qmd (rewrites chem_master.csv), then re-run this script.
# N15 is measured on NOx (nitrate + nitrite); water was filtered and frozen.

library(tidyverse)
library(here)

normalize_island <- function(x) str_replace_all(x, "['\u2018\u2019\u02bc]", "'")
norm_id <- function(x) str_replace_all(x, "\\s+", "")
island_levels <- c("O'ahu", "Maui", "Moloka'i", "L\u0101na'i")

dir.create(here("output", "isotope", "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "isotope", "tables"), recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load -------------------------------------------------------------------------------
base <- read_csv(here("data", "chem_master.csv"), show_col_types = FALSE)
stopifnot(all(c("N15", "O18", "cess_exposure_idx") %in% names(base)))
iso_raw <- read_csv(here("data", "isotope.csv"), show_col_types = FALSE) |>
  mutate(sample_id_norm = norm_id(sample_id))

# ---- 2. Isotope-to-sample matching audit ---------------------------------------------------
base <- base |> mutate(sample_id_norm = norm_id(sample_id))

iso_audit <- iso_raw |>
  mutate(
    n_in_isotope_file = ave(seq_along(sample_id_norm), sample_id_norm, FUN = length),
    matched = sample_id_norm %in% base$sample_id_norm,
    whitespace_altered = sample_id != sample_id_norm
  )
write_csv(iso_audit, here("output", "isotope", "tables", "isotope_join_audit.csv"))

stopifnot(!anyDuplicated(base$sample_id_norm))   # normalised IDs must stay unique in chem

# Unmatched IDs: list near-miss candidates for the user to review; never auto-corrected.
unmatched <- iso_audit |> filter(!matched) |> pull(sample_id_norm)
near_miss <- map_dfr(unmatched, \(u) {
  prefix <- str_extract(u, "^[A-Za-z]+")
  num <- as.integer(str_extract(u, "\\d+"))
  base |>
    filter(str_starts(sample_id_norm, prefix)) |>
    mutate(num = as.integer(str_extract(sample_id_norm, "\\d+")), unmatched_id = u) |>
    filter(abs(num - !!num) <= 1) |>
    select(unmatched_id, candidate = sample_id, island, collected_hst)
})

# ---- 3. Build water_all --------------------------------------------------------------------
# GAM flag reproduces the NO3+NO2 GAM data filter used in kona_low_nutrient_analysis.qmd
# (step12-nox-data): not offshore, non-BDL NO3NO2 and SiO2, complete GAM predictors.
gam_raw_vars <- c("NO3NO2", "SiO2", "chirps_storm_cumulative_mm", "dist_to_stream_mouth_m",
                  "mean_slope_deg", "mean_ksat_um_s", "pct_agriculture", "pct_impervious",
                  "dist_to_nearest_npdes_m", "cess_exposure_idx", "fetch_mean_km", "hs_mean_m")

# N15/O18 already merged into chem_master (repeat runs averaged there).
water_all <- base |>
  mutate(
    island = factor(normalize_island(island), levels = island_levels),
    nitrite_umol = suppressWarnings(as.numeric(`NO2 (umol/L)`)),
    nitrite_bdl = `NO2 (umol/L)` == "<0.07",
    log_NO3NO2 = log(NO3NO2),
    log_SiO2 = log(SiO2),
    log_dist_stream_m = log(dist_to_stream_mouth_m + 1),
    log_ksat = log(mean_ksat_um_s + 1),
    log_dist_npdes_m = log(dist_to_nearest_npdes_m + 1),
    log_cess_exposure = log1p(cess_exposure_idx),
    ahupuaa_key = paste(ahupuaa, mokupuni, sep = "|"),
    has_iso = !is.na(N15),
    in_gam = !offshore_reference & !is.na(NO3NO2) & !bdl_NO3NO2 & !is.na(SiO2) & !bdl_SiO2 &
      if_all(all_of(gam_raw_vars), \(x) !is.na(x))
  )

# z-scores using the GAM subset's mean/SD for every sample, so isotope-model coefficients
# are on the same standardized scale as the Figure 5 GAM.
gam_z_vars <- c("log_SiO2", "chirps_storm_cumulative_mm", "log_dist_stream_m", "mean_slope_deg",
                "log_ksat", "pct_agriculture", "pct_impervious", "log_dist_npdes_m",
                "log_cess_exposure", "fetch_mean_km", "hs_mean_m")
z_ref <- water_all |> filter(in_gam) |>
  summarise(across(all_of(gam_z_vars), list(mean = mean, sd = sd))) |>
  pivot_longer(everything(), names_to = c("var", ".value"), names_pattern = "(.*)_(mean|sd)$")
write_csv(z_ref, here("output", "isotope", "tables", "gam_zscore_reference.csv"))

for (v in gam_z_vars) {
  r <- z_ref[z_ref$var == v, ]
  water_all[[paste0("z_", v)]] <- (water_all[[v]] - r$mean) / r$sd
}

water_all <- water_all |> select(-sample_id_norm)
water_iso <- water_all |> filter(has_iso)

# ---- 4. Save -------------------------------------------------------------------------------
saveRDS(water_all, here("output", "isotope", "data", "water_all.rds"))
saveRDS(water_iso, here("output", "isotope", "data", "water_iso.rds"))
write_csv(water_all, here("output", "isotope", "data", "water_all.csv"))
write_csv(water_iso, here("output", "isotope", "data", "water_iso.csv"))
if (nrow(near_miss) > 0) {
  write_csv(near_miss, here("output", "isotope", "tables", "isotope_unmatched_candidates.csv"))
}

message("water_all: ", nrow(water_all), " | water_iso: ", nrow(water_iso),
        " | in_gam: ", sum(water_all$in_gam), " | iso in GAM: ", sum(water_iso$in_gam))
