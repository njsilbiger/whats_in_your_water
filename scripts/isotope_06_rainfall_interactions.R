# Isotope analysis, step 8: rainfall x cesspool and rainfall x agriculture, tested as prespecified models.
#
# (A) Joint mixing + regression model (as in step 7), saved with intercept and variance parameters so the
#     predicted waste-associated fraction can be plotted against rainfall at low and high exposure.
#     Models: M4a (rain x cesspool), M4b (rain x agriculture), M5 (both interactions together).
# (B) Observed d15N as the response (brms), which makes no mixing-model assumptions; changes in d15N can reflect
#     nitrogen transformations as well as source changes.
# Outputs: output/isotope/tables/rain_*.csv, output/isotope/model_objects/rain_*.rds

library(tidyverse)
library(here)
library(rstan)
library(posterior)
library(brms)

SEED <- 2847
set.seed(SEED)
options(mc.cores = 4)
rstan_options(auto_write = FALSE)
tab_dir <- here("output", "isotope", "tables")
mod_dir <- here("output", "isotope", "model_objects")

water_iso <- readRDS(here("output", "isotope", "data", "water_iso.rds")) |>
  mutate(maui = as.numeric(island == "Maui"), molokai = as.numeric(island == "Moloka'i"), lanai = as.numeric(island == "L\u0101na'i"),
         ahup_i = as.integer(factor(ahupuaa_key)),
         rain_x_cess = z_chirps_storm_cumulative_mm * z_log_cess_exposure,
         rain_x_ag = z_chirps_storm_cumulative_mm * z_pct_agriculture)
zref <- read_csv(file.path(tab_dir, "gam_zscore_reference.csv"), show_col_types = FALSE)
rain_ref <- zref[zref$var == "chirps_storm_cumulative_mm", ]

pri <- read_csv(file.path(tab_dir, "endmember_priors.csv"), show_col_types = FALSE)
pool <- function(groups) {
  g <- pri[pri$group %in% groups, ]
  c(m = mean(g$mean), sd = sqrt(mean(g$source_sd^2) + var(g$mean)), se = sqrt(sum(g$se_group_mean^2)) / nrow(g))
}
ww <- pri[pri$group == "wastewater", ]
src_w <- c(m = ww$mean, sd = ww$source_sd, se = ww$se_group_mean)
src_o <- pool(c("synthetic_fertilizer", "soil_background", "marine_nitrate"))
enrich <- list(none = c(0, 0), moderate = c(2.5, 1.44))

isl <- c("maui", "molokai", "lanai")
base <- c("z_log_cess_exposure", "z_pct_agriculture", "z_chirps_storm_cumulative_mm")
models <- list(
  M4a_rain_x_cess = c(base, "rain_x_cess", isl),
  M4b_rain_x_ag = c(base, "rain_x_ag", isl),
  M5_both = c(base, "rain_x_cess", "rain_x_ag", isl)
)

# ---- design coverage: where in rainfall x exposure space are the samples? -----------------------------------
q <- \(x, p) quantile(x, p, names = FALSE)
lo_hi <- tibble(
  exposure = c("cesspool", "agriculture"),
  z_lo = c(q(water_iso$z_log_cess_exposure, 0.10), q(water_iso$z_pct_agriculture, 0.10)),
  z_hi = c(q(water_iso$z_log_cess_exposure, 0.90), q(water_iso$z_pct_agriculture, 0.90)),
  z_mid = c(median(water_iso$z_log_cess_exposure), median(water_iso$z_pct_agriculture))
)
cover <- bind_rows(
  water_iso |> mutate(group = case_when(z_log_cess_exposure <= q(z_log_cess_exposure, 0.25) ~ "low", z_log_cess_exposure >= q(z_log_cess_exposure, 0.75) ~ "high", TRUE ~ "middle"),
                      exposure = "cesspool"),
  water_iso |> mutate(group = case_when(z_pct_agriculture <= q(z_pct_agriculture, 0.25) ~ "low", z_pct_agriculture >= q(z_pct_agriculture, 0.75) ~ "high", TRUE ~ "middle"),
                      exposure = "agriculture")
) |>
  mutate(rain_band = cut(chirps_storm_cumulative_mm, c(0, 200, 300, 450, Inf), labels = c("<200 mm", "200-300", "300-450", ">450 mm"))) |>
  count(exposure, group, rain_band) |> pivot_wider(names_from = rain_band, values_from = n, values_fill = 0)
write_csv(cover, file.path(tab_dir, "rain_design_coverage.csv"))
write_csv(lo_hi, file.path(tab_dir, "rain_exposure_levels.csv"))

# ---- (A) joint models --------------------------------------------------------------------------------------
sm <- stan_model(file = here("scripts", "isotope_two_source_mixing_covariates.stan"), model_name = "mixing_covariates")
make_data <- function(cols, en) {
  list(N = nrow(water_iso), y = water_iso$N15, K = length(cols), X = as.matrix(water_iso[, cols]),
       A = max(water_iso$ahup_i), ah = water_iso$ahup_i,
       m_w = src_w[["m"]], se_w = src_w[["se"]], sd_w = src_w[["sd"]], m_o = src_o[["m"]], se_o = src_o[["se"]], sd_o = src_o[["sd"]],
       sd_logsd = 0.25, sigma_a = 0.3, enr_mean = en[1], enr_sd = en[2], prior_beta_sd = 1, prior_only = 0)
}

gh <- list(nodes = qnorm((1:60 - 0.5) / 60), weights = rep(1 / 60, 60))   # equal-weight quadrature for E over a standard normal
pop_mean_fraction <- function(eta, sd_tot) {
  # eta: draws vector; sd_tot: draws vector; returns E[inv_logit(eta + sd_tot * Z)]
  vapply(seq_along(eta), \(d) sum(gh$weights * plogis(eta[d] + sd_tot[d] * gh$nodes)), 0)
}

# prediction grid: rainfall over the 5th-95th percentile of the isotope samples
rain_grid_mm <- seq(q(water_iso$chirps_storm_cumulative_mm, 0.05), q(water_iso$chirps_storm_cumulative_mm, 0.95), length.out = 40)
rain_grid_z <- (rain_grid_mm - rain_ref$mean) / rain_ref$sd

preds <- list(); eff <- list(); k <- 0
for (m in names(models)) for (e in names(enrich)) {
  k <- k + 1
  cols <- models[[m]]
  fit <- rstan::sampling(sm, data = make_data(cols, enrich[[e]]), chains = 4, iter = 3000, warmup = 1500, seed = SEED + 600 + k,
                         control = list(adapt_delta = 0.99, max_treedepth = 12), refresh = 0,
                         pars = c("mu_w", "mu_o", "s_w", "s_o", "alpha0", "beta", "sigma_f", "sigma_ah"))
  d <- rstan::extract(fit, c("alpha0", "beta", "sigma_f", "sigma_ah"))
  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  arr <- as_draws_array(fit)
  sdg <- summarise_draws(subset_draws(arr, variable = c("alpha0", "beta", "sigma_f", "sigma_ah")), "rhat", "ess_bulk")
  dg <- tibble(n_divergent = sum(vapply(sp, \(x) sum(x[, "divergent__"]), 0)), max_rhat = max(sdg$rhat), min_ess_bulk = min(sdg$ess_bulk),
               min_ebfmi = min(rstan::get_bfmi(fit)))
  saveRDS(list(model = m, enrichment = e, cols = cols, alpha0 = d$alpha0, beta = d$beta, sigma_f = d$sigma_f, sigma_ah = d$sigma_ah),
          file.path(mod_dir, paste0("rain_joint_", m, "_", e, ".rds")))
  sd_tot <- sqrt(d$sigma_f^2 + d$sigma_ah^2)
  ix <- setNames(seq_along(cols), cols)
  medz <- list(z_log_cess_exposure = median(water_iso$z_log_cess_exposure), z_pct_agriculture = median(water_iso$z_pct_agriculture))

  for (ex in c("cesspool", "agriculture")) {
    zcol <- if (ex == "cesspool") "z_log_cess_exposure" else "z_pct_agriculture"
    int_col <- if (ex == "cesspool") "rain_x_cess" else "rain_x_ag"
    if (!(int_col %in% cols)) next
    lh <- lo_hi[lo_hi$exposure == ex, ]
    for (lev in c("low", "high")) {
      zl <- if (lev == "low") lh$z_lo else lh$z_hi
      out <- map_dfr(seq_along(rain_grid_z), \(g) {
        zr <- rain_grid_z[g]
        x <- setNames(rep(0, length(cols)), cols)
        x["z_log_cess_exposure"] <- medz$z_log_cess_exposure; x["z_pct_agriculture"] <- medz$z_pct_agriculture
        x[zcol] <- zl; x["z_chirps_storm_cumulative_mm"] <- zr
        if ("rain_x_cess" %in% cols) x["rain_x_cess"] <- zr * x["z_log_cess_exposure"]
        if ("rain_x_ag" %in% cols) x["rain_x_ag"] <- zr * x["z_pct_agriculture"]
        eta <- d$alpha0 + as.vector(d$beta %*% x)
        fr <- pop_mean_fraction(eta, sd_tot)
        tibble(rain_mm = rain_grid_mm[g], fraction = mean(fr), lwr = quantile(fr, 0.025), upr = quantile(fr, 0.975))
      })
      preds[[length(preds) + 1]] <- bind_cols(tibble(model = m, enrichment = e, exposure = ex, level = lev, z_exposure = zl), out)
    }
  }

  # contrast: slope of the waste fraction against rainfall (per SD of rainfall) at high minus low exposure
  for (ex in c("cesspool", "agriculture")) {
    int_col <- if (ex == "cesspool") "rain_x_cess" else "rain_x_ag"
    if (!(int_col %in% cols)) next
    lh <- lo_hi[lo_hi$exposure == ex, ]
    b_int <- d$beta[, ix[[int_col]]]
    delta_z <- lh$z_hi - lh$z_lo
    contrast <- b_int * delta_z                      # change in the rainfall slope on the logit scale, high vs low exposure
    eff[[length(eff) + 1]] <- bind_cols(tibble(model = m, enrichment = e, exposure = ex, term = int_col, delta_exposure_sd = delta_z),
      tibble(beta_int_mean = mean(b_int), beta_int_sd = sd(b_int), beta_int_lwr = quantile(b_int, 0.025), beta_int_upr = quantile(b_int, 0.975),
             p_positive = mean(b_int > 0),
             contrast_mean = mean(contrast), contrast_lwr = quantile(contrast, 0.025), contrast_upr = quantile(contrast, 0.975),
             sd_ratio_to_prior = sd(b_int) / 1), dg)
  }
  message("joint ", k, "/", length(models) * length(enrich))
}
write_csv(bind_rows(preds), file.path(tab_dir, "rain_joint_predictions.csv"))
write_csv(bind_rows(eff), file.path(tab_dir, "rain_joint_interactions.csv"))

# ---- (B) observed d15N as the response ---------------------------------------------------------------------
pr <- c(prior(normal(9, 5), class = Intercept), prior(normal(0, 2), class = b), prior(normal(0, 2), class = sd), prior(normal(0, 3), class = sigma))
f_a <- bf(N15 ~ z_chirps_storm_cumulative_mm * z_log_cess_exposure + z_pct_agriculture + maui + molokai + lanai + (1 | ahupuaa_key))
f_b <- bf(N15 ~ z_chirps_storm_cumulative_mm * z_pct_agriculture + z_log_cess_exposure + maui + molokai + lanai + (1 | ahupuaa_key))
f_c <- bf(N15 ~ z_chirps_storm_cumulative_mm * z_log_cess_exposure + z_chirps_storm_cumulative_mm * z_pct_agriculture + maui + molokai + lanai + (1 | ahupuaa_key))
bforms <- list(M4a_rain_x_cess = f_a, M4b_rain_x_ag = f_b, M5_both = f_c)
bdat <- water_iso |> select(N15, z_chirps_storm_cumulative_mm, z_log_cess_exposure, z_pct_agriculture, maui, molokai, lanai, ahupuaa_key)

b_eff <- list(); b_pred <- list()
for (m in names(bforms)) {
  bf_fit <- brm(bforms[[m]], data = bdat, prior = pr, chains = 4, iter = 4000, warmup = 1500, seed = SEED + 700,
                control = list(adapt_delta = 0.95), refresh = 0, silent = 2)
  saveRDS(bf_fit, file.path(mod_dir, paste0("rain_brms_", m, ".rds")))
  dr <- as_draws_df(bf_fit)
  rh <- summarise_draws(subset_draws(as_draws_array(bf_fit), variable = "^b_|^sd_|^sigma", regex = TRUE), "rhat", "ess_bulk")
  np <- nuts_params(bf_fit); ndiv <- sum(np$Value[np$Parameter == "divergent__"])
  for (nm in grep("^b_.*:", names(dr), value = TRUE)) {
    b <- dr[[nm]]
    b_eff[[length(b_eff) + 1]] <- tibble(model = m, term = sub("^b_", "", nm), mean = mean(b), sd = sd(b), lwr = quantile(b, 0.025), upr = quantile(b, 0.975),
                                         p_positive = mean(b > 0), n_divergent = ndiv, max_rhat = max(rh$rhat), min_ess_bulk = min(rh$ess_bulk))
  }
  for (ex in c("cesspool", "agriculture")) {
    int_term <- if (ex == "cesspool") "z_chirps_storm_cumulative_mm:z_log_cess_exposure" else "z_chirps_storm_cumulative_mm:z_pct_agriculture"
    if (!(paste0("b_", int_term) %in% names(dr))) next
    lh <- lo_hi[lo_hi$exposure == ex, ]
    for (lev in c("low", "high")) {
      zl <- if (lev == "low") lh$z_lo else lh$z_hi
      nd <- tibble(z_chirps_storm_cumulative_mm = rain_grid_z, z_log_cess_exposure = median(water_iso$z_log_cess_exposure),
                   z_pct_agriculture = median(water_iso$z_pct_agriculture), maui = 0, molokai = 0, lanai = 0, ahupuaa_key = "new")
      nd[[if (ex == "cesspool") "z_log_cess_exposure" else "z_pct_agriculture"]] <- zl
      ep <- posterior_epred(bf_fit, newdata = nd, re_formula = NA, allow_new_levels = TRUE)
      b_pred[[length(b_pred) + 1]] <- tibble(model = m, exposure = ex, level = lev, rain_mm = rain_grid_mm,
                                            d15N = colMeans(ep), lwr = apply(ep, 2, quantile, 0.025), upr = apply(ep, 2, quantile, 0.975))
    }
  }
  message("brms ", m)
}
write_csv(bind_rows(b_eff), file.path(tab_dir, "rain_brms_interactions.csv"))
write_csv(bind_rows(b_pred), file.path(tab_dir, "rain_brms_predictions.csv"))
message("done")
