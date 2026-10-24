# Isotope analysis, step 7: does the isotope-derived waste-associated fraction vary with cesspool exposure,
# agriculture, NOx and rainfall?  Two analyses, both propagating isotope uncertainty:
#   (A) joint Bayesian model (rstan): the fraction regression is fitted together with the mixing model;
#   (B) modular repeated-draw analysis: regress draws from the isotope-only fractions (step 6, independent
#       uniform prior) on the covariates and pool with Rubin's rules. Because those draws ignore the covariates,
#       (B) tends to be attenuated; it is a conservative check that keeps the isotope estimates untouched.
# Covariates are used only as predictors of the fraction, never to estimate it in the isotope-only model.
# Outputs: output/isotope/tables/lu_*.csv, output/isotope/model_objects/lu_*.rds

library(tidyverse)
library(here)
library(rstan)
library(posterior)
library(sandwich)

SEED <- 2847
set.seed(SEED)
options(mc.cores = 4)
rstan_options(auto_write = FALSE)
tab_dir <- here("output", "isotope", "tables")
mod_dir <- here("output", "isotope", "model_objects")

water_iso <- readRDS(here("output", "isotope", "data", "water_iso.rds")) |>
  mutate(
    z_log_NOx = as.numeric(scale(log_NO3NO2)),
    maui = as.numeric(island == "Maui"), molokai = as.numeric(island == "Moloka'i"), lanai = as.numeric(island == "L\u0101na'i"),
    ahup_i = as.integer(factor(ahupuaa_key))
  )
stopifnot(!anyNA(select(water_iso, z_log_cess_exposure, z_pct_agriculture, z_chirps_storm_cumulative_mm, z_log_NOx)))

pri <- read_csv(file.path(tab_dir, "endmember_priors.csv"), show_col_types = FALSE)
pool <- function(groups) {
  g <- pri[pri$group %in% groups, ]
  c(m = mean(g$mean), sd = sqrt(mean(g$source_sd^2) + var(g$mean)), se = sqrt(sum(g$se_group_mean^2)) / nrow(g))
}
ww <- pri[pri$group == "wastewater", ]
src_w <- c(m = ww$mean, sd = ww$source_sd, se = ww$se_group_mean)
src_o <- pool(c("synthetic_fertilizer", "soil_background", "marine_nitrate"))
enrich <- list(none = c(0, 0), moderate = c(2.5, 1.44))

water_iso <- water_iso |>
  mutate(rain_x_cess = z_chirps_storm_cumulative_mm * z_log_cess_exposure,
         rain_x_ag = z_chirps_storm_cumulative_mm * z_pct_agriculture)

isl <- c("maui", "molokai", "lanai")
models <- list(
  M1_cess_ag = c("z_log_cess_exposure", "z_pct_agriculture", isl),
  M2_plus_NOx = c("z_log_cess_exposure", "z_pct_agriculture", "z_log_NOx", isl),
  M3_plus_rain = c("z_log_cess_exposure", "z_pct_agriculture", "z_chirps_storm_cumulative_mm", isl),
  M4a_rain_x_cess = c("z_log_cess_exposure", "z_pct_agriculture", "z_chirps_storm_cumulative_mm", "rain_x_cess", isl),
  M4b_rain_x_ag = c("z_log_cess_exposure", "z_pct_agriculture", "z_chirps_storm_cumulative_mm", "rain_x_ag", isl)
)

sm <- stan_model(file = here("scripts", "isotope_two_source_mixing_covariates.stan"), model_name = "mixing_covariates")

make_data <- function(cols, en, y = water_iso$N15, prior_only = 0, prior_beta_sd = 1) {
  list(N = nrow(water_iso), y = y, K = length(cols), X = as.matrix(water_iso[, cols]),
       A = max(water_iso$ahup_i), ah = water_iso$ahup_i,
       m_w = src_w[["m"]], se_w = src_w[["se"]], sd_w = src_w[["sd"]],
       m_o = src_o[["m"]], se_o = src_o[["se"]], sd_o = src_o[["sd"]],
       sd_logsd = 0.25, sigma_a = 0.3, enr_mean = en[1], enr_sd = en[2],
       prior_beta_sd = prior_beta_sd, prior_only = prior_only)
}

fit_one <- function(cols, en, seed, y = water_iso$N15) {
  rstan::sampling(sm, data = make_data(cols, en, y = y), chains = 4, iter = 3000, warmup = 1500, seed = seed,
                  control = list(adapt_delta = 0.99, max_treedepth = 12), refresh = 0,
                  pars = c("mu_w", "mu_o", "s_w", "s_o", "alpha0", "beta", "sigma_f", "sigma_ah", "f"))
}

summarise_beta <- function(fit, cols, model, enrichment, analysis = "joint", extra = list()) {
  d <- rstan::extract(fit, c("alpha0", "beta", "f"))
  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  arr <- as_draws_array(fit)
  sm_ <- summarise_draws(subset_draws(arr, variable = c("alpha0", "beta", "sigma_f", "sigma_ah", "mu_w", "mu_o")), "rhat", "ess_bulk")
  dg <- tibble(n_divergent = sum(vapply(sp, \(x) sum(x[, "divergent__"]), 0)), max_rhat = max(sm_$rhat),
               min_ess_bulk = min(sm_$ess_bulk), min_ebfmi = min(rstan::get_bfmi(fit)))
  eff <- map_dfr(seq_along(cols), \(k) {
    b <- d$beta[, k]
    dfrac <- plogis(d$alpha0 + b) - plogis(d$alpha0)
    tibble(term = cols[k], beta_mean = mean(b), beta_sd = sd(b), beta_lwr = quantile(b, 0.025), beta_upr = quantile(b, 0.975),
           p_positive = mean(b > 0), sd_ratio_to_prior = sd(b) / 1,
           dfrac_mean = mean(dfrac), dfrac_lwr = quantile(dfrac, 0.025), dfrac_upr = quantile(dfrac, 0.975))
  })
  list(effects = bind_cols(tibble(analysis = analysis, model = model, enrichment = enrichment), eff, dg), draws = d$beta)
}

# ---- (A) joint models -----------------------------------------------------------------------------------
eff_joint <- list(); k <- 0
joint_done <- file.exists(file.path(tab_dir, "lu_joint_effects.csv")) && file.exists(file.path(tab_dir, "lu_permutation_null.csv"))
if (!joint_done) for (m in names(models)) for (e in names(enrich)) {
  k <- k + 1
  fit <- fit_one(models[[m]], enrich[[e]], SEED + 300 + k)
  r <- summarise_beta(fit, models[[m]], m, e)
  eff_joint[[k]] <- r$effects
  saveRDS(list(model = m, enrichment = e, cols = models[[m]], beta = r$draws), file.path(mod_dir, paste0("lu_beta_", m, "_", e, ".rds")))
  if (m == "M1_cess_ag") saveRDS(fit, file.path(mod_dir, paste0("lu_fit_M1_", e, ".rds")))
  message("joint ", k, "/", length(models) * length(enrich))
}
if (!joint_done) write_csv(bind_rows(eff_joint), file.path(tab_dir, "lu_joint_effects.csv"))

# permutation null for M1: shuffle d15N across samples (covariates and sites intact) and refit
perm <- list(); P <- 10
if (!joint_done) for (e in names(enrich)) for (p in seq_len(P)) {
  set.seed(SEED + 1000 + p)
  yp <- sample(water_iso$N15)
  fit <- fit_one(models$M1_cess_ag, enrich[[e]], SEED + 2000 + p, y = yp)
  r <- summarise_beta(fit, models$M1_cess_ag, "M1_cess_ag", e, analysis = paste0("permutation_", p))
  perm[[length(perm) + 1]] <- r$effects |> filter(term %in% c("z_log_cess_exposure", "z_pct_agriculture"))
  message("perm ", e, " ", p)
}
if (!joint_done) write_csv(bind_rows(perm), file.path(tab_dir, "lu_permutation_null.csv"))

# ---- (B) modular repeated-draw analysis ----------------------------------------------------------------------
rubin <- function(est, v) {
  m <- length(est); qbar <- mean(est); W <- mean(v); B <- var(est); Tv <- W + (1 + 1 / m) * B
  df <- (m - 1) * (1 + W / ((1 + 1 / m) * B))^2
  tibble(estimate = qbar, se = sqrt(Tv), lwr = qbar - qt(0.975, df) * sqrt(Tv), upr = qbar + qt(0.975, df) * sqrt(Tv),
         p_two_sided = 2 * pt(-abs(qbar / sqrt(Tv)), df), frac_between = (1 + 1 / m) * B / Tv)
}
mod <- list()
for (e in names(enrich)) {
  fm <- readRDS(file.path(mod_dir, paste0("mix_draws_f_", e, ".rds")))
  fm <- matrix(as.numeric(fm), nrow(fm), ncol(fm)); idx <- sample(nrow(fm), 300)
  for (m in names(models)) {
    cols <- models[[m]]
    form <- reformulate(cols, "lf")
    fits <- map(idx, \(s) {
      d <- water_iso; d$lf <- qlogis(pmin(pmax(fm[s, ], 0.001), 0.999))
      lm(form, data = d)
    })
    ests <- map(fits, \(l) coef(l)[-1]); vs <- map(fits, \(l) diag(vcovCL(l, cluster = water_iso$ahup_i, type = "HC1"))[-1])
    mod[[length(mod) + 1]] <- map_dfr(cols, \(cn) bind_cols(
      tibble(analysis = "modular", model = m, enrichment = e, term = cn),
      rubin(map_dbl(ests, cn), map_dbl(vs, cn))))
  }
}
write_csv(bind_rows(mod), file.path(tab_dir, "lu_modular_effects.csv"))

# ---- descriptive: unadjusted rank correlation of the isotope-only waste probability with covariates ----------
samp <- read_csv(file.path(tab_dir, "mix_sample_posteriors_four_variants.csv"), show_col_types = FALSE) |>
  left_join(select(water_iso, sample_id, z_log_cess_exposure, z_pct_agriculture, z_chirps_storm_cumulative_mm, z_log_NOx), by = "sample_id")
cor_tab <- samp |> group_by(model, enrichment) |>
  summarise(across(c(z_log_cess_exposure, z_pct_agriculture, z_chirps_storm_cumulative_mm, z_log_NOx),
                   \(x) cor(x, f_mean, method = "spearman")), .groups = "drop")
write_csv(cor_tab, file.path(tab_dir, "lu_unadjusted_rank_correlations.csv"))
message("done")
