# Isotope analysis, step 6: two-source Bayesian d15N mixing model (rstan) on the isotope samples.
#
# Fractions are estimated from d15N alone plus literature priors. Agricultural land cover, the cesspool
# index, NOx concentration and the GAM results are NOT used anywhere in this script.
# Inputs : output/isotope/data/water_iso.rds, output/isotope/tables/endmember_priors.csv
# Outputs: output/isotope/tables/mix_*.csv, output/isotope/model_objects/*.rds

library(tidyverse)
library(here)
library(rstan)
library(posterior)
library(loo)

SEED <- 2847
set.seed(SEED)
options(mc.cores = 4)
rstan_options(auto_write = FALSE)

dir.create(here("output", "isotope", "model_objects"), showWarnings = FALSE, recursive = TRUE)
tab_dir <- here("output", "isotope", "tables")

water_iso <- readRDS(here("output", "isotope", "data", "water_iso.rds"))
pri <- read_csv(file.path(tab_dir, "endmember_priors.csv"), show_col_types = FALSE)

# ---- literature priors (same definitions as the identifiability simulation) -----------------
pool <- function(groups) {
  g <- pri[pri$group %in% groups, ]
  c(m = mean(g$mean), sd = sqrt(mean(g$source_sd^2) + var(g$mean)), se = sqrt(sum(g$se_group_mean^2)) / nrow(g))
}
ww <- pri[pri$group == "wastewater", ]
src_w <- c(m = ww$mean, sd = ww$source_sd, se = ww$se_group_mean)
src_o <- pool(c("synthetic_fertilizer", "soil_background", "marine_nitrate"))

make_data <- function(sw, so, enr_mean, enr_sd, sigma_a, y = water_iso$N15, prior_only = 0) {
  list(N = length(y), y = y,
       m_w = sw[["m"]], se_w = sw[["se"]], sd_w = sw[["sd"]],
       m_o = so[["m"]], se_o = so[["se"]], sd_o = so[["sd"]],
       sd_logsd = 0.25, sigma_a = sigma_a, enr_mean = enr_mean, enr_sd = enr_sd, prior_only = prior_only)
}

enrich <- list(none = c(0, 0), weak = c(1.25, 0.72), moderate = c(2.5, 1.44), strong = c(5, 2.89))
shift <- function(s, k) { s[["m"]] <- s[["m"]] + k * s[["se"]]; s }
scale_sd <- function(s, k) { s[["sd"]] <- s[["sd"]] * k; s }
endm <- list(
  "literature (base)"        = list(w = src_w, o = src_o),
  "cesspool-like waste N(11, 4.5)" = list(w = c(m = 11, sd = 4.5, se = src_w[["se"]]), o = src_o),
  "source means +2 SE"       = list(w = shift(src_w, 2), o = shift(src_o, 2)),
  "source means -2 SE"       = list(w = shift(src_w, -2), o = shift(src_o, -2)),
  "source SDs x1.5"          = list(w = scale_sd(src_w, 1.5), o = scale_sd(src_o, 1.5)),
  "source SDs x0.67"         = list(w = scale_sd(src_w, 0.67), o = scale_sd(src_o, 0.67))
)

runs <- bind_rows(
  expand_grid(endmember = names(endm), enrichment = c("none", "moderate"), sigma_a = 0.3),
  tibble(endmember = "literature (base)", enrichment = c("weak", "strong"), sigma_a = 0.3),
  expand_grid(endmember = "literature (base)", enrichment = c("none", "moderate"), sigma_a = 1.0)
) |> mutate(run_id = row_number(),
            primary = endmember == "literature (base)" & sigma_a == 0.3 & enrichment %in% c("none", "moderate"))

# ---- compile once ---------------------------------------------------------------------------
sm <- stan_model(file = here("scripts", "isotope_two_source_mixing.stan"), model_name = "two_source_mixing")

fit_run <- function(r) {
  e <- endm[[r$endmember]]; en <- enrich[[r$enrichment]]
  dat <- make_data(e$w, e$o, en[1], en[2], r$sigma_a)
  rstan::sampling(sm, data = dat, chains = 4, iter = 2000, warmup = 1000, seed = SEED + r$run_id,
                  control = list(adapt_delta = 0.95), refresh = 0)
}

diag_row <- function(fit, r) {
  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  draws <- as_draws_array(fit)
  s <- summarise_draws(subset_draws(draws, variable = c("mu_w", "mu_o", "s_w", "s_o", "f")), "rhat", "ess_bulk", "ess_tail")
  tibble(r, n_divergent = sum(vapply(sp, \(x) sum(x[, "divergent__"]), 0)),
         max_treedepth_hits = sum(vapply(sp, \(x) sum(x[, "treedepth__"] >= 10), 0)),
         max_rhat = max(s$rhat, na.rm = TRUE), min_ess_bulk = min(s$ess_bulk, na.rm = TRUE),
         min_ess_tail = min(s$ess_tail, na.rm = TRUE),
         min_ebfmi = min(rstan::get_bfmi(fit)))
}

sample_summary <- function(fit, r) {
  f <- rstan::extract(fit, "f")$f
  tibble(run_id = r$run_id, sample_id = water_iso$sample_id, N15 = water_iso$N15,
         f_mean = colMeans(f), f_lwr = apply(f, 2, quantile, 0.025), f_upr = apply(f, 2, quantile, 0.975),
         p_gt10 = colMeans(f > 0.10), p_gt20 = colMeans(f > 0.20), p_gt50 = colMeans(f > 0.50))
}

ppc_stats <- function(y) c(mean = mean(y), sd = sd(y), min = min(y), max = max(y), median = median(y),
                           frac_gt12 = mean(y > 12), frac_lt6 = mean(y < 6))

diags <- list(); samples <- list(); params <- list(); ppcs <- list(); loos <- list()
t0 <- Sys.time()
for (i in seq_len(nrow(runs))) {
  r <- runs[i, ]
  fit <- fit_run(r)
  diags[[i]] <- diag_row(fit, r)
  samples[[i]] <- sample_summary(fit, r)
  pm <- summarise_draws(subset_draws(as_draws_array(fit), variable = c("mu_w", "mu_o", "s_w", "s_o")),
                        "mean", "sd", ~quantile2(.x, c(0.025, 0.975)))
  params[[i]] <- bind_cols(r, pm)
  fdraw <- rstan::extract(fit, "f")$f
  params[[i]] <- bind_rows(params[[i]],
    bind_cols(r, tibble(variable = "mean_f (average of sample fractions)", mean = mean(fdraw), sd = sd(rowMeans(fdraw)),
                        q2.5 = quantile(rowMeans(fdraw), 0.025), q97.5 = quantile(rowMeans(fdraw), 0.975))))
  if (r$primary) {
    yrep <- rstan::extract(fit, "y_rep")$y_rep
    obs <- ppc_stats(water_iso$N15)
    rep_stats <- t(apply(yrep, 1, ppc_stats))
    ppcs[[length(ppcs) + 1]] <- tibble(run_id = r$run_id, enrichment = r$enrichment, statistic = names(obs),
                                       observed = obs, rep_mean = colMeans(rep_stats),
                                       ppp = vapply(names(obs), \(s) mean(rep_stats[, s] >= obs[[s]]), 0))
    set.seed(SEED + 100 + i)
    saveRDS(list(run = r, y_rep = yrep[sample(nrow(yrep), 300), ], y = water_iso$N15),
            here("output", "isotope", "model_objects", paste0("mix_ppc_", r$enrichment, ".rds")))
    ll <- loo::extract_log_lik(fit, "log_lik", merge_chains = FALSE)
    loos[[r$enrichment]] <- loo(ll, r_eff = loo::relative_eff(exp(ll)))
    saveRDS(fit, here("output", "isotope", "model_objects", paste0("mix_fit_", r$enrichment, ".rds")))
    saveRDS(as_draws_matrix(subset_draws(as_draws_array(fit), variable = "f")),
            here("output", "isotope", "model_objects", paste0("mix_draws_f_", r$enrichment, ".rds")))
  }
  message("run ", i, "/", nrow(runs), " | ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}

write_csv(bind_rows(diags), file.path(tab_dir, "mix_diagnostics.csv"))
write_csv(bind_rows(samples), file.path(tab_dir, "mix_sample_posteriors_all_runs.csv"))
write_csv(bind_rows(params), file.path(tab_dir, "mix_parameter_posteriors.csv"))
write_csv(bind_rows(ppcs), file.path(tab_dir, "mix_ppc_stats.csv"))
write_csv(runs, file.path(tab_dir, "mix_runs.csv"))

# primary sample-level export (literature priors; with and without enrichment allowance)
prim <- runs |> filter(primary) |> select(run_id, endmember, enrichment, sigma_a)
bind_rows(samples) |> inner_join(prim, by = "run_id") |>
  left_join(select(water_iso, sample_id, island, ahupuaa, collected_hst), by = "sample_id") |>
  write_csv(file.path(tab_dir, "mix_sample_posteriors_primary.csv"))

lc <- loo_compare(loos[c("none", "moderate")])
write_csv(as_tibble(as.data.frame(lc), rownames = "model"), file.path(tab_dir, "mix_loo_compare.csv"))
saveRDS(loos, here("output", "isotope", "model_objects", "mix_loo.rds"))
message("done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
