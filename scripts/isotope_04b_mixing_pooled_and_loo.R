# Isotope analysis, step 6 (extension): population-pooled fractions, and a leave-one-out comparison with the
# fractions integrated out (the per-sample fractions are local parameters, so LOO on the conditional
# likelihood is unreliable). Uses only d15N and literature priors.
# Requires: scripts/isotope_04_mixing_model.R has been run (primary fits saved).

library(tidyverse)
library(here)
library(rstan)
library(posterior)
library(loo)

SEED <- 2847
set.seed(SEED)
options(mc.cores = 4)
rstan_options(auto_write = FALSE)
tab_dir <- here("output", "isotope", "tables")

water_iso <- readRDS(here("output", "isotope", "data", "water_iso.rds"))
pri <- read_csv(file.path(tab_dir, "endmember_priors.csv"), show_col_types = FALSE)
pool <- function(groups) {
  g <- pri[pri$group %in% groups, ]
  c(m = mean(g$mean), sd = sqrt(mean(g$source_sd^2) + var(g$mean)), se = sqrt(sum(g$se_group_mean^2)) / nrow(g))
}
ww <- pri[pri$group == "wastewater", ]
src_w <- c(m = ww$mean, sd = ww$source_sd, se = ww$se_group_mean)
src_o <- pool(c("synthetic_fertilizer", "soil_background", "marine_nitrate"))
enrich <- list(none = c(0, 0), moderate = c(2.5, 1.44))
sigma_a <- 0.3

sm <- stan_model(file = here("scripts", "isotope_two_source_mixing_pooled.stan"), model_name = "two_source_pooled")

fits <- list(); diag <- list(); par <- list(); ppc <- list(); samp <- list()
ppc_stats <- function(y) c(mean = mean(y), sd = sd(y), min = min(y), max = max(y), median = median(y),
                           frac_gt12 = mean(y > 12), frac_lt6 = mean(y < 6))
for (k in names(enrich)) {
  en <- enrich[[k]]
  dat <- list(N = nrow(water_iso), y = water_iso$N15, m_w = src_w[["m"]], se_w = src_w[["se"]], sd_w = src_w[["sd"]],
              m_o = src_o[["m"]], se_o = src_o[["se"]], sd_o = src_o[["sd"]], sd_logsd = 0.25,
              sigma_a = sigma_a, enr_mean = en[1], enr_sd = en[2])
  fit <- rstan::sampling(sm, data = dat, chains = 4, iter = 6000, warmup = 2000, seed = SEED + 50 + length(fits),
                         control = list(adapt_delta = 0.99), refresh = 0)
  fits[[k]] <- fit
  saveRDS(fit, here("output", "isotope", "model_objects", paste0("mix_pooled_fit_", k, ".rds")))
  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  s <- summarise_draws(subset_draws(as_draws_array(fit), variable = c("mu_w", "mu_o", "s_w", "s_o", "pi_w", "kappa", "f")),
                       "rhat", "ess_bulk", "ess_tail")
  diag[[k]] <- tibble(model = "pooled", enrichment = k, n_divergent = sum(vapply(sp, \(x) sum(x[, "divergent__"]), 0)),
                      max_rhat = max(s$rhat), min_ess_bulk = min(s$ess_bulk), min_ess_tail = min(s$ess_tail),
                      min_ebfmi = min(rstan::get_bfmi(fit)))
  par[[k]] <- summarise_draws(subset_draws(as_draws_array(fit), variable = c("mu_w", "mu_o", "s_w", "s_o", "pi_w", "kappa")),
                              "mean", "sd", ~quantile2(.x, c(0.025, 0.975))) |> mutate(model = "pooled", enrichment = k, .before = 1)
  f <- rstan::extract(fit, "f")$f
  samp[[k]] <- tibble(model = "pooled", enrichment = k, sample_id = water_iso$sample_id, N15 = water_iso$N15,
                      f_mean = colMeans(f), f_lwr = apply(f, 2, quantile, 0.025), f_upr = apply(f, 2, quantile, 0.975),
                      p_gt10 = colMeans(f > 0.1), p_gt20 = colMeans(f > 0.2), p_gt50 = colMeans(f > 0.5))
  yrep <- rstan::extract(fit, "y_rep")$y_rep
  obs <- ppc_stats(water_iso$N15); rs <- t(apply(yrep, 1, ppc_stats))
  ppc[[k]] <- tibble(model = "pooled", enrichment = k, statistic = names(obs), observed = obs, rep_mean = colMeans(rs),
                     ppp = vapply(names(obs), \(s) mean(rs[, s] >= obs[[s]]), 0))
  set.seed(SEED + 200)
  saveRDS(list(y_rep = yrep[sample(nrow(yrep), 300), ], y = water_iso$N15), here("output", "isotope", "model_objects", paste0("mix_pooled_ppc_", k, ".rds")))
}
write_csv(bind_rows(diag), file.path(tab_dir, "mix_pooled_diagnostics.csv"))
write_csv(bind_rows(par), file.path(tab_dir, "mix_pooled_parameters.csv"))
write_csv(bind_rows(samp), file.path(tab_dir, "mix_pooled_sample_posteriors.csv"))
write_csv(bind_rows(ppc), file.path(tab_dir, "mix_pooled_ppc_stats.csv"))

# ---- leave-one-out with the fractions integrated out (grid over f) -------------------------------
fg <- (0:99 + 0.5) / 100
marg_ll <- function(fit, k, pooled) {
  d <- rstan::extract(fit, c("mu_w", "mu_o", "s_w", "s_o", if (pooled) c("pi_w", "kappa")))
  en <- enrich[[k]]; y <- water_iso$N15
  S <- length(d$mu_w)
  ll <- matrix(NA_real_, S, length(y))
  for (s in seq_len(S)) {
    mu <- fg * d$mu_w[s] + (1 - fg) * d$mu_o[s] + en[1]
    sg <- sqrt((fg * d$s_w[s])^2 + ((1 - fg) * d$s_o[s])^2 + sigma_a^2 + en[2]^2)
    w <- if (pooled) { x <- dbeta(fg, d$pi_w[s] * d$kappa[s], (1 - d$pi_w[s]) * d$kappa[s]); x / sum(x) } else rep(1 / length(fg), length(fg))
    dens <- outer(y, seq_along(fg), \(a, b) dnorm(a, mu[b], sg[b]))
    ll[s, ] <- log(as.vector(dens %*% w))
  }
  ll
}
loos <- list()
for (k in names(enrich)) {
  fu <- readRDS(here("output", "isotope", "model_objects", paste0("mix_fit_", k, ".rds")))
  for (m in c("independent", "pooled")) {
    fit <- if (m == "pooled") fits[[k]] else fu
    ll <- marg_ll(fit, k, m == "pooled")
    llc <- array(ll, dim = c(nrow(ll) / 4, 4, ncol(ll)))  # draws are stored chain by chain
    loos[[paste(m, k, sep = "_")]] <- loo(llc, r_eff = relative_eff(exp(llc)))
  }
}
lc <- loo_compare(loos)
write_csv(as_tibble(as.data.frame(lc), rownames = "model"), file.path(tab_dir, "mix_loo_marginal_compare.csv"))
saveRDS(loos, here("output", "isotope", "model_objects", "mix_loo_marginal.rds"))
message("done")
