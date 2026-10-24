# Isotope analysis, step 5: simulation-based identifiability of d15N source fractions.
#
# Design
# - Per-sample mixing model for one isotope with source variability (Moore & Semmens form):
#     delta_i ~ Normal( sum_k f_ik * mu_k [+ enrichment],
#                       sqrt( sum_k f_ik^2 * (sd_k^2 + se_k^2) + sigma_a^2 [+ enrichment variance] ) )
#   Uncertainty in the source means mu_k ~ Normal(m_k, se_k) is integrated out per sample (exact for a
#   sample taken alone; it ignores that mu_k is shared across samples, which the coverage results test,
#   because each simulated dataset draws one set of true source means).
# - Posterior of f_i on a simplex grid with a Dirichlet prior (exact; no MCMC needed for this one-isotope
#   per-sample model). The Stan model in step 6 adds shared source means and hierarchical structure.
# - Truth: sources drawn per sample from Normal(mu_k_true, sd_k), fractions from a Dirichlet, optional
#   enrichment (denitrification-like, Uniform(0, 5) per mil) and analytical error.
# Inputs : output/isotope/tables/endmember_priors.csv
# Outputs: output/isotope/tables/ident_sim_*.csv

library(tidyverse)
library(here)

SEED <- 2847
set.seed(SEED)

pri <- read_csv(here("output", "isotope", "tables", "endmember_priors.csv"), show_col_types = FALSE)

# ---- source definitions -------------------------------------------------------------------
pool <- function(groups, label) {
  g <- filter(pri, group %in% groups)
  tibble(source = label, mean = mean(g$mean),
         sd = sqrt(mean(g$source_sd^2) + var(g$mean)),
         se = sqrt(sum(g$se_group_mean^2)) / nrow(g))
}
one <- function(grp, label) {
  g <- pri[pri$group == grp, ]
  tibble(source = label, mean = g$mean, sd = g$source_sd, se = g$se_group_mean)
}

src_def <- function(structure, overlap) {
  ww <- one("wastewater", "wastewater")
  if (overlap == "high") {
    # cesspool-specific nitrate in Hawaii (about 10-12 per mil) rather than treated effluent
    ww$mean <- 11; ww$sd <- 4.5
  }
  if (structure == "two_source") {
    bind_rows(ww, pool(c("synthetic_fertilizer", "soil_background", "marine_nitrate"), "other"))
  } else {
    bind_rows(ww, one("synthetic_fertilizer", "agriculture"),
              pool(c("soil_background", "marine_nitrate"), "background"))
  }
}

# ---- overlap of the source distributions per scenario ------------------------------------
ovl <- function(m1, s1, m2, s2) integrate(\(x) pmin(dnorm(x, m1, s1), dnorm(x, m2, s2)), -100, 150)$value
overlap_tab <- expand_grid(structure = c("two_source", "three_source"), overlap = c("low", "high")) |>
  rowwise() |>
  reframe({
    s <- src_def(structure, overlap)
    pr <- t(combn(nrow(s), 2))
    tibble(structure = structure, overlap = overlap,
           pair = paste(s$source[pr[, 1]], "vs", s$source[pr[, 2]]),
           overlap_coef = map2_dbl(pr[, 1], pr[, 2], \(a, b) ovl(s$mean[a], s$sd[a], s$mean[b], s$sd[b])))
  })
write_csv(overlap_tab, here("output", "isotope", "tables", "ident_sim_overlap.csv"))
source_tab <- expand_grid(structure = c("two_source", "three_source"), overlap = c("low", "high")) |>
  rowwise() |> reframe(src_def(structure, overlap) |> mutate(structure = structure, overlap = overlap))
write_csv(source_tab, here("output", "isotope", "tables", "ident_sim_sources.csv"))

# ---- grid, simulation, fitting ------------------------------------------------------------
make_grid <- function(K) {
  if (K == 2) {
    N <- 100; j <- 0:N
    cbind((j + 0.5) / (N + 1), (N - j + 0.5) / (N + 1))
  } else {
    N <- 50
    g <- expand.grid(j1 = 0:N, j2 = 0:N)
    g <- g[g$j1 + g$j2 <= N, ]
    cbind(g$j1, g$j2, N - g$j1 - g$j2) |> (\(j) (j + 0.5) / (N + 1.5))()
  }
}

# mixture regimes: Dirichlet concentration for (wastewater, other sources...)
alpha_for <- function(structure, regime) {
  a <- switch(regime, low_waste = c(0.6, 1.2, 2.0), moderate = c(1, 1, 1), high_waste = c(2, 1, 1))
  if (structure == "two_source") c(a[1], sum(a[-1])) else a
}

rdirich <- function(n, alpha) {
  g <- matrix(rgamma(n * length(alpha), shape = rep(alpha, each = n)), n)
  g / rowSums(g)
}

simulate_sets <- function(src, alpha, sigma_a, enrich, R = 50, n = 108) {
  K <- nrow(src)
  res <- vector("list", R)
  for (r in seq_len(R)) {
    mu <- rnorm(K, src$mean, src$se)
    f <- rdirich(n, alpha)
    x <- matrix(rnorm(n * K, rep(mu, each = n), rep(src$sd, each = n)), n, K)
    d <- rowSums(f * x) + if (enrich) runif(n, 0, 5) else 0
    res[[r]] <- list(delta = d + rnorm(n, 0, sigma_a), f = f, id = rep(r, n))
  }
  list(delta = unlist(map(res, "delta")), f = do.call(rbind, map(res, "f")), id = unlist(map(res, "id")))
}

fit_grid <- function(delta, src, grid_f, sigma_a, aware = FALSE, alpha_prior = NULL,
                     mean_shift_se = 0, sd_mult = 1) {
  K <- nrow(src)
  m <- src$mean + mean_shift_se * src$se
  s2 <- (src$sd * sd_mult)^2 + src$se^2
  m_g <- as.vector(grid_f %*% m) + if (aware) 2.5 else 0
  v_g <- as.vector((grid_f^2) %*% s2) + sigma_a^2 + if (aware) 25 / 12 else 0
  d2 <- outer(delta, m_g, "-")^2
  lp <- -0.5 * sweep(d2, 2, v_g, "/") - 0.5 * rep(log(v_g), each = length(delta))
  if (!is.null(alpha_prior)) lp <- lp + rep(as.vector(log(grid_f) %*% (alpha_prior - 1)), each = length(delta))
  lp <- lp - apply(lp, 1, max)
  W <- exp(lp); W <- W / rowSums(W)
  est <- W %*% grid_f
  # equal-tailed 95% intervals by marginal grid ordering
  lo <- hi <- matrix(NA_real_, length(delta), K)
  for (k in seq_len(K)) {
    o <- order(grid_f[, k]); cs <- t(apply(W[, o, drop = FALSE], 1, cumsum))
    fk <- grid_f[o, k]
    lo[, k] <- fk[max.col(cs >= 0.025, ties.method = "first")]
    hi[, k] <- fk[max.col(cs >= 0.975, ties.method = "first")]
  }
  dom <- matrix(0, nrow(grid_f), K); dom[cbind(seq_len(nrow(grid_f)), max.col(grid_f, ties.method = "first"))] <- 1
  list(est = est, lo = lo, hi = hi, pdom = W %*% dom, p_ww_gt50 = as.vector(W %*% (grid_f[, 1] > 0.5)))
}

summarise_fit <- function(fit, truth, id, src) {
  K <- nrow(src)
  per_k <- map_dfr(seq_len(K), \(k) {
    e <- fit$est[, k]; t <- truth[, k]
    cover <- fit$lo[, k] <= t & t <= fit$hi[, k]
    byset <- tibble(id, err = e - t, cover = cover) |> group_by(id) |> summarise(bias = mean(err), cover = mean(cover), .groups = "drop")
    tibble(source = src$source[k], bias = mean(e - t), bias_se = sd(byset$bias) / sqrt(nrow(byset)),
           rmse = sqrt(mean((e - t)^2)), coverage95 = mean(cover), coverage_se = sd(byset$cover) / sqrt(nrow(byset)),
           ci_width = mean(fit$hi[, k] - fit$lo[, k]),
           r2 = 1 - sum((e - t)^2) / sum((t - mean(t))^2))
  })
  # dominant-source classification
  true_dom <- max.col(truth, ties.method = "first")
  call <- max.col(fit$pdom, ties.method = "first"); conf <- apply(fit$pdom, 1, max)
  confident <- conf >= 0.8; wrong <- call != true_dom
  cls <- tibble(
    confident_rate = mean(confident),
    accuracy_all = mean(!wrong),
    accuracy_confident = if (any(confident)) mean(!wrong[confident]) else NA_real_,
    confident_wrong_rate = mean(confident & wrong),
    # wastewater (source 1) vs the rest, using P(wastewater dominant)
    ww_true_dom_rate = mean(true_dom == 1),
    ww_missed_conf = if (any(true_dom == 1)) mean(fit$pdom[true_dom == 1, 1] <= 0.2) else NA_real_,
    ww_false_conf = if (any(true_dom != 1)) mean(fit$pdom[true_dom != 1, 1] >= 0.8) else NA_real_
  )
  # AUC: how well P(source a dominant) ranks truly a-dominant above truly b-dominant samples
  auc <- function(score, a, b) {
    pos <- score[true_dom == a]; neg <- score[true_dom == b]
    if (length(pos) < 5 || length(neg) < 5) return(NA_real_)
    r <- rank(c(pos, neg)); (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) / (length(pos) * length(neg))
  }
  if (K == 2) cls$auc_ww_vs_other <- auc(fit$pdom[, 1], 1, 2)
  if (K == 3) {
    cls$ag_called_ww_conf <- if (any(true_dom == 2)) mean(fit$pdom[true_dom == 2, 1] >= 0.8) else NA_real_
    cls$ww_called_ag_conf <- if (any(true_dom == 1)) mean(fit$pdom[true_dom == 1, 2] >= 0.8) else NA_real_
    cls$ag_called_bg_conf <- if (any(true_dom == 2)) mean(fit$pdom[true_dom == 2, 3] >= 0.8) else NA_real_
    cls$bg_called_ag_conf <- if (any(true_dom == 3)) mean(fit$pdom[true_dom == 3, 2] >= 0.8) else NA_real_
    cls$auc_ww_vs_ag <- auc(fit$pdom[, 1], 1, 2)
    cls$auc_ww_vs_bg <- auc(fit$pdom[, 1], 1, 3)
    cls$auc_ag_vs_bg <- auc(fit$pdom[, 2], 2, 3)
  }
  list(per_k = per_k, cls = cls)
}

# ---- main grid ---------------------------------------------------------------------------
cells <- expand_grid(structure = c("two_source", "three_source"), overlap = c("low", "high"),
                     sigma_a = c(0.3, 1.0), truth_enrich = c(FALSE, TRUE),
                     regime = c("low_waste", "moderate", "high_waste"))
grids <- list(`2` = make_grid(2), `3` = make_grid(3))

metrics <- list(); classes <- list()
t0 <- Sys.time()
for (i in seq_len(nrow(cells))) {
  cl <- cells[i, ]
  src <- src_def(cl$structure, cl$overlap)
  K <- nrow(src); gf <- grids[[as.character(K)]]
  sim <- simulate_sets(src, alpha_for(cl$structure, cl$regime), cl$sigma_a, cl$truth_enrich)
  for (aware in c(FALSE, TRUE)) {
    fit <- fit_grid(sim$delta, src, gf, cl$sigma_a, aware = aware)
    sm <- summarise_fit(fit, sim$f, sim$id, src)
    key <- cl |> mutate(fit_enrich_aware = aware)
    metrics[[length(metrics) + 1]] <- bind_cols(key, sm$per_k)
    classes[[length(classes) + 1]] <- bind_cols(key, sm$cls)
  }
  if (i %% 8 == 0) message("cell ", i, "/", nrow(cells), " | ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}
write_csv(bind_rows(metrics), here("output", "isotope", "tables", "ident_sim_metrics.csv"))
write_csv(bind_rows(classes), here("output", "isotope", "tables", "ident_sim_classification.csv"))

# ---- prior sensitivity (base truth: no enrichment, moderate mixtures) ------------------------
variants <- tribble(
  ~variant,            ~alpha_scale, ~shift, ~sdm,
  "base (Dirichlet 1)", 1,            0,      1,
  "sparser fractions (Dirichlet 0.5)", 0.5, 0, 1,
  "denser fractions (Dirichlet 2)",    2,   0, 1,
  "source means +2 SE",  1,            2,      1,
  "source means -2 SE",  1,           -2,      1,
  "source SDs x1.5",     1,            0,      1.5,
  "source SDs x0.67",    1,            0,      0.67
)
sens <- list()
for (st in c("two_source", "three_source")) for (sg in c(0.3, 1.0)) {
  src <- src_def(st, "low"); K <- nrow(src); gf <- grids[[as.character(K)]]
  sim <- simulate_sets(src, alpha_for(st, "moderate"), sg, FALSE)
  for (v in seq_len(nrow(variants))) {
    vv <- variants[v, ]
    fit <- fit_grid(sim$delta, src, gf, sg, alpha_prior = rep(vv$alpha_scale, K), mean_shift_se = vv$shift, sd_mult = vv$sdm)
    sm <- summarise_fit(fit, sim$f, sim$id, src)
    sens[[length(sens) + 1]] <- bind_cols(tibble(structure = st, sigma_a = sg, variant = vv$variant), sm$per_k, sm$cls)
  }
}
write_csv(bind_rows(sens), here("output", "isotope", "tables", "ident_sim_prior_sensitivity.csv"))
message("done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
