# Isotope analysis: publication figures on the drivers of nitrate concentration from d15N.
#
# Figure 1  Sources and what d15N can say (literature endmembers, observed d15N, P(waste-associated N dominant))
# Figure 2  Nitrate concentration and d15N (does high NOx carry waste-like nitrate?)
# Figure 3  Land-use drivers (cesspool exposure, agriculture) of d15N and of the waste-associated fraction
# Figure 4  Rainfall interactions (predicted waste fraction and d15N at low and high exposure)
# Figure 5  GAM effects on NOx next to the two-source and three-source isotope tests of the same drivers
#
# Everything is read from saved tables and model objects. Figures are written to output/isotope/figures/ as
# vector PDF and 600-dpi PNG. Chemical notation uses expression() (no unicode sub/superscripts); no titles inside figures.

library(tidyverse)
library(here)
library(patchwork)

tab_dir <- here("output", "isotope", "tables")
mod_dir <- here("output", "isotope", "model_objects")
fig_dir <- here("output", "isotope", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

water_iso <- readRDS(here("output", "isotope", "data", "water_iso.rds")) |>
  mutate(island = factor(island, levels = c("O'ahu", "Maui", "Moloka'i", "L\u0101na'i")))
island_pal <- c("O'ahu" = "#1565C0", "Maui" = "#2E7D32", "Moloka'i" = "#E65100", "L\u0101na'i" = "#6A1B9A")

# The saved Bayesian fits must have been run on the current isotope samples
n_fit <- ncol(readRDS(file.path(mod_dir, "mix_draws_f_none.rds")))
if (n_fit != nrow(water_iso)) {
  stop("Saved model fits use ", n_fit, " isotope samples but water_iso has ", nrow(water_iso),
       ". Re-run scripts isotope_04, isotope_04b, isotope_05 and isotope_06 before making figures.",
       call. = FALSE)
}

theme_iso_paper <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
          axis.line = element_line(color = "gray30", linewidth = 0.3), axis.ticks = element_line(color = "gray30", linewidth = 0.3),
          plot.tag = element_text(face = "bold", size = base_size + 2), legend.title = element_text(size = base_size),
          legend.text = element_text(size = base_size - 1), strip.text = element_text(face = "plain", size = base_size))
}
lab_d15N <- expression(delta^{15}*N~"(per mil vs air)")
lab_nox <- expression(NO[3]+NO[2]~(mu*mol~L^{-1}))

save_fig <- function(plot, name, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(fig_dir, paste0(name, ".png")), plot, width = width, height = height, dpi = 600)
}

src_cols <- c(wastewater = "#D55E00", manure = "#CC79A7", synthetic_fertilizer = "#009E73", soil_background = "#E69F00", marine_nitrate = "#0072B2")
src_labs <- c(wastewater = "Wastewater", manure = "Manure / organic fertilizer", synthetic_fertilizer = "Synthetic fertilizer",
              soil_background = "Soil / low-impact groundwater", marine_nitrate = "Marine nitrate")

# ---- Figure 1 -------------------------------------------------------------------------------------------------
pri <- read_csv(file.path(tab_dir, "endmember_priors.csv"), show_col_types = FALSE)
xg <- seq(-8, 30, length.out = 500)
dens <- pri |> rowwise() |> mutate(d = list(tibble(x = xg, y = dnorm(xg, mean, source_sd)))) |> ungroup() |> select(group, d) |> unnest(d)

pA <- ggplot() +
  geom_histogram(data = water_iso, aes(N15, after_stat(density)), binwidth = 1, fill = "gray70", color = "white", linewidth = 0.2) +
  geom_area(data = dens, aes(x, y, fill = group), alpha = 0.22, position = "identity") +
  geom_line(data = dens, aes(x, y, color = group), linewidth = 0.7) +
  scale_color_manual(values = src_cols, labels = src_labs, name = "Literature sources") +
  scale_fill_manual(values = src_cols, labels = src_labs, name = "Literature sources") +
  coord_cartesian(xlim = c(-2, 26)) +
  labs(x = NULL, y = "Density", tag = "A") +
  guides(color = guide_legend(ncol = 1, override.aes = list(linewidth = 1)), fill = guide_legend(ncol = 1)) +
  theme_iso_paper() + theme(legend.position = "right")

mx <- read_csv(file.path(tab_dir, "mix_sample_posteriors_four_variants.csv"), show_col_types = FALSE) |>
  mutate(variant = paste(model, "fractions,", if_else(enrichment == "none", "no enrichment allowance", "enrichment allowance")),
         variant = factor(variant, c("independent fractions, no enrichment allowance", "independent fractions, enrichment allowance",
                                     "pooled fractions, no enrichment allowance", "pooled fractions, enrichment allowance")))
var_cols <- c("#0072B2", "#56B4E9", "#D55E00", "#E69F00")
pB <- ggplot(mx, aes(N15, p_gt50, color = variant, linetype = variant)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.2, ymax = 0.8, fill = "gray90", alpha = 0.7) +
  geom_line(linewidth = 0.8) +
  geom_rug(data = water_iso, aes(x = N15), inherit.aes = FALSE, sides = "b", alpha = 0.35, length = unit(0.03, "npc")) +
  scale_color_manual(values = var_cols, name = "Mixing model") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"), name = "Mixing model") +
  coord_cartesian(xlim = c(-2, 26), ylim = c(0, 1)) +
  labs(x = lab_d15N, y = "P(waste-associated N dominates)", tag = "B") +
  guides(color = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1)) +
  theme_iso_paper() + theme(legend.position = "right")
fig1 <- pA / pB + plot_layout(heights = c(1, 1))
save_fig(fig1, "iso_fig1_sources_and_probability", 7.2, 6)

# ---- Figure 2 -------------------------------------------------------------------------------------------------
rho <- suppressWarnings(cor.test(water_iso$NO3NO2, water_iso$N15, method = "spearman"))
p2A <- ggplot(water_iso, aes(NO3NO2, N15)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), color = "gray20", fill = "gray60", linewidth = 0.7) +
  geom_point(aes(color = island), size = 1.8, alpha = 0.85) +
  scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100)) +
  scale_color_manual(values = island_pal, name = "Island") +
  annotate("label", x = Inf, y = Inf, hjust = 1.02, vjust = 1.2, size = 3, fill = "white", alpha = 0.85, linewidth = 0,
           label = sprintf("Spearman rho = %.2f (n = %d)", unname(rho$estimate), nrow(water_iso))) +
  labs(x = lab_nox, y = lab_d15N, tag = "A") + theme_iso_paper()

cls_breaks <- c(0, 3, 6, 12, 25, Inf)
cls_lab <- c("<3", "3-6", "6-12", "12-25", ">25")
nox_cls <- cut(water_iso$NO3NO2, cls_breaks, labels = cls_lab)
class_draws <- map_dfr(c("none", "moderate"), \(e) {
  f <- readRDS(file.path(mod_dir, paste0("mix_draws_f_", e, ".rds"))); f <- matrix(as.numeric(f), nrow(f), ncol(f))
  map_dfr(cls_lab, \(cl) {
    m <- rowMeans(f[, nox_cls == cl, drop = FALSE])
    tibble(enrichment = e, class = cl, n = sum(nox_cls == cl), mean = mean(m), lwr = quantile(m, 0.025), upr = quantile(m, 0.975))
  })
}) |> mutate(class = factor(class, cls_lab),
             setting = factor(if_else(enrichment == "none", "No enrichment allowance", "Enrichment allowance"),
                              c("No enrichment allowance", "Enrichment allowance")))
p2B <- ggplot(class_draws, aes(class, mean, color = setting, shape = setting)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.5), size = 0.45) +
  geom_text(data = distinct(class_draws, class, n), aes(class, 0.02, label = paste0("n = ", n)), inherit.aes = FALSE, size = 2.6, color = "gray30") +
  scale_color_manual(values = c("No enrichment allowance" = "#0072B2", "Enrichment allowance" = "#D55E00"), name = "Mixing model") +
  scale_shape_manual(values = c(16, 17), name = "Mixing model") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = expression(NO[3]+NO[2]~class~(mu*mol~L^{-1})), y = "Mean waste-associated fraction", tag = "B") +
  theme_iso_paper()
fig2 <- (p2A + p2B) + plot_layout(widths = c(1.15, 1), guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title.position = "top")
save_fig(fig2, "iso_fig2_nitrate_concentration_and_d15N", 7.2, 3.8)

# ---- Figure 3 -------------------------------------------------------------------------------------------------
sp_c <- suppressWarnings(cor.test(water_iso$cess_exposure_idx, water_iso$N15, method = "spearman"))
sp_a <- suppressWarnings(cor.test(water_iso$pct_agriculture, water_iso$N15, method = "spearman"))
p3A <- ggplot(water_iso, aes(cess_exposure_idx, N15)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray20", fill = "gray60", linewidth = 0.7) +
  geom_point(aes(color = island), size = 1.8, alpha = 0.85) +
  scale_x_continuous(trans = "log1p", breaks = c(0, 1, 5, 20, 80)) +
  scale_color_manual(values = island_pal, name = "Island") +
  annotate("label", x = Inf, y = Inf, hjust = 1.02, vjust = 1.2, size = 3, fill = "white", alpha = 0.85, linewidth = 0, label = sprintf("Spearman rho = %.2f", unname(sp_c$estimate))) +
  labs(x = "Cesspool exposure index (log scale)", y = lab_d15N, tag = "A") + theme_iso_paper()
p3B <- ggplot(water_iso, aes(pct_agriculture, N15)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray20", fill = "gray60", linewidth = 0.7) +
  geom_point(aes(color = island), size = 1.8, alpha = 0.85) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = island_pal, name = "Island") +
  annotate("label", x = Inf, y = Inf, hjust = 1.02, vjust = 1.2, size = 3, fill = "white", alpha = 0.85, linewidth = 0, label = sprintf("Spearman rho = %.2f", unname(sp_a$estimate))) +
  labs(x = "Agricultural land cover (ahupua\u02bba)", y = lab_d15N, tag = "B") + theme_iso_paper()

lu_j <- read_csv(file.path(tab_dir, "lu_joint_effects.csv"), show_col_types = FALSE)
lu_m <- read_csv(file.path(tab_dir, "lu_modular_effects.csv"), show_col_types = FALSE)
pick_terms <- tribble(~model, ~term, ~label,
  "M1_cess_ag", "z_log_cess_exposure", "Cesspool exposure", "M1_cess_ag", "z_pct_agriculture", "Agricultural land cover",
  "M2_plus_NOx", "z_log_NOx", "NOx concentration", "M3_plus_rain", "z_chirps_storm_cumulative_mm", "Storm rainfall")
eff <- bind_rows(
  lu_j |> inner_join(pick_terms, by = c("model", "term")) |>
    transmute(label, analysis = if_else(enrichment == "none", "Joint model, no enrichment allowance", "Joint model, enrichment allowance"), est = beta_mean, lwr = beta_lwr, upr = beta_upr),
  lu_m |> filter(enrichment == "none") |> inner_join(pick_terms, by = c("model", "term")) |>
    transmute(label, analysis = "Modular check (conservative)", est = estimate, lwr, upr)) |>
  mutate(label = factor(label, rev(pick_terms$label)),
         analysis = factor(analysis, c("Joint model, no enrichment allowance", "Joint model, enrichment allowance", "Modular check (conservative)")))
p3C <- ggplot(eff, aes(est, label, color = analysis, shape = analysis)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), position = position_dodge(width = 0.6), size = 0.45) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "gray40"), name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  labs(x = "Effect on logit(waste-associated fraction) per SD of predictor", y = NULL, tag = "C") +
  theme_iso_paper() + theme(legend.position = "bottom", legend.direction = "vertical")
top3 <- wrap_elements(full = (p3A | p3B) + plot_layout(guides = "collect") & theme(legend.position = "bottom"))
fig3 <- top3 / p3C + plot_layout(heights = c(1, 1.05))
save_fig(fig3, "iso_fig3_land_use_drivers", 7.2, 7.4)

# ---- Figure 4 -------------------------------------------------------------------------------------------------
rn_lev <- read_csv(file.path(tab_dir, "rain_exposure_levels.csv"), show_col_types = FALSE)
rn_pred <- read_csv(file.path(tab_dir, "rain_joint_predictions.csv"), show_col_types = FALSE)
rn_bpred <- read_csv(file.path(tab_dir, "rain_brms_predictions.csv"), show_col_types = FALSE)
near <- pmap_dfr(list(rn_lev$exposure, rn_lev$z_lo, rn_lev$z_hi), \(ex, lo, hi) {
  col <- if (ex == "cesspool") "z_log_cess_exposure" else "z_pct_agriculture"
  map_dfr(c(low = lo, high = hi), \(z) water_iso[abs(water_iso[[col]] - z) <= 0.5, ] |> transmute(rain_mm = chirps_storm_cumulative_mm), .id = "level") |>
    mutate(exposure = ex)
})
rn_range <- near |> group_by(exposure, level) |> summarise(rain_min = min(rain_mm), rain_max = max(rain_mm), n = n(), .groups = "drop")
ex_lab <- c(cesspool = "Cesspool exposure", agriculture = "Agricultural land cover")
lev_cols <- c(low = "#0072B2", high = "#D55E00")
lev_labs <- c(low = "Low exposure (10th percentile)", high = "High exposure (90th percentile)")
clip <- \(d) d |> left_join(rn_range, by = c("exposure", "level")) |> filter(rain_mm >= rain_min, rain_mm <= rain_max) |>
  mutate(level = factor(level, c("low", "high")), exposure = factor(exposure, names(ex_lab), ex_lab))
near_f <- near |> mutate(level = factor(level, c("low", "high")), exposure = factor(exposure, names(ex_lab), ex_lab))

pf <- rn_pred |>
  filter((model == "M4a_rain_x_cess" & exposure == "cesspool") | (model == "M4b_rain_x_ag" & exposure == "agriculture")) |> clip() |>
  mutate(setting = if_else(enrichment == "none", "No enrichment allowance", "Enrichment allowance"))
p4A <- ggplot(pf, aes(rain_mm, fraction, color = level, fill = level)) +
  geom_ribbon(data = filter(pf, enrichment == "none"), aes(ymin = lwr, ymax = upr, group = level), alpha = 0.18, color = NA) +
  geom_line(aes(linetype = setting), linewidth = 0.8) +
  geom_rug(data = near_f, aes(x = rain_mm, color = level), inherit.aes = FALSE, sides = "b", alpha = 0.5, length = unit(0.03, "npc")) +
  facet_wrap(~exposure) +
  scale_color_manual(values = lev_cols, labels = lev_labs, name = NULL) + scale_fill_manual(values = lev_cols, guide = "none") +
  scale_linetype_manual(values = c("No enrichment allowance" = "solid", "Enrichment allowance" = "dashed"), name = NULL) +
  coord_cartesian(xlim = c(120, 700), ylim = c(0, 1)) +
  labs(x = "Storm rainfall (mm)", y = "Predicted waste-associated fraction", tag = "A") + theme_iso_paper()

pb <- rn_bpred |>
  filter((model == "M4a_rain_x_cess" & exposure == "cesspool") | (model == "M4b_rain_x_ag" & exposure == "agriculture")) |> clip()
p4B <- ggplot(pb, aes(rain_mm, d15N, color = level, fill = level)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = level), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_rug(data = near_f, aes(x = rain_mm, color = level), inherit.aes = FALSE, sides = "b", alpha = 0.5, length = unit(0.03, "npc")) +
  facet_wrap(~exposure) +
  scale_color_manual(values = lev_cols, labels = lev_labs, name = NULL) + scale_fill_manual(values = lev_cols, guide = "none") +
  coord_cartesian(xlim = c(120, 700)) +
  labs(x = "Storm rainfall (mm)", y = expression(Predicted~delta^{15}*N~"(per mil)"), tag = "B") +
  theme_iso_paper() + theme(strip.text = element_blank())
fig4 <- (p4A / p4B) + plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.direction = "vertical")
save_fig(fig4, "iso_fig4_rainfall_interactions", 7.2, 6.4)

# ---- Figure 5 -------------------------------------------------------------------------------------------------
# Drivers of NOx in the GAM (Figure 5 model, nox_M_int in kona_low_nutrient_analysis.qmd; standardized effects on
# log NOx, 95% intervals) next to the isotope-based tests of the same drivers (effects on the logit of the
# waste-associated fraction per SD, joint model).
gam_eff <- tribble(
  ~driver, ~est, ~lwr, ~upr,
  "Cesspool exposure", 0.137, 0.0106, 0.263,
  "Agricultural land cover", 0.462, 0.112, 0.813,
  "Rainfall x cesspool", 0.203, 0.073, 0.333,
  "Rainfall x agriculture", 0.659, 0.243, 1.08)
drv_order <- rev(gam_eff$driver)
iso_pick <- tribble(~model, ~term, ~driver,
  "M1_cess_ag", "z_log_cess_exposure", "Cesspool exposure", "M1_cess_ag", "z_pct_agriculture", "Agricultural land cover",
  "M4a_rain_x_cess", "rain_x_cess", "Rainfall x cesspool", "M4b_rain_x_ag", "rain_x_ag", "Rainfall x agriculture")
iso_eff <- lu_j |> inner_join(iso_pick, by = c("model", "term")) |>
  transmute(driver, setting = factor(if_else(enrichment == "none", "No enrichment allowance", "Enrichment allowance"),
                                     c("No enrichment allowance", "Enrichment allowance")),
            est = beta_mean, lwr = beta_lwr, upr = beta_upr) |>
  mutate(driver = factor(driver, drv_order))
p5A <- ggplot(mutate(gam_eff, driver = factor(driver, drv_order)), aes(est, driver)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), color = "gray20", size = 0.5) +
  labs(x = expression(atop("Standardized effect on", log(NO[3]+NO[2])~"(GAM)")), y = NULL, tag = "A") + theme_iso_paper()
p5B <- ggplot(iso_eff, aes(est, driver, color = setting, shape = setting)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), position = position_dodge(width = 0.55), size = 0.45) +
  scale_color_manual(values = c("#0072B2", "#D55E00"), name = "Mixing model") + scale_shape_manual(values = c(16, 17), name = "Mixing model") +
  labs(x = expression(atop("Effect on logit(waste-associated", "fraction) per SD (two-source isotopes)")), y = NULL, tag = "B") +
  theme_iso_paper() + theme(axis.text.y = element_blank())
# Three-source joint model (isotope_08): cesspool -> waste vs background, agriculture -> fertilizer vs background
ag3 <- read_csv(file.path(tab_dir, "ag3_joint_effects.csv"), show_col_types = FALSE)
ag3_eff <- bind_rows(
  ag3 |> filter(equation == "waste_vs_bg", term == "z_log_cess_exposure") |> mutate(driver = "Cesspool exposure"),
  ag3 |> filter(equation == "fert_vs_bg", term == "z_pct_agriculture") |> mutate(driver = "Agricultural land cover")) |>
  transmute(driver = factor(driver, drv_order),
            setting = factor(if_else(enrichment == "none", "No enrichment allowance", "Enrichment allowance"),
                             c("No enrichment allowance", "Enrichment allowance")),
            est = beta_mean, lwr = beta_lwr, upr = beta_upr)
p5C <- ggplot(ag3_eff, aes(est, driver, color = setting, shape = setting)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), position = position_dodge(width = 0.55), size = 0.45) +
  scale_y_discrete(limits = drv_order, drop = FALSE) +
  scale_color_manual(values = c("#0072B2", "#D55E00"), name = "Mixing model") + scale_shape_manual(values = c(16, 17), name = "Mixing model") +
  labs(x = expression(atop("Effect on log-ratio to background", "per SD (three-source isotopes)")), y = NULL, tag = "C") +
  theme_iso_paper() + theme(axis.text.y = element_blank())
verdict_txt <- c("Rainfall x agriculture" = "Inconclusive", "Rainfall x cesspool" = "Inconclusive",
                 "Agricultural land cover" = "Waste fraction falls;\nfertilizer effect\nnot informed",
                 "Cesspool exposure" = "Suggestive\n(positive)")
verdict <- tibble(driver = factor(drv_order, drv_order), text = unname(verdict_txt[drv_order]))
p5D <- ggplot(verdict, aes(0, driver, label = text)) + geom_text(hjust = 0, size = 3, lineheight = 0.9) +
  scale_x_continuous(limits = c(0, 1)) + labs(x = NULL, y = NULL, tag = " ") +
  theme_void(base_size = 9) + theme(plot.tag = element_text(size = 11))
fig5 <- (p5A + p5B + p5C + p5D) + plot_layout(widths = c(1, 1, 1, 0.6), guides = "collect") &
  theme(legend.position = "bottom", legend.direction = "horizontal")
save_fig(fig5, "iso_fig5_gam_vs_isotope_drivers", 9.6, 3.8)

message("figures written to ", fig_dir)
