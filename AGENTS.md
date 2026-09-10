# What's In Your Water — Project Memory & Analysis Framework

**Principal Investigators:** Dr. Nyssa Silbiger, Dr. Andrea Kealoha, Dr. Sara Kahanamoku (University of Hawaiʻi)
**Target journal:** Nature Communications or Environmental Science & Technology
**Current clean analysis file:** `kona_low_nutrient_analysis.qmd`

---

## ⚠ TOP-PRIORITY TASK — Acquire Molokaʻi cesspool coverage

**Status: OPEN. Do this before finalizing any cesspool-related result, figure, or the title decision.**

Molokaʻi currently has **no** cesspool data (all cesspool metrics `NA`), which forced the
two-tier design (four-island primary models without cesspools + Oʻahu/Maui-only cesspool
submodels). A Molokaʻi on-site sewage disposal (OSDS) layer is available and must be
downloaded and integrated.

**Source (Hawaiʻi Statewide GIS / DOH):**
- Portal page: https://geoportal.hawaii.gov/datasets/d5dd71c0b4b0444080b2eadff3edbf77_23/explore
- Item title: **"On-site Sewage Disposal Systems - Molokai"** (DOH source-water protection program, systems as of 2010; fields include approximate location, OSDS type, effluent, and nitrogen/phosphorus flux).
- **ArcGIS REST endpoint:** `https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer/23`
- Download via REST query (mirror `02_download_cesspool_layers.R`): `.../MapServer/23/query?where=1=1&outFields=*&f=geojson`. Cache raw to `data/molokai_osds_layer.geojson`.

**KEY FINDING — the integration code already exists.** `02_download_cesspool_layers.R`
already downloads layer 23, filters to **`class_i > 0` (Class I OSDS = cesspools /
direct-discharge pits)**, and merges those points into the same combined point layer as
the HCPT layer-32 points before computing ahupuaʻa density and sample distances. This is
the canonical cesspool definition for the project — Class I, treated as points exactly
like the HCPT layer (methodologically consistent with Oʻahu/Maui). The reason Molokaʻi is
still `NA` is that the CSVs were **never regenerated** after this code was added (and the
buffer-count step is separate). The task is therefore to **run the pipeline and rebuild
downstream files**, not to write new download logic.

**Processing (composite key `c("ahupuaa","mokupuni")` on every ahupuaʻa join):**
1. Re-run `02_download_cesspool_layers.R` (Class I filter already in place) to regenerate `data/cesspool_by_ahupuaa.csv` (`cesspool_density_per_km2`, `n_cesspools`) and `data/sample_cesspool_distances.csv` (`dist_to_nearest_cesspool_m`) **with Molokaʻi rows**. Note this re-runs the 82k-point HCPT download and sources `01_load_clean_data.R` (Google Sheets auth).
2. Regenerate `data/cesspool_buffer_counts.csv` (`n_cess_500m`, `n_cess_1km`) from the combined point layer so Molokaʻi buffer counts are populated (currently NA).
3. Rebuild the cesspool columns of `chem_master.csv` so Molokaʻi is no longer `NA` (re-run the relevant `kona_low_nutrient_analysis.qmd` chunks).
4. **Verify** the Class I filter still matches the layer's current schema (fields include `class_i..iv`, `type`, `osds_qty`, `n_flux`, `p_flux`; raw cached to `data/molokai_osds_layer.geojson`, 1,651 records / ~1,956 systems).

**Downstream analyses to update once Molokaʻi cesspool data exist (all in `kona_low_nutrient_analysis.qmd`):**
- Reconsider the **two-tier design**: with Molokaʻi covered, the cesspool submodels expand to three islands (Oʻahu, Maui, Molokaʻi); evaluate whether cesspools can enter the *primary* models directly. Lānaʻi remains uncovered.
- Re-run the NO₃+NO₂ (Step 12), PO₄ (Step 13), NH₄ (Step 14) cesspool submodels; the Step 15 Figure 5 two-scope plot; the Step 17 amplification test + leave-one-ahupuaʻa-out CV (now able to include Molokaʻi, and possibly leave-one-island-out across three islands); the Step 23 cesspool-metric sensitivity.
- Update every callout/caption/table that currently states "Molokaʻi NA" or "Oʻahu + Maui only."

**Comparability caveat (must document):** the Molokaʻi Class I points come from a
**different source and vintage** (DOH OSDS 2010) than the HCPT layer 32 used for
Oʻahu/Maui, even though the pipeline treats both uniformly as points → density. Cross-island
cesspool magnitude comparisons may still be affected by inventory completeness/vintage.
Add a source/vintage indicator, prefer within-island relative metrics, and report a
sensitivity with and without Molokaʻi. Once Molokaʻi is populated, revisit the two-tier
design (cesspool submodels expand to three islands; consider cesspools in the primary
models) and re-run Steps 12–15, 17, and 23; Lānaʻi remains uncovered.

---

## Analysis Framework: Extreme Rainfall, Watershed Connectivity, and Coastal Nutrient Exposure Across Hawaiʻi

### Preferred title

**Extreme rainfall reveals human amplification of land-to-sea nutrient transport across tropical coastlines**

### Alternative titles

- **Landscape modification amplifies coastal nutrient exposure during extreme rainfall**
- **Watershed structure predicts coastal nutrient hotspots following an extreme rainfall event**
- **Extreme rainfall activates contrasting nutrient pathways from land to sea**

### Central framing

This paper should not be framed simply as a description of nutrient concentrations following the March 2026 Kona Low. Instead, use the storm as a large-scale natural experiment that simultaneously perturbed hundreds of watersheds across four Hawaiian Islands.

The overarching conceptual framework is:

**Extreme rainfall → watershed connectivity → coastal nutrient delivery**

with chronic human modification potentially amplifying the response:

**Extreme rainfall × land use/infrastructure → magnitude and composition of coastal nutrient exposure**

The four chemical response variables are:

1. **NO₃+NO₂** — oxidized inorganic nitrogen
2. **PO₄** — dissolved inorganic phosphorus
3. **NH₄/NH₃** — reduced inorganic nitrogen; retain the variable name used in the analytical dataset (`NH3`) but report the chemically correct species/method in the manuscript
4. **SiO₂/Si** — dissolved silicate

Do not create a single composite "water quality" index as the main response. The preliminary analyses indicate that the nutrient species behave differently, and those differences should be used to understand mechanism.

Prior Hawaiʻi studies demonstrate that terrestrial groundwater and storm runoff can rapidly deliver nutrients to coastal ecosystems and that land use strongly modifies nutrient concentrations and fluxes. Knee et al. found inverse salinity relationships for nitrate+nitrite, phosphate, and silica along the Kona coast, while Bishop et al. showed that land use and groundwater flow paths strongly determine nutrient fluxes to Maui coastal waters. Storm studies in Kāneʻohe Bay have additionally shown that steep tropical watersheds can deliver rapid freshwater, sediment, and nutrient pulses to coastal waters on time scales that traditional monitoring often misses.

---

## Storm Context — March 2026 Kona Low

| Metric | Value |
|--------|-------|
| Peak rainfall | 68–124 mm/day (island mean, NCEP) |
| Peak discharge | 231–1,040 cfs (USGS gauges, Mar 14–15) |
| Post-storm chlorophyll bloom | 0.5–3 mg/m³ coastal mean (VIIRS) |
| Community science sampling window | March 28–April 8 (12–26 days post-peak) |
| WQP conventional monitoring records | **0 records** during storm window (last: Dec 18, 2025) |

---

## Overarching Questions and Hypotheses

Organize all analyses around four questions.

### Question 1 — How large and spatially heterogeneous was the coastal nutrient pulse?

**Hypothesis 1:** The Kona Low produced strong but spatially heterogeneous enrichment of dissolved nutrients across Hawaiian coastlines.

Preliminary findings: freshwater/storm-influenced samples had ~6× higher Si, ~3× higher NO₃+NO₂, and ~2.3× higher PO₄ than contemporaneous high-salinity marine-background samples. NH₄ should be allowed to behave differently rather than forced into this pattern.

---

### Question 2 — Was the 2026 event unusual relative to normal temporal variability?

**Hypothesis 2:** Routine rainfall generates relatively small coastal nutrient fluctuations around chronic background conditions, whereas the extreme March 2026 event generated nutrient anomalies outside the range normally observed.

Use the long-term Hui o Ka Wai Ola dataset and CHIRPS rainfall to establish temporal context. The appropriate interpretation is an **extreme-event response**, not proof of a formal ecological threshold unless threshold models provide statistical support.

---

### Question 3 — What controls where nutrients reach the ocean?

**Hypothesis 3:** Watershed hydrology and landscape structure determine terrestrial connectivity to the coast.

Predicted major controls: distance to stream mouth, rainfall/storm discharge, watershed slope, soil saturated hydraulic conductivity, coastal exposure/flushing. Si should be especially useful as a terrestrial-water tracer because it is enriched in Hawaiian groundwater through basalt weathering.

---

### Question 4 — Does human modification amplify nutrient enrichment beyond hydrologic delivery alone?

**Hypothesis 4:** After accounting for terrestrial-water delivery, coastal NO₃+NO₂ and PO₄ concentrations increase with anthropogenic watershed pressures such as cesspool density, agricultural land cover, impervious surface, population density, and/or permitted discharge infrastructure.

**This is the most important hypothesis in the paper.**

The conceptual distinction is:

- **Si / physical predictors** = how strongly land and ocean are connected
- **N and P relative to that terrestrial connection** = how nutrient-rich that delivered water is

Do not claim that any single source caused elevated nutrients unless the analysis can distinguish that source from correlated land-use variables.

---

## Analysis Steps

### STEP 0 — Start a Clean and Fully Reproducible Analysis

**Filename:** `kona_low_nutrient_analysis.qmd`

**Sections:**
1. Setup
2. Data inventory and QA/QC
3. Sampling coverage
4. Event magnitude and long-term context
5. Spatial nutrient patterns
6. Terrestrial connectivity
7. Human amplification
8. Nutrient-specific models
9. Variance partitioning
10. Out-of-sample hotspot prediction
11. Sensitivity analyses
12. Publication figures
13. Supplementary analyses
14. Model diagnostics
15. Reproducibility information

**Rules:**
- Use `here::here()` for all paths
- Set one global random seed at top
- Do not overwrite any raw or existing processed data
- Create: `output/main_figures/`, `output/supplement/`, `output/tables/`, `output/model_objects/`
- Save every final model as `.rds`
- Save all figures as both PDF and high-resolution PNG/TIFF

---

### STEP 1 — Audit the Sample Data Before Doing Any Modeling

The current exploratory document describes **694 samples**; the full project now contains approximately **780 samples**. Do not begin by complete-case filtering.

Load: `data/clean_chemdata.csv`

Produce an explicit sample-flow table:

| Stage | n samples | n sites | n islands |
|---|--:|--:|--:|
| Raw chemistry dataset | | | |
| Coordinates available | | | |
| NO₃+NO₂ available | | | |
| PO₄ available | | | |
| NH₄ available | | | |
| Si available | | | |
| Ahupuaʻa assigned | | | |
| Stream-distance data | | | |
| Land-cover data | | | |
| Cesspool data | | | |
| Full landscape model dataset | | | |

**Do not** require all four nutrients to be present for nutrient-specific models. A sample with NO₃+NO₂ but missing NH₄ should still enter the NO₃+NO₂ model. Report actual sample size for every analysis.

Determine whether multiple samples share identical or nearly identical coordinates and whether these represent technical replicates, same-site same-date samples, repeat samples through time, or independent nearby observations. Create a stable `site_id`. Retain `sample_id`.

---

### STEP 2 — Load and Document the Existing Spatial Covariates

See **Data Sources** section below for file paths and variable names.

---

### STEP 3 — Create One Master Analytical Dataset

Build `analysis_master` with one row per water sample. Never aggregate chemistry before building this master table.

**Include:**
- Sample identity: `sample_id`, `site_id`, `island`, date/time, `latitude`, `longitude`, `ahupuaa`, `moku`
- Responses: `NO3NO2`, `PO4`, `NH3`, `SiO2`
- Hydrologic/physical predictors: `dist_to_stream_mouth_m`, `mean_cfs_march`, `mean_slope_deg`, `mean_ksat_um_s`, `mean_gwetroot`, `fetch_mean_km`, `hs_mean_m`, `hs_at_collection_m`
- Anthropogenic predictors: `cesspool_density_per_km2`, `dist_to_nearest_cesspool_m`, cesspool count within 500 m, cesspool count within 1 km, `pct_agriculture`, `pct_impervious`, `pop_density_km2`, dist to NPDES, `cattle_per_km2`

Create a missingness matrix. Produce Pearson and Spearman correlations among all continuous predictors. Calculate VIF after choosing candidate model sets. Do not automatically drop predictors because of an arbitrary VIF rule.

---

### STEP 4 — Define Predictor Groups Before Looking at Model Results

This is essential for avoiding post hoc storytelling. Define four conceptual groups **before** fitting models.

**A. Storm/hydrologic forcing:** mean March discharge, antecedent soil wetness, spatially resolved CHIRPS storm rainfall

**B. Watershed transport/connectivity:** stream-mouth distance, mean slope, Ksat

**C. Anthropogenic nutrient sources:** cesspool metric, agricultural land cover, impervious cover, population density, NPDES distance, livestock (if retained)

**D. Coastal dilution/retention:** wave fetch, significant wave height

---

### STEP 5 — Improve the Rainfall Metric

Keep CHIRPS monthly island-mean for the **historical Hui analysis**. For the 780-sample spatial analysis, extract **spatially resolved storm rainfall** for each sampled watershed.

**Preferred metric:** cumulative CHIRPS rainfall over the primary storm interval for each ahupuaʻa or watershed.

Also calculate: 3-day max rainfall, 7-day cumulative rainfall, cumulative rainfall from storm onset to sampling date, days since peak rainfall.

If daily 2026 CHIRPS is available, make **cumulative rainfall before each individual sample** the preferred event-exposure variable.

---

### STEP 6 — Figure 1: Show the Scale of the Natural Experiment

Four-island map showing every sampling location. Do not show nutrient concentration yet. Show: Oʻahu, Maui, Molokaʻi, Lānaʻi, all sampling points, major stream mouths, optionally ahupuaʻa boundaries subtly, and Pacific inset. Include event timeline beside/beneath maps.

Report: n samples, n unique sites, coastline extent, n ahupuaʻa, n islands, sampling date range.

**Figure 1 message:** A distributed sampling network captured a regional extreme event across hundreds of coastal locations.

---

### STEP 7 — Quantify the Magnitude of Nutrient Enrichment

Analyze NO₃+NO₂, PO₄, NH₄, and Si separately. Use log scales and consider `log1p(response)`. Before transforming, inspect zeros, negatives, below-detection-limit values, extreme observations. Never convert true zeros or detection-limit values without documenting the rule.

Report: median, IQR, mean, 95% bootstrap CI, maximum, n.

Salinity comparison is descriptive/event-validation only. Do not use salinity as the central response or put it in the main mechanistic model. Perform sensitivity checks around storm/background salinity thresholds.

**Figure 2 candidate (4 panels: A=NO₃+NO₂, B=PO₄, C=NH₄, D=Si):** Storm-influenced vs marine-background concentrations. Raw points plus distributions, not bars. Display median fold enrichment [bootstrap 95% CI].

**Figure 2 message:** The storm signal was strongest for Si and oxidized N, whereas nutrient forms differed markedly in their enrichment patterns.

---

### STEP 8 — Establish Long-Term Context Using Hui o Ka Wai Ola

Load Hui files (see Data Sources). Construct station-specific historical baselines using **pre-2026 observations**. Require minimum historical n per station.

$$\text{FoldChange}_{it} = \frac{\text{Concentration}_{it}}{\text{HistoricalMedian}_i}$$

Calculate for NO₃+NO₂, PO₄, Si, NH₄ (if adequate historical data). Calculate monthly station-normalized anomalies. Pair with CHIRPS Maui rainfall 2016–2026.

---

### STEP 9 — Test Whether the 2026 Event Was Statistically Extreme

For each nutrient:
1. Rank March 2026 rainfall relative to all months
2. Rank March 2026 nutrient anomaly relative to all historical months
3. Calculate its percentile and standardized anomaly
4. Fit the routine rainfall–nutrient relationship **excluding** March 2026
5. Predict the expected anomaly for March 2026 rainfall
6. Calculate observed-minus-predicted deviation

Fit alternative relationships: linear, log, GAM, segmented regression if justified. Use AIC/LOO-CV to determine whether nonlinear behavior is supported. Do **not** use the word "threshold" unless segmented/nonlinear modeling supports one.

**Figure 3:** Panel A: 2016–2026 Maui monthly rainfall time series. Panel B: NO₃+NO₂ anomaly vs rainfall. Panels C–D: Si and PO₄ if informative.

**Figure 3 message:** The 2026 event produced coastal nutrient conditions outside the range typically associated with routine rainfall variability.

---

### STEP 10 — Characterize Spatial Structure Without Making Kriging the Inference

For each nutrient separately calculate: global Moran's I, spatial correlogram, empirical variogram, characteristic range of spatial autocorrelation. Test transformed and raw concentrations. Do this separately by island where sample size allows.

Use spatial interpolation for **visualization only**, not as evidence of measured values where no samples exist. Avoid extrapolating across mountain ridges, land, unsampled coastlines, wide bays, or long gaps. If kriging is retained, mask to a narrow coastal buffer and show observed points on top.

**Figure 4 candidate:** Four nutrient maps or one nutrient at a time with islands faceted.

**Figure 4 message:** NO₃+NO₂ and Si produce coherent spatial hotspots, whereas NH₄ is considerably more localized/heterogeneous. (Only state this if Moran's I/variograms support it.)

---

### STEP 11 — Model Terrestrial Connectivity Using Si First

**Response:** log(Si)

**Candidate predictors:** cumulative storm rainfall, log stream-mouth distance, log storm discharge, slope, log Ksat, antecedent soil moisture, wave exposure, island.

**Priority interaction:** StreamDistance × Slope (preliminary analysis suggests effect of stream-mouth proximity changes with catchment slope). Also Rainfall × Ksat if justified.

**Preferred model structure:**
$$\log(Si_{ijk}) = \alpha + \beta X + u_{\text{island}} + u_{\text{ahupuaa}} + \text{SpatialEffect} + \epsilon$$

If repeated observations exist at sites, include site as additional random effect. Possible R frameworks: `brms`, `mgcv`, `glmmTMB` + spatial structure, `sdmTMB`. Choose based on diagnostics.

**Scientific interpretation:** This model estimates which landscape characteristics determine the strength of **terrestrial-water delivery to coastal waters during the storm**. Use language "terrestrial-water delivery" not exclusively "surface runoff" (Hawaiian dissolved Si can identify meteoric groundwater as well as surface pathways).

---

### STEP 12 — Model NO₃+NO₂ After Accounting for Terrestrial Delivery

**The central analysis of the paper.**

$$\log(NO_x) = \beta_1\log(Si) + \beta_2\text{Cesspool} + \beta_3\text{Agriculture} + \beta_4\text{Impervious} + \beta_5\text{NPDES} + \beta_6\text{Population} + \text{Physical} + \text{Random/Spatial}$$

**Key question:** After sites differ in how strongly they are connected to terrestrial water, do human land-use variables explain additional variation in coastal nitrogen?

Test interactions only where conceptually justified: Rainfall × Cesspool, Rainfall × Agriculture, Si × Cesspool (does high anthropogenic source strength produce disproportionately high N when hydrologic connectivity is also strong?).

---

### STEP 13 — Model PO₄ the Same Way

Fit a parallel PO₄ model using the same conceptual predictor groups. Do not describe PO₄ as a specific wastewater tracer. Ask whether watershed wastewater infrastructure or other anthropogenic land-use variables explain PO₄ enrichment **beyond** terrestrial-water transport. Determine whether the cesspool–PO₄ relationship survives island effects, spatial autocorrelation, Si/transport adjustment, and other correlated source variables.

---

### STEP 14 — Treat NH₄ as an Important Contrast

Run the same broad modeling framework for NH₄ but do not expect identical predictors. If explanatory power is substantially weaker, report it. Do not interpret weak spatial predictability as a failure.

Evaluate whether NH₄: has weaker Moran's I, has shorter spatial autocorrelation range, has lower R², is poorly predicted by watershed-scale variables.

If so, the finding is: **Reduced inorganic N is controlled at finer spatial or biogeochemical scales than NO₃+NO₂.** Describe possible mechanisms (regeneration, uptake, sediment transformations, redox processes, localized inputs) as hypotheses rather than conclusions.

---

### STEP 15 — Standardize the Four Models and Compare Effect Sizes

Use identical standardized predictor definitions wherever possible. Create a coefficient plot with one row per predictor and separate estimates for Si, NO₃+NO₂, PO₄, NH₄. Show standardized effect size ± 95% CI. Group predictor labels visually into: Hydrologic forcing, Watershed transport, Anthropogenic loading, Coastal dilution.

**Figure 5 (most important mechanistic figure):**
- Si dominated by physical/hydrologic connectivity
- NO₃+NO₂ influenced by connectivity + anthropogenic land use
- PO₄ influenced by connectivity + selected land-use variables
- NH₄ weakly explained or controlled by different factors

**Figure 5 message:** Nutrient forms record different components of the land-to-sea transport process.

---

### STEP 16 — Partition Variance Among Mechanistic Driver Groups

For each nutrient quantify unique and shared explanatory contribution of: (1) storm/hydrologic forcing, (2) watershed connectivity, (3) anthropogenic sources, (4) coastal dilution/retention, (5) spatial/island structure.

If Bayesian models: compare nested models with LOO and calculate changes in Bayesian R².

M0 = random/spatial effects only → M1 = M0 + hydrology → M2 = M1 + watershed connectivity → M3 = M2 + anthropogenic → M4 = M3 + coastal dilution

**Figure 6 candidate:** Stacked/grouped variance figure for four nutrients.

| Response | Hydrology | Connectivity | Human | Coastal | Residual |
|---|---|---|---|---|---|
| Si | | | | | |
| NO₃+NO₂ | | | | | |
| PO₄ | | | | | |
| NH₄ | | | | | |

---

### STEP 17 — Test "Human Amplification" Explicitly

**Model A:** Hydrology + connectivity + coastal environment
**Model B:** Model A + anthropogenic variables
**Model C:** Model B + hydrology × anthropogenic interactions

Compare using spatially structured cross-validation. Report: ΔR² or Bayesian R², ΔRMSE, ΔLOO/ELPD, coefficient/interaction uncertainty.

The claim **"human land use amplifies extreme-event nutrient delivery"** should only appear in the title if Models B/C clearly outperform the physical-only model and relevant anthropogenic effects are robust. Otherwise use the safer title: **Watershed structure predicts coastal nutrient exposure following extreme rainfall**.

---

### STEP 18 — Use Spatial Cross-Validation

Random train/test splits are **inappropriate** because neighboring samples are not independent. Use blocked spatial cross-validation.

Packages: `blockCV` or custom leave-one-ahupuaʻa-out / leave-region-out folds.

Run at minimum:
- **A. Spatial block CV**
- **B. Leave-one-island-out CV** — asks whether relationships learned on three islands predict the fourth; tests generalizability

Report for all four nutrients: R², RMSE, MAE, calibration slope, observed vs predicted plot.

---

### STEP 19 — Retain Random Forest, But Repurpose It

Do not use random forest as the primary inferential model. Use it for the applied question: **Can publicly available landscape information identify nutrient hotspots after an extreme event?**

Fit separate continuous-response random forests for NO₃+NO₂, PO₄, NH₄, Si. Do not classify PAM water types as the principal endpoint. Use spatial cross-validation. Generate: permutation variable importance, accumulated local effects or carefully interpreted PDPs, observed vs predicted. Do not interpret variable importance as causality.

---

### STEP 20 — Identify Coastal Vulnerability Without Arbitrary Clustering

Define high nutrient exposure objectively: upper 10%, upper 20%, scientifically defensible reference threshold, or posterior probability exceeds a selected value. Avoid defining "hotspots" from the data and then testing predictors of those same data without cross-validation. Create vulnerability maps based on **cross-validated model predictions**. If extrapolating beyond sampled watersheds, mask areas outside predictor space in training data.

---

### STEP 21 — Examine Interactions That Test Mechanism

**Priority interactions only:**

1. **StreamDistance × Slope** — Does proximity to a stream matter more in steep watersheds?
2. **Rainfall/Discharge × CesspoolDensity** — Does greater hydrologic forcing produce larger N/P response where wastewater is denser?
3. **Rainfall/Discharge × Agriculture** — Does extreme forcing increase coastal nitrogen disproportionately in agricultural watersheds?
4. **Rainfall × Ksat** — Does substrate permeability alter how precipitation translates into coastal nutrient delivery?

Visualize interactions using predicted marginal effects with 95% intervals. Do not discretize continuous predictors for inference; quantiles may be used only to make interaction plots easier to explain.

---

### STEP 22 — Test Whether Si:N or N:P Ratios Add Real Information

Do not automatically include in the main paper. N:P and Si:N ratios are mathematically coupled to the response variables and can generate misleading correlations. If retained, make them supplementary descriptive analyses.

Avoid: "Low N:P proves wastewater" or "High Si:N proves volcanic runoff." Use: "nutrient stoichiometry varied among locations in ways consistent with differences in terrestrial source composition and transport."

---

### STEP 23 — Build a Formal Sensitivity-Analysis Section

Repeat key results under plausible analytical alternatives. At minimum:
1. log vs alternative transformations
2. with and without extreme concentration observations
3. with and without island effects
4. spatial model vs nonspatial mixed model
5. multiple cesspool metrics
6. multiple agricultural metrics if available
7. stream-gauge assignment thresholds of 10, 15, and 20 km
8. salinity-background thresholds varied around exploratory values
9. samples aggregated to site/date vs all samples
10. leave-one-island-out analyses
11. with and without Si as a covariate in the N/P models

Create a supplementary table showing whether the sign and magnitude of primary effects remain stable.

---

### STEP 24 — Deal Explicitly with Pseudoreplication

Multiple observations within the same ahupuaʻa share identical landscape covariates. Do not treat every observation as an independent watershed replicate.

Use hierarchical random effects: `(1|Island/Ahupuaa/Site)` as supported by data structure. Alternatively aggregate to site/date for landscape analyses and use sample-level model as sensitivity check.

Report n samples, n sites, n ahupuaʻa, n islands for every primary model.

---

### STEP 25 — Publication Figure Set (~6 principal figures)

| Fig | Content | Take-home |
|---|---|---|
| 1 | Sampling design + storm timeline | Extreme rainfall captured across hundreds of coastal locations on four islands |
| 2 | NO₃+NO₂, PO₄, NH₄, Si distributions + storm/background contrasts | Magnitude of enrichment differed strongly among nutrient forms |
| 3 | CHIRPS rainfall + Hui long-term nutrient anomalies | March 2026 generated conditions outside normal variability |
| 4 | Spatial distributions of four nutrients | Coastal nutrient exposure was highly heterogeneous and nutrient-specific |
| 5 | Standardized coefficient/effect-size plot for four nutrient models | Physical connectivity determines delivery; human land use explains additional N/P enrichment |
| 6 | Variance partitioning by driver group OR spatially cross-validated vulnerability map | Landscape characteristics allow hotspot prediction before sampling |

---

### STEP 26 — Publication Graphics Standards

Create one custom ggplot theme: `theme_kona_paper()`

Requirements:
- White background, minimal grid lines, consistent font
- ~8–10 pt final-size text
- No unnecessary legends
- Panel letters A, B, C, D
- Units on every axis (μmol L⁻¹ consistently)
- Subscript chemical notation
- Accessible palettes (no rainbow)
- Maps use the same concentration scale for a nutrient across islands; log color scales where justified
- Interpretable in grayscale where possible
- Save: PDF vector + 600-dpi TIFF or PNG
- Never put statistical interpretation only in figure captions

---

### STEP 27 — Produce Model-Diagnostic Figures Automatically

For every final model save: residual vs fitted, QQ plot, posterior predictive check (if Bayesian), spatial map of residuals, residual Moran's I, leverage/influence diagnostics, observed vs predicted, cross-validation performance.

A model is not final if residual spatial autocorrelation remains strong.

---

### STEP 28 — Generate Publication Tables

**Table 1:** Sampling summary by island (n samples, n sites, n ahupuaʻa, date range, median and IQR for each nutrient).

**Table 2:** Data source and predictor table (Variable, Process represented, Spatial scale, Source, Transformation, Missingness).

**Table 3:** Primary model results (standardized estimate, 95% interval, p-value or posterior probability, model R², spatial CV R²).

**Supplementary table:** Sensitivity-analysis results.

---

### STEP 29 — Primary Results Story to Test

Do not write these as conclusions until the final models support them.

1. A single extreme rainfall event generated widespread but spatially heterogeneous coastal nutrient enrichment across four Hawaiian Islands.
2. The March 2026 event produced nutrient anomalies outside the range typically associated with routine rainfall variability in a decade-long coastal monitoring record.
3. Terrestrial-water connectivity (stream proximity, topography, hydrology, soil properties) explained where storm-derived material reached the coast.
4. After accounting for terrestrial connectivity, human land use explained additional variation in coastal NO₃+NO₂ and/or PO₄ exposure.
5. Different nutrient forms retained contrasting landscape signatures: Si and NO₃+NO₂ were comparatively coherent at watershed-to-coast scales, whereas NH₄ was more locally variable.
6. Publicly available landscape information allowed identification of coastlines most vulnerable to nutrient exposure during extreme rainfall.

---

### STEP 30 — Discussion Framework

1. **Extreme events reveal land–sea connections that routine monitoring misses** — dense sampling addresses the observational gap created by mismatch between storm timescales and monthly monitoring cycles.
2. **The same storm does not produce the same coastal exposure everywhere** — shifts interpretation from "large storm = large pulse" to "large storm × landscape = coastal exposure."
3. **Extreme events mobilize chronic anthropogenic pressures** — long-term land-use decisions determine what becomes available for transport when hydrologic connectivity increases. The advance is testing mechanisms simultaneously across hundreds of coastal locations during a single large disturbance.
4. **Different nutrients record different stages of the land-to-sea pathway** — Si reflects terrestrial-water connectivity; NO₃+NO₂ reflects connectivity + N sources/transformations; PO₄ may reflect source composition + geochemical controls; NH₄ may reflect shorter-scale source and biological transformations.

---

### STEP 31 — Language to Avoid

| Avoid | Use instead |
|---|---|
| "cesspools caused high nitrate" | "NO₃+NO₂ concentrations increased with cesspool density after accounting for hydrologic and landscape covariates" |
| "Si proves runoff" | "Si provides an indicator of terrestrial-water influence" |
| "NH₄ comes from sediment remineralization" | "NH₄ showed weaker watershed-scale predictability, consistent with stronger influence of localized sources and rapid biogeochemical cycling" |
| "ecological consequences" | "coastal nutrient exposure", "coastal biogeochemical disturbance", "land-to-sea nutrient transport" |

---

### STEP 32 — Journal-Level Framing

**Nature Communications:** demonstrate something broader than Hawaiʻi — *Extreme climatic disturbances reveal how chronic landscape modification controls the transfer of nutrients between terrestrial and coastal ecosystems.*

**ES&T:** emphasize — *Predicting the mobilization, transport, and coastal exposure of anthropogenic nutrients during an extreme hydrologic event.*

The novelty is the ability to **resolve the interaction between extreme-event hydrology, watershed properties, and human nutrient sources at an unusually large spatial scale.** Not simply "780 samples."

---

### STEP 33 — Conditions for the Strongest Title

Use **"Extreme rainfall reveals human amplification of land-to-sea nutrient transport across tropical coastlines"** only if:
1. Anthropogenic variables explain substantial additional NO₃+NO₂ and/or PO₄ variation after controlling for terrestrial connectivity
2. Effects are robust to spatial structure and island
3. Spatial cross-validation confirms predictive value
4. Results are not driven by one island or a handful of extreme sites

Fallback: **"Watershed structure predicts coastal nutrient exposure following extreme rainfall"**

If nutrient-specific contrasts dominate: **"Extreme rainfall activates contrasting land-to-sea nutrient pathways across tropical coastlines"**

---

### STEP 34 — Final Analysis Checklist

At the end, automatically write a section entitled **"What the analyses actually support"** with: (1) three strongest supported conclusions, (2) conclusions that are suggestive but uncertain, (3) hypotheses not supported, (4) which proposed title is justified.

Do not force the results to match the proposed story.

Checklist items:
- [ ] Sample attrition audited
- [ ] Response-specific sample sizes reported
- [ ] Repeated sites identified
- [ ] Predictor collinearity evaluated
- [ ] Spatial rainfall calculated
- [ ] Long-term Hui comparison completed
- [ ] Four nutrient spatial structures evaluated
- [ ] Si connectivity model completed
- [ ] NO₃+NO₂ human-amplification model completed
- [ ] PO₄ model completed
- [ ] NH₄ model completed
- [ ] Model residual spatial autocorrelation checked
- [ ] Hierarchical structure accounted for
- [ ] Spatial cross-validation completed
- [ ] Leave-one-island-out prediction completed
- [ ] Variance partitioning completed
- [ ] Key interactions tested
- [ ] Sensitivity analyses completed
- [ ] Main figures exported
- [ ] Supplementary figures exported
- [ ] Tables exported
- [ ] All model objects saved
- [ ] `sessioninfo::session_info()` saved
- [ ] All claims updated to match actual model results

---

## Code Rules

### All libraries must be loaded in the setup chunk

Every `library()` call must appear in the single `packages` setup chunk at the top of `kona_low_nutrient_analysis.qmd` (or equivalent setup section in any new analysis document). Never place `library()` calls inside individual analysis chunks, even for packages that are only used once. If a new package is needed, add it to the setup chunk, not inline.

---

## Language Rules

### Never use "citizen science" — always "community science"

All text, figure labels, captions, annotations, code comments, and manuscript prose must use **"community science"** (or "community scientist"). The term "citizen science" is never acceptable in any output from this project. This applies to:
- Quarto document prose and callouts
- Figure annotations and captions
- Code comments
- Tables and supplementary materials

---

## Critical Data Rules

### Ahupuaʻa joins MUST use a composite key

**Ahupuaʻa names are not unique across islands.** At least seven names recur on multiple islands:
Hālawa (Oʻahu, Molokaʻi), Kahana (Oʻahu, Maui), Kalihi (Oʻahu, Maui), Kawela (Oʻahu, Maui, Molokaʻi), Waiheʻe (Oʻahu, Maui), Wailua (Maui, Kauaʻi), Waimea (Oʻahu, Kauaʻi).

**Every join to an ahupuaʻa-level table must use `c("ahupuaa", "mokupuni")` as the composite key**, never `ahupuaa` alone. Joining on name alone silently assigns the wrong island's predictor values (confirmed: Lānaʻi Kamoku samples received cesspool data from Hawaiʻi Island's Kamoku before this was fixed).

The canonical island identifier in `ahupuaa_cache.csv` is the column `mokupuni`. Some covariate tables use the column name `island` instead — confirm encoding compatibility before renaming, or map explicitly. The `cesspool_by_ahupuaa.csv` table uses `island` with identical encoding to `mokupuni` and should be renamed before joining.

### Lānaʻi cesspool data status (confirmed correct after composite-key fix)

Lānaʻi does **not** appear in `cesspool_by_ahupuaa.csv` — there is no ahupuaʻa-level cesspool layer for the island. After the composite-key fix was applied, all Lānaʻi samples correctly receive:

- `cesspool_density_per_km2` = `NA` (island absent from cesspool table)
- `dist_to_nearest_cesspool_m` = `NA` (distances in the sample-level file pointed to cesspools on neighboring islands, 16–34 km away — not meaningful)
- `n_cess_500m` = 0 and `n_cess_1km` = 0 (correct: no cesspools in the HCPT layer within those buffers on Lānaʻi)

The earlier "Kamoku on Lānaʻi has 9 cesspools" note was a false alarm — those 9 cesspools belong to the Kamoku ahupuaʻa on Hawaiʻi Island, which shares the name. The composite-key join now prevents this collision.

### Island-name apostrophe encoding mismatch

**Two different apostrophe characters appear in island names across files**, causing silent join failures:

| Encoding | Byte | Character | Files |
|---|---|---|---|
| Unicode left single quote | U+2018 (8216) | `'` | `chem_master.csv` (`island` column), `ahupuaa_cache.csv` (`mokupuni`) |
| ASCII apostrophe | 39 | `'` | `nasa_power_soilmoist.csv`, `usda_nass_livestock_hi.csv` |

Confirmed failure: all 20 Lāna'i samples received `NA` for `mean_gwetroot` and `cattle_per_km2` until encoding was normalised.

**Always normalise before any island-level join** using:

```r
normalize_island <- function(x) str_replace_all(x, "['\u2018\u2019\u02bc]", "'")
```

Apply to both sides of the join when the source file's encoding is uncertain. This helper is defined in `kona_low_nutrient_analysis.qmd` (step2-load-remaining-covariates chunk).

### Molokaʻi cesspool data status

HCPT layer 32 does **not** cover Molokaʻi, so all cesspool metrics for Molokaʻi are
currently `NA` in `chem_master.csv`. **This is being superseded** — a Molokaʻi OSDS layer
has been identified and must be downloaded and integrated (see the **TOP-PRIORITY TASK**
section near the top of this file: `Infrastructure/MapServer/23`). Until that is done,
Molokaʻi drops out of all cesspool submodels and the two-tier design applies.

---

## Data Sources & Technical Reference

### Authentication & credentials

Google Sheets auth: `options(gargle_oauth_cache = ".secrets"); gs4_auth(email = "silbiger@hawaii.edu")`

### Core chemistry

| File | Key variables |
|---|---|
| `data/clean_chemdata.csv` | `sample_id`, `latitude`, `longitude`, `island`, `collected_hst`, `NO3NO2`, `PO4`, `NH3`, `SiO2`, `Salinity` |

**Reef health thresholds (µmol/L):** NO₃+NO₂ > 1, PO₄ > 0.1, SiO₂ > 5, NH₃ > 1

### Spatial units

| File | Contents |
|---|---|
| `data/ahupuaa_cache.csv` | Ahupuaʻa assignment per `sample_id` |
| `data/ahupuaa_boundaries.geojson` | Ahupuaʻa polygon boundaries |

Ahupuaʻa = traditional Hawaiian watershed polygons (~650 across 4 islands); primary spatial unit for predictor aggregation. Islands: Oʻahu, Maui, Molokaʻi (kriged), Lānaʻi (raw points only, n≈20).

### Cesspool variables

| File | Key variables |
|---|---|
| `data/sample_cesspool_distances.csv` | `dist_to_nearest_cesspool_m` per sample |
| `data/cesspool_by_ahupuaa.csv` | `cesspool_density_per_km2`, `n_cesspools` per ahupuaʻa |
| `data/cesspool_buffer_counts.csv` | `n_cess_500m`, `n_cess_1km` per sample |
| `data/molokai_osds_layer.geojson` | Molokaʻi OSDS points (to be downloaded — see TOP-PRIORITY TASK) |

Source: Hawaiʻi GIS (ArcGIS REST, layer 32) for Oʻahu/Maui. Note: Molokaʻi currently set to NA — HCPT layer 32 does not cover Molokaʻi (imputed with island median in RF pipeline); **a Molokaʻi OSDS layer (`Infrastructure/MapServer/23`, DOH 2010) is pending integration — see TOP-PRIORITY TASK, note the cross-source comparability caveat.** Lānaʻi = 0 (no layer documented).

Do not put multiple highly correlated cesspool metrics in the same inferential model without checking collinearity.

### Land cover (NOAA C-CAP)

| File | Key variables |
|---|---|
| `data/ccap_ahupuaa_summary.csv` | `pct_impervious`, `pct_agriculture` per ahupuaʻa |
| `data/ccap/` | GeoTIFFs: Oʻahu, Maui, Molokaʻi, Lānaʻi |

C-CAP classes: impervious = high/med/low developed (2, 3, 4); agriculture = cultivated + pasture (6, 7).

### Watershed slope

| File | Key variables |
|---|---|
| `data/ahupuaa_slope.csv` | `mean_slope_deg` per ahupuaʻa |

Source: NASA SRTM v3 via `elevatr`. Citation: Farr et al. 2007, Rev. Geophys. 45, RG2004.

### Soil hydraulic conductivity

| File | Key variables |
|---|---|
| `data/ahupuaa_ksat.csv` | `mean_ksat_um_s` per ahupuaʻa |

Source: USDA SSURGO via soilDB SDA API. Ksat = saturated hydraulic conductivity (µm/s), mean over top 100 cm. **Caution:** high permeability may reduce surface runoff while increasing groundwater transport — interpret direction from data.

### Stream mouths & discharge

| File | Key variables |
|---|---|
| `data/osm_stream_mouths.csv` | Stream mouth GPS coordinates (OpenStreetMap, 400 m coastal buffer filter) |
| `data/usgs_discharge_march2026.csv` | `mean_cfs_march`, `n_days`, per ahupuaʻa |
| `data/usgs_gauges_march2026.csv` | Gauge locations with `mean_cfs_march` |
| `data/usgs_discharge_cache.csv` | Full daily discharge time series for storm cascade figure |

Key gauges: Makaha Stream 16211600 (Oʻahu), Halawa Stream 16400000 (Molokaʻi), Hanawi Stream 16508000 (Maui). Current analysis assigns nearest gauge within 15 km. Conduct sensitivity at 10, 15, 20 km thresholds.

### NPDES outfalls

| File | Key variables |
|---|---|
| `data/epa_facilities_hi.csv` | NPDES facility lat/lon; derived `log_dist_outfall` per sample |

Source: US EPA ECHO 2025. Do not describe every NPDES facility as a wastewater source (includes multiple permitted types).

### Wave exposure & coastal flushing

| File | Key variables |
|---|---|
| `data/wave_fetch.csv` | `fetch_mean_km`, `fetch_min_km` per sample |
| `data/sample_waveheight.csv` | `hs_mean_m`, `hs_at_collection_m` (WW3 PacIOOS) |

Treat as coastal dilution/retention modifiers, not nutrient sources.

### Population

| File | Key variables |
|---|---|
| `data/pop_density_ahupuaa.csv` | `pop_density_km2` per ahupuaʻa |

Source: US Census ACS 2022, areal interpolation to ahupuaʻa.

### Antecedent soil moisture

| File | Key variables |
|---|---|
| `data/nasa_power_soilmoist.csv` | `mean_gwetroot` by island (Feb 1–Mar 10, 2026) |

Source: NASA POWER GWETROOT.

### Livestock

| File | Key variables |
|---|---|
| `data/usda_nass_livestock_hi.csv` | `cattle_per_km2` by island |

Source: USDA NASS 2022 Census of Agriculture. Coarse: Maui County combines Maui, Molokaʻi, Lānaʻi. Treat as weak/contextual; consider excluding from primary inferential model.

### Rainfall

| File | Key variables |
|---|---|
| `hawaii_ncep_janjun_cache.csv` | Daily `precip_mmday` by island, Jan–Jun 2026 (NCEP GDAS) |
| `data/chirps_maui_monthly_full.csv` | Monthly island-mean CHIRPS rainfall 2016–2026 |

### Remote sensing (cached)

| File | Contents |
|---|---|
| `hawaii_chlorophyll_cache.rds` | VIIRS NOAA-20 4 km daily Chl-a, Feb–Jun 2026 |
| `data/viirs_750m_bloom_cache.rds` | VIIRS 750 m bloom (Mar 20–Apr 8) |
| `data/viirs_750m_prestorm_extended.rds` | VIIRS 750 m pre-storm baseline (Jan 1–Mar 13) |
| `data/hawaii_bathy_1min.rds` | NOAA ETOPO bathymetry (depth QC for VIIRS) |

### Validation dataset (Hui o Ka Wai Ola)

| File | Contents |
|---|---|
| `data/background_nutrients/hui-south-maui-thru-2026-1st-quarter.0.xlsx` | South Maui fixed stations, monthly 2016–2026 |
| `data/background_nutrients/hui-west-maui-thru-2026-1st-quarter.0.xlsx` | West Maui fixed stations, monthly 2016–2026 |

Unit conversions: NNN ÷ 14.007 (µg/L N → µmol/L), Phosphate ÷ 30.974 (µg/L P → µmol/L), Silicate ÷ 28.086 (µg/L Si → µmol/L).

Fold-changes up to 43× above historical medians post-storm (Mālā Ramp). Used as independent validation of spatial patterns.

### Other spatial data (Allen Coral Atlas)

ACA WFS (public, no auth): `https://allencoralatlas.org/geoserver/ows`
Layers: `coral-atlas:benthic_data_verbose`, `coral-atlas:geomorphic_data_verbose`
Columns: `id`, `class_name`, `area_sqkm`, `geometry`
Coordinate order for queries: CRS:84 (lon_min, lat_min, lon_max, lat_max)
Cached at: `data/aca_hawaii_coral.rds`, `data/aca_reef_kriged_NO3.rds`

---

## Existing Analysis Files (Do Not Modify)

| File | Purpose |
|---|---|
| `nutrient_hotspot_maps.qmd` | Exploratory analysis (3,774 lines); reference only |
| `paper_figures.qmd` | Clean 4-figure paper figure set |
| `01_load_clean_data.R` | Google Sheets ingest, GPS parsing, data cleaning → `df_clean` |
| `02_download_cesspool_layers.R` | ArcGIS REST API → cesspool CSVs/RDS |
| `02_assign_neighborhoods.R` | Nominatim reverse geocoding (cached) |
| `03_wave_fetch_population.R` | Wave fetch + Census areal interpolation |
| `app.R` | Public Shiny dashboard (shinyapps.io) |
| `science_paper_roadmap.qmd` | Manuscript structure roadmap |

**New analysis goes in:** `kona_low_nutrient_analysis.qmd`

---

## Key Analytical Notes from Exploratory Work

- **PAM clustering (k=3):** Marine background (n≈262), Nearshore enriched (n≈309), Runoff-dominated (n≈123). Seed: 8341.
- **Random forest:** OOB accuracy varies (61–67%) vs ~36% null; seed 5572. Predictor set not finalized.
- **Moran's I:** SiO₂ and NO₃ significantly clustered (watershed-driven); NH₃ not (biology-driven).
- **Salinity thresholds:** Storm-influenced ≤ 34 PSU (any freshwater dilution); Marine background > 34 PSU. No samples excluded — the former middle zone (32–34 PSU) is included in the storm-influenced group.
- **Kriging:** Ordinary kriging, log1p transform, spherical variogram, min_range = island_diagonal/5, 250 m grid, 3 km coastal band.
- **Island palette:** O'ahu = #1565C0, Maui = #2E7D32, Moloka'i = #E65100, Lāna'i = #6A1B9A
