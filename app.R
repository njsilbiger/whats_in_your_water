# ============================================================
# What's In Your Water? — Interactive Sample Map Dashboard
#
# Public-facing Shiny app showing ocean water sample collection
# locations across Hawaiʻi.
#
# Author: Nyssa Silbiger
# Date:   2026-03-28
# ============================================================

library(shiny)
library(bslib)
library(tidyverse) 
library(googlesheets4)
library(lubridate)
library(leaflet)
library(fontawesome)
library(ggforce)
library(patchwork)
library(ggridges)


# ------------------------------------------------------------------
# Load and prepare data
# Data is loaded fresh from Google Sheets each time the app starts.
# For deployment, ensure gs4_auth() credentials are available on the
# server (e.g., via gargle's encrypted token or environment variables).
# ------------------------------------------------------------------

source("01_load_clean_data.R")

# Cesspool data (pre-computed by 02_download_cesspool_layers.R)
cesspool_dist     <- read_csv("data/sample_cesspool_distances.csv",
                               show_col_types = FALSE)
cesspool_by_ahupuaa <- read_csv("data/cesspool_by_ahupuaa.csv",
                                 show_col_types = FALSE)

# Normalizes island name strings for joining across datasets with
# inconsistent okina/macron encoding
normalize_island <- function(x) {
  x |>
    stringr::str_replace_all("[\u02BB\u2018\u2019']", "") |>
    stringr::str_replace_all("\u0101", "a") |>
    stringr::str_to_lower() |>
    stringr::str_trim()
}

# Read chemistry results and full join with sample metadata
chem_data <- read_csv("data/ChemData.csv", show_col_types = FALSE) |>
  rename(
    sample_id = sample_ID,
    NO3NO2    = `NO3+NO2 (umol/L)`,
    PO4       = `PO4 (umol/L)`,
    SiO2      = `SiO2 (umol/L)`,
    NH3       = `NH3 (umol/L)`
  ) |>
  mutate(
    sample_id = toupper(sample_id),
    across(c(Salinity, NO3NO2, PO4, SiO2, NH3),
           ~if (is.character(.)) readr::parse_number(.) else .)
  )

df_app <- df_clean |>
  mutate(
    hour        = hour(collected_hst),
    minute      = minute(collected_hst),
    # Classify samples: 6:00 AM–6:45 PM = Daytime, otherwise Nighttime
    time_of_day = if_else(
      (hour * 60 + minute) >= (6 * 60) & (hour * 60 + minute) < (18 * 60 + 45),
      "Daytime", "Nighttime"
    ),
    date = as.Date(collected_hst, tz = "Pacific/Honolulu")
  ) |>
  full_join(chem_data, by = "sample_id") |>
  left_join(cesspool_dist, by = "sample_id")

island_choices   <- sort(unique(df_app$island))
tod_choices      <- c("Daytime", "Nighttime")

# Named vector: display label -> ISO date string used for filtering
date_vals    <- sort(unique(df_app$date))
date_choices <- setNames(as.character(date_vals), format(date_vals, "%b %d, %Y"))
total_samples    <- nrow(df_app)

marker_color <- "#f4a261"

# Reef health thresholds (µmol/L) for Hawaiian nearshore waters
thresholds <- c(NO3NO2 = 1, PO4 = 0.1, SiO2 = 5, NH3 = 1)

# Fixed island color palette (matched to sorted island names at startup)
island_palette <- c("#1565C0", "#2E7D32", "#E65100", "#6A1B9A")


# ------------------------------------------------------------------
# Helper: build a timeline step
# ------------------------------------------------------------------

make_timeline_step <- function(icon_name, title, badge_label, step_type, desc) {
  circle_class <- switch(step_type,
    "active" = "step-circle-active",
    "soon"   = "step-circle-soon",
    "future" = "step-circle-future"
  )
  badge_class <- switch(step_type,
    "active" = "bg-primary",
    "soon"   = "bg-warning text-dark",
    "future" = "bg-secondary"
  )
  tags$div(
    class = "timeline-step",
    tags$div(
      class = paste("step-icon-circle", circle_class),
      HTML(fa(icon_name, fill = "white", height = "1.4em"))
    ),
    tags$div(class = "step-title", title),
    tags$span(class = paste("badge step-badge", badge_class), badge_label),
    tags$div(class = "step-desc", desc)
  )
}


# ------------------------------------------------------------------
# Helper: build a parameter explanation card
# ------------------------------------------------------------------

make_param_card <- function(icon_name, param_name, definition,
                            coral_text, runoff_text) {
  card(
    height = "100%",
    card_header(
      tags$span(
        class = "d-flex align-items-center gap-2",
        HTML(fa(icon_name, fill = "#0077b6", height = "1.1em")),
        tags$strong(param_name)
      )
    ),
    card_body(
      tags$p(definition),
      tags$p(tags$strong("Coral reef health: "), coral_text),
      tags$p(tags$strong("Terrestrial runoff signal: "), runoff_text)
    )
  )
}


# ------------------------------------------------------------------
# Helper: speedometer gauge for one nutrient (used on Results tab)
# ------------------------------------------------------------------

make_gauge <- function(pct, nutrient_name, threshold_label,
                       units = "\u00b5mol/L") {
  grade_color <- case_when(
    pct < 25 ~ "#43A047",
    pct < 50 ~ "#FB8C00",
    TRUE     ~ "#E53935"
  )
  fill_end <- (pct / 100) * pi

  ggplot() +
    geom_arc_bar(
      data = tibble(x0=0, y0=0, r0=0.58, r=1, start=0, end=pi),
      aes(x0=x0, y0=y0, r0=r0, r=r, start=start, end=end),
      fill = "grey88", color = NA
    ) +
    geom_arc_bar(
      data = tibble(x0=0, y0=0, r0=0.58, r=1, start=0, end=fill_end),
      aes(x0=x0, y0=y0, r0=r0, r=r, start=start, end=end),
      fill = grade_color, color = NA
    ) +
    annotate("segment",
             x = 0, xend = 0, y = 0.58, yend = 1.08,
             color = "white", linewidth = 0.8) +
    annotate("text", x = 0, y = 0.22,
             label = paste0(round(pct), "%"),
             size = 9, fontface = "bold", color = grade_color, hjust = 0.5) +
   # annotate("text", x = 0, y = -0.08,
    #         label = "of samples\nexceeded threshold",
     #        size = 3, color = "grey45", hjust = 0.5, lineheight = 1.1) +
    annotate("text", x =  1.2, y = -0.02, label = "0%",
             size = 3, color = "grey55", hjust = 0.5) +
    annotate("text", x = -1.2, y = -0.02, label = "100%",
             size = 3, color = "grey55", hjust = 0.5) +
    coord_cartesian(xlim = c(-1.6, 1.6), ylim = c(-0.45, 1.25)) +
    ggtitle(
      nutrient_name,
      subtitle = paste0("threshold: >", threshold_label, " ", units)
    ) +
    theme_void() +
    theme(
      # Keep arcs circular: panel height / panel width = data height / data width
      aspect.ratio  = 1.7 / 3.2,
      plot.title    = element_text(hjust=0.5, face="bold", size=13,
                                   color="grey15", margin=margin(b=1)),
      plot.subtitle = element_text(hjust=0.5, size=9,
                                   color="grey55", margin=margin(b=2)),
      plot.margin   = margin(8, 8, 8, 8)
    )
}


# ------------------------------------------------------------------
# Helper: build a press / media card
# ------------------------------------------------------------------

make_press_card <- function(outlet, date_str, headline, excerpt, url,
                            img_url = NULL, img_alt = "") {
  img_top <- if (!is.null(img_url)) {
    tags$a(
      href = url, target = "_blank",
      tags$img(src = img_url, alt = img_alt, class = "press-card-img")
    )
  } else {
    tags$div(
      class = "press-img-placeholder",
      HTML(fa("newspaper", fill = "white", height = "2.5em"))
    )
  }

  card(
    height = "100%",
    img_top,
    card_body(
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$span(class = "badge bg-secondary", outlet),
        tags$small(class = "text-muted", date_str)
      ),
      tags$p(class = "fw-semibold mb-2 lh-sm", headline),
      tags$p(class = "small text-muted mb-3", excerpt),
      tags$a(
        href   = url,
        target = "_blank",
        class  = "btn btn-sm btn-outline-primary",
        HTML(fa("arrow-up-right-from-square", height = "0.85em")),
        " Read article"
      )
    )
  )
}


# ------------------------------------------------------------------
# CSS
# ------------------------------------------------------------------

app_css <- "
  /* ---- Value boxes ---- */
  .value-box-value { font-size: 1rem !important; }
  .value-box-title { font-size: 0.75rem !important; }

  /* ---- Timeline ---- */
  .timeline-track {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    position: relative;
    margin: 10px 0 30px 0;
  }
  .timeline-track::before {
    content: '';
    position: absolute;
    top: 28px;
    left: calc(10% + 5px);
    right: calc(10% + 5px);
    height: 3px;
    background: linear-gradient(to right, #0077b6 30%, #f4a261 60%, #adb5bd);
    z-index: 0;
  }
  .timeline-step {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    position: relative;
    z-index: 1;
  }
  .step-icon-circle {
    width: 58px;
    height: 58px;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    margin-bottom: 10px;
    border: 3px solid #fff;
    box-shadow: 0 2px 8px rgba(0,0,0,0.18);
  }
  .step-circle-active { background-color: #0077b6; }
  .step-circle-soon   { background-color: #f4a261; }
  .step-circle-future { background-color: #adb5bd; }
  .step-title {
    font-weight: 600;
    font-size: 0.85rem;
    text-align: center;
    margin-bottom: 5px;
    min-height: 2.4em;
    line-height: 1.2;
  }
  .step-badge { margin-bottom: 6px; font-size: 0.7rem; }
  .step-desc {
    font-size: 0.73rem;
    color: #6c757d;
    text-align: center;
    max-width: 120px;
    line-height: 1.35;
  }

  /* ---- Progress indicator ---- */
  .progress-section {
    background: #f8f9fa;
    border-radius: 10px;
    padding: 18px 22px;
    margin: 16px 0 20px 0;
  }
  .progress-label {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin-bottom: 8px;
  }
  .progress-label .big-count {
    font-size: 1.6rem;
    font-weight: 700;
    color: #0077b6;
    line-height: 1;
  }
  .progress-label .total-count {
    font-size: 0.9rem;
    color: #6c757d;
  }
  .progress { height: 14px; border-radius: 7px; }
  .progress-bar { border-radius: 7px; transition: width 0.6s ease; }

  /* ---- Funding logos ---- */
  .funding-section {
    margin-top: auto;
    padding-top: 14px;
  }
  .funding-label {
    font-size: 0.7rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: #6c757d;
    margin-bottom: 10px;
  }
  .logo-grid {
    display: flex;
    flex-direction: column;
    gap: 12px;
    align-items: flex-start;
  }
  .logo-grid img {
    max-height: 44px;
    max-width: 100%;
    width: auto;
    object-fit: contain;
    filter: none;
  }

  /* ---- Press cards ---- */
  .press-card-img {
    width: 100%;
    height: 180px;
    object-fit: cover;
    border-radius: calc(var(--bs-card-inner-border-radius))
                   calc(var(--bs-card-inner-border-radius)) 0 0;
    display: block;
  }
  .press-img-placeholder {
    height: 180px;
    background: linear-gradient(135deg, #0077b6 0%, #023e8a 100%);
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: calc(var(--bs-card-inner-border-radius))
                   calc(var(--bs-card-inner-border-radius)) 0 0;
  }

  /* ---- Leaflet legend: always visible inside map card ---- */
  #map-card .card-body {
    overflow: visible !important;
  }
  .leaflet-control-container {
    overflow: visible !important;
  }

  /* ---- Leaflet legend: vertically centered on the right ---- */
  #map .leaflet-top.leaflet-right {
    top: 50% !important;
    transform: translateY(-50%) !important;
  }

  /* ---- Parameter section heading ---- */
  .param-section-title {
    font-size: 1.25rem;
    font-weight: 700;
    color: #0077b6;
    border-bottom: 2px solid #0077b6;
    padding-bottom: 8px;
    margin: 28px 0 16px 0;
  }

  /* ================================================================
     MOBILE  (≤ 767 px)
     Desktop layout is unchanged above. These rules replace the old
     zoom: 0.5 hack with proper responsive layout so text is readable
     and all tap targets meet minimum size.
     ================================================================ */

  @media (max-width: 767px) {

    /* ---- Navbar brand: wrap gracefully rather than overflow ---- */
    .navbar-brand {
      font-size: 0.78rem;
      white-space: normal;
      line-height: 1.25;
      max-width: 52vw;
    }

    /* ---- Nav tabs: horizontally scrollable row, no wrapping ---- */
    .navbar-nav {
      flex-wrap: nowrap;
      overflow-x: auto;
      -webkit-overflow-scrolling: touch;
      scrollbar-width: none;
      padding-bottom: 2px;
    }
    .navbar-nav::-webkit-scrollbar { display: none; }
    .navbar-nav .nav-link {
      font-size: 0.8rem;
      padding: 0.4rem 0.55rem;
      white-space: nowrap;
    }

    /* ---- Sidebar layout: override fillable so content scrolls ----
       fillable = TRUE locks overflow:hidden, which collapses the map
       to zero height on small screens. */
    .bslib-sidebar-layout {
      height: auto !important;
      overflow: visible !important;
    }
    .bslib-sidebar-layout > .main {
      height: auto !important;
      overflow-y: auto !important;
      padding: 6px !important;
    }

    /* ---- Sidebar toggle button: easy to tap ---- */
    .bslib-sidebar-layout > .collapse-toggle {
      min-width: 44px;
      min-height: 44px;
      display: flex;
      align-items: center;
      justify-content: center;
    }

    /* ---- Sidebar panel: full-width drawer when open ---- */
    .bslib-sidebar-layout .bslib-sidebar {
      max-width: 100% !important;
    }

    /* ---- Map card and leaflet output ----
       Overhead: navbar ~56px, 2 rows of value boxes ~168px,
       card header ~44px, padding ~16px = ~284px total. */
    #map-card { height: auto !important; }
    #map {
      height: calc(100svh - 56px - 168px - 44px - 20px) !important;
      min-height: 260px;
    }

    /* ---- Value boxes: compact 2-per-row grid ---- */
    .bslib-value-box { height: 72px !important; min-width: 130px; }
    .value-box-title { font-size: 0.62rem !important; }
    .value-box-value { font-size: 0.9rem !important; }

    /* ---- Main content column: fill the width ---- */
    .bslib-main-col { padding-left: 6px !important; padding-right: 6px !important; }

    /* ---- Buttons: comfortable tap targets ---- */
    .btn     { min-height: 40px; }
    .btn-sm  { min-height: 34px; font-size: 0.82rem; padding: 0.35rem 0.65rem; }

    /* ---- Accordion: easier to tap open/close ---- */
    .accordion-button { padding: 0.75rem 1rem; font-size: 0.9rem; }

    /* ---- Leaflet popups: readable text, easy-to-hit close button ---- */
    .leaflet-popup-content {
      font-size: 0.9rem !important;
      line-height: 1.55 !important;
      min-width: 220px;
    }
    .leaflet-popup-close-button {
      font-size: 1.4rem !important;
      padding: 6px 10px !important;
    }

    /* ---- Timeline: stack vertically ---- */
    .timeline-track {
      flex-direction: column;
      align-items: center;
      gap: 24px;
    }
    .timeline-track::before {
      top: 0;
      bottom: 0;
      left: 28px;
      right: auto;
      width: 3px;
      height: auto;
      background: linear-gradient(to bottom, #0077b6 30%, #f4a261 60%, #adb5bd);
    }
    .timeline-step {
      flex-direction: row;
      align-items: flex-start;
      gap: 16px;
      width: 100%;
    }
    .step-icon-circle { margin-bottom: 0; flex-shrink: 0; }
    .step-title { text-align: left; min-height: unset; }
    .step-desc  { text-align: left; max-width: none; }

    /* ---- Progress section: tighter padding ---- */
    .progress-section { padding: 12px 14px; }
    .progress-label .big-count { font-size: 1.3rem; }

    /* ---- Funding logos: wrap and center on narrow screens ---- */
    .funding-logos {
      flex-wrap: wrap !important;
      justify-content: center !important;
      gap: 10px !important;
    }
    .funding-logos img,
    .funding-logos a > img { max-height: 30px !important; }

    /* ---- Content pages: tighter container padding ---- */
    .container-lg {
      padding-left: 12px !important;
      padding-right: 12px !important;
    }

    /* ---- Section headings: slightly smaller on mobile ---- */
    .param-section-title { font-size: 1.05rem; margin: 20px 0 12px 0; }

    /* ---- Input labels and small text: stay legible ---- */
    .form-check-label { font-size: 0.88rem; }
    .small, small     { font-size: 0.82rem !important; }
  }
"


# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------

funding_banner <- tags$div(
  style = "background: #212529; padding: 14px 24px; margin-top: 32px;",
  div(class = "funding-label", "Funding generously provided by"),
  div(
    class = "funding-logos",
    style = "background: #ffffff; border-radius: 8px; padding: 12px 18px; display: flex; flex-wrap: wrap; gap: 16px; align-items: center;",
    tags$a(
      href = "https://www.soest.hawaii.edu", target = "_blank",
      tags$img(
        src   = "https://www.soest.hawaii.edu/soest_web/logos/soest_logo_textline_307C_white_1000px.jpg",
        alt   = "School of Ocean and Earth Science and Technology (SOEST)",
        title = "School of Ocean and Earth Science and Technology (SOEST)",
        style = "max-height: 40px; width: auto; max-width: 100%;")
    ),
    tags$a(
      href = "https://seagrant.soest.hawaii.edu", target = "_blank",
      tags$img(
        src   = "logo_seagrant.png",
        alt   = "University of Hawai\u02BBi Sea Grant College Program",
        title = "University of Hawai\u02BBi Sea Grant College Program",
        style = "max-height: 40px; width: auto; max-width: 100%;")
    ),
    tags$a(
      href = "https://manoa.hawaii.edu", target = "_blank",
      tags$img(
        src   = "logo_uhmanoa.png",
        alt   = "University of Hawai\u02BBi at M\u0101noa",
        title = "University of Hawai\u02BBi at M\u0101noa",
        style = "max-height: 28px; width: auto; max-width: 100%;")
    )
  )
)

# ---- Social / OG meta tags ------------------------------------------------
# Replace APP_URL with your deployed app URL (e.g. https://yourname.shinyapps.io/whats_in_your_water)
app_url   <- "APP_URL"
app_image <- paste0(app_url, "/map_preview.png")

social_tags <- tags$head(
  # Standard meta
  tags$meta(name = "description",
            content = paste(
              "Community science water quality monitoring around O\u02BBahu and Maui Nui",
              "after the March 2026 Kona Low storm.",
              "Explore sample locations, collection details, and salinity data."
            )),

  # Open Graph (Facebook, LinkedIn, iMessage, Slack, etc.)
  tags$meta(property = "og:type",        content = "website"),
  tags$meta(property = "og:url",         content = app_url),
  tags$meta(property = "og:title",
            content = "What\u2019s In Your Water? \u2014 Hawai\u02BBi Ocean Sampling"),
  tags$meta(property = "og:description",
            content = paste(
              "Community science water quality monitoring around O\u02BBahu and Maui Nui",
              "after the March 2026 Kona Low storm."
            )),
  tags$meta(property = "og:image",       content = app_image),
  tags$meta(property = "og:image:alt",
            content = "Researchers collecting ocean water samples in Hawai\u02BBi"),
  tags$meta(property = "og:locale",      content = "en_US"),

  # Twitter / X card
  tags$meta(name = "twitter:card",        content = "summary_large_image"),
  tags$meta(name = "twitter:title",
            content = "What\u2019s In Your Water? \u2014 Hawai\u02BBi Ocean Sampling"),
  tags$meta(name = "twitter:description",
            content = paste(
              "Community science water quality monitoring around O\u02BBahu and Maui Nui",
              "after the March 2026 Kona Low storm."
            )),
  tags$meta(name = "twitter:image",       content = app_image),
  tags$meta(name = "twitter:image:alt",
            content = "Researchers collecting ocean water samples in Hawai\u02BBi")
)

ui <- tagList(
  social_tags,
  page_navbar(
  title  = "What's In Your Water? — Hawaiʻi Ocean Sampling",
  theme  = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0077b6"
  ),

  tags$head(
    tags$meta(
      name    = "description",
      content = paste(
        "What's In Your Water? is a community science project collecting",
        "ocean water samples across O\u02BBahu and Maui Nui to assess coastal",
        "water quality after the March 2026 Kona Low storm. Explore",
        "sample locations and follow along as lab results come in."
      )
    ),
    tags$meta(property = "og:title",
              content  = "What\u2019s In Your Water? \u2014 Hawai\u02BBi Ocean Sampling"),
    tags$meta(property = "og:description",
              content  = paste(
                "Community science tracking coastal water quality across",
                "O\u02BBahu and Maui Nui after the March 2026 Kona Low storm."
              )),
    tags$meta(property = "og:type", content = "website")
  ),

  tags$style(HTML(app_css)),

  # ================================================================
  # Page 1: Data
  # ================================================================

  nav_panel(
    title = "Data",
    value = "data-tab",

    layout_sidebar(
      fillable = TRUE,

      # ---- Sidebar -----------------------------------------------
      sidebar = sidebar(
        width  = 310,
        open   = list(desktop = "open", mobile = "closed"),

        p(
          strong("What\u2019s In Your Water?"), "is a community science project",
          "collecting ocean water samples across O\u02BBahu and Maui Nui to",
          "assess coastal water quality after the March 2026 Kona Low storm.",
          "Samples are gathered by volunteer community members and scientists.",
          "We are in the process of analyzing these samples for salinity, nutrients,",
          "and dissolved organic matter \u2014 important indicators of coral reef health."
        ),
        p(
          "Use the filters below to explore samples by mokupuni and time of",
          "collection. Click any marker on the map for collection details."
        ),
        tags$div(
          class = "d-flex flex-column gap-1 mb-2",
          tags$small(
            tags$a(
              href    = "javascript:void(0);",
              onclick = paste0(
                "document.querySelectorAll('.nav-link').forEach(",
                "function(el){",
                "  if(el.textContent.trim()==='What are we measuring?') el.click();",
                "});"
              ),
              HTML(fa("circle-arrow-right", height = "0.85em")),
              " What are we measuring?"
            )
          ),
          tags$small(
            tags$a(
              href    = "javascript:void(0);",
              onclick = paste0(
                "document.querySelectorAll('.nav-link').forEach(",
                "function(el){",
                "  if(el.textContent.trim()==='Press') el.click();",
                "});"
              ),
              HTML(fa("newspaper", height = "0.85em")),
              " Press coverage"
            )
          ),
          tags$small(
            tags$a(
              href    = "javascript:void(0);",
              onclick = paste0(
                "document.querySelectorAll('.nav-link').forEach(",
                "function(el){",
                "  if(el.textContent.trim()==='About Us') el.click();",
                "});"
              ),
              HTML(fa("users", height = "0.85em")),
              " About the scientists"
            )
          )
        ),

        accordion(
          open = c("Map Display", "Filter Samples"),

          accordion_panel(
            title = "Map Display",
            icon  = HTML(fa("map", height = "0.9em")),
            radioButtons(
              inputId  = "color_by",
              label    = "Color Points By",
              choices  = c(
                "Default"                         = "default",
                "Salinity (psu)"                  = "salinity",
                "Nitrate+Nitrite (\u00b5mol/L)"   = "nitrate",
                "Phosphate (\u00b5mol/L)"         = "phosphate",
                "Silicate (\u00b5mol/L)"          = "silicate",
                "Ammonium (\u00b5mol/L)"          = "ammonium"
              ),
              selected = "default"
            ),
            # tags$hr(class = "my-2"),
            # checkboxInput(
            #   inputId = "show_cesspools",
            #   label   = tagList(
            #     HTML(fa("triangle-exclamation", height = "0.85em", fill = "#c0392b")),
            #     " Show Cesspool Priority Zones"
            #   ),
            #   value   = FALSE
            # ),
            # conditionalPanel(
            #   condition = "input.show_cesspools",
            #   tags$div(
            #     class = "small text-muted mt-1",
            #     tags$span(
            #       style = "background:red; display:inline-block; width:10px; height:10px; border-radius:2px; margin-right:4px;"
            #     ), "High",
            #     tags$span(
            #       style = "background:orange; display:inline-block; width:10px; height:10px; border-radius:2px; margin: 0 4px 0 8px;"
            #     ), "Medium",
            #     tags$span(
            #       style = "background:#FFD700; display:inline-block; width:10px; height:10px; border-radius:2px; margin: 0 4px 0 8px;"
            #     ), "Low"
            #   ),
            #   tags$p(
            #     class = "small text-muted mt-1 mb-0",
            #     "Cesspool priority zones from the ",
            #     tags$a(
            #       href   = "https://seagrant.soest.hawaii.edu/cesspools-tool/",
            #       target = "_blank",
            #       "Hawaiʻi Cesspool Prioritization Tool (HCPT)"
            #     ), "."
            #   )
            # )
          ),

          accordion_panel(
            title = "Filter Samples",
            icon  = HTML(fa("filter", height = "0.9em")),
            checkboxGroupInput(
              inputId  = "island",
              label    = "Mokupuni (Island)",
              choices  = island_choices,
              selected = island_choices
            ),
            selectizeInput(
              inputId  = "moku_filter",
              label    = "Moku",
              choices  = c("All moku" = ""),
              selected = "",
              multiple = FALSE,
              options  = list(placeholder = "All moku")
            ),
            selectizeInput(
              inputId  = "ahupuaa_filter",
              label    = "Ahupua\u02BBa",
              choices  = c("All ahupua\u02BBa" = ""),
              selected = "",
              multiple = FALSE,
              options  = list(placeholder = "All ahupua\u02BBa")
            ),
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-1 mt-2",
              tags$label("Sampling Date", class = "fw-semibold mb-0"),
              tags$span(
                actionLink("date_select_all",   "All",  class = "small"),
                " / ",
                actionLink("date_deselect_all", "None", class = "small")
              )
            ),
            checkboxGroupInput(
              inputId  = "sampling_date",
              label    = NULL,
              choices  = date_choices,
              selected = date_choices
            ),
            actionButton(
              inputId = "reset_filters",
              label   = tagList(HTML(fa("rotate-left", height = "0.85em")), " Reset All Filters"),
              class   = "btn-sm btn-outline-secondary w-100 mt-2"
            )
          ),

          accordion_panel(
            title = "Search & Export",
            icon  = HTML(fa("magnifying-glass", height = "0.9em")),
            tags$label("Find Sample by ID", class = "fw-semibold mb-1 d-block"),
            textInput(
              "sample_id_search",
              label       = NULL,
              placeholder = "e.g. OA324",
              width       = "100%"
            ),
            tags$div(
              class = "d-flex gap-2 mb-2",
              actionButton(
                "search_sample_btn", "Search",
                class = "btn-sm btn-primary flex-fill"
              ),
              actionButton(
                "clear_sample_btn", "Clear",
                class = "btn-sm btn-outline-secondary"
              )
            ),
            uiOutput("sample_search_result"),
            downloadButton(
              "download_data",
              label = "Download Samples (CSV)",
              class = "btn-sm btn-outline-primary w-100 mt-2"
            ),
            tags$div(
              class = "mt-2 p-2 border rounded bg-light",
              style = "font-size: 0.72rem; line-height: 1.4;",
              tags$span(class = "fw-semibold d-block mb-1", "How to cite:"),
              "Silbiger, N., Kealoha, A., & Kahanamoku, S. (2026). ",
              tags$i("What\u2019s In Your Water? \u2014 Hawai\u02BBi Coastal Water Quality Samples, March 2026 Kona Low."),
              " Zenodo. ",
              tags$a(
                href   = "https://doi.org/10.5281/zenodo.19410137",
                target = "_blank",
                class  = "text-muted",
                "DOI: 10.5281/zenodo.19410137"
              )
            ),
            tags$small(
              class = "text-muted mt-1 d-block",
              HTML(fa("rotate", height = "0.8em")),
              " Data last loaded: ",
              textOutput("data_loaded_at_text", inline = TRUE)
            )
          )
        )
      ),

      # ---- Main content ------------------------------------------
      layout_column_wrap(
        width = "140px",
        fill  = FALSE,
        heights_equal = "row",
        uiOutput("samples_box"),
        value_box(
          title = "Mokupuni (Islands)",
          value = textOutput("n_islands"),
          showcase = fa("map-location-dot"),
          theme = "info",
          height = "90px"
        ),
        value_box(
          title = "Collection Date",
          value = textOutput("collection_date"),
          showcase = fa("calendar-days"),
          theme = "secondary",
          height = "90px"
        ),
        value_box(
          title = "Participants",
          value = textOutput("n_samplers"),
          showcase = fa("users"),
          theme = "success",
          height = "90px"
        )
      ),

      card(
        id          = "map-card",
        full_screen = TRUE,
        card_header("Sample Collection Locations"),
        div(
          style = "position: relative;",
          leafletOutput("map", height = "650px"),
          uiOutput("empty_map_overlay")
        )
      ),

      funding_banner
    )
  ),


  # ================================================================
  # Page 2: Results
  # ================================================================

  nav_panel(
    title    = "Results",
    fillable = FALSE,

    div(
      class = "container-lg py-4",

      h3(
        class = "mb-1",
        HTML(fa("chart-bar", fill = "#0077b6", height = "1em")),
        " Water Quality Results"
      ),
      p(
        class = "text-muted mb-4",
        "Summary of chemistry results across all sampled sites.",
        "This panel updates automatically as new lab data arrives."
      ),

      uiOutput("results_no_data_msg"),

      # ---- Report card gauges ----------------------------------------
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("gauge-high", fill = "#0077b6", height = "1em")),
            tags$strong("Reef Health Report Card")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Percentage of water samples exceeding reef health thresholds",
            "for Hawaiian nearshore waters."
          ),
          plotOutput("plot_report_card", height = "420px")
        )
      ),

      # ---- Strip plot (full width) -----------------------------------
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("circle-dot", fill = "#0077b6", height = "1em")),
            tags$strong("Nutrient Concentrations \u2014 All Samples")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Each dot is one water sample. The dashed red line marks the reef health threshold.",
            "Dots to the right of the line exceeded the threshold."
          ),
          layout_column_wrap(
            width = "50%",
            heights_equal = "row",
            plotOutput("plot_strip_no3no2", height = "200px"),
            plotOutput("plot_strip_po4",    height = "200px"),
            plotOutput("plot_strip_sio2",   height = "200px"),
            plotOutput("plot_strip_nh3",    height = "200px")
          )
        )
      ),

      # ---- Salinity ridgeline (full width) ---------------------------
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("water", fill = "#0077b6", height = "1em")),
            tags$strong("Salinity Distribution by Island")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Each curve shows the full spread of salinity readings across samples from that island.",
            "The dotted green line marks normal seawater (34\u00a0psu) \u2014",
            "values to the left indicate freshwater mixing from storm runoff."
          ),
          plotOutput("plot_ridgeline", height = "320px")
        )
      ),

      # ---- Pollution burden by island (full width) ---------------
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("chart-bar", fill = "#0077b6", height = "1em")),
            tags$strong("Pollution Burden by Island")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Share of analyzed samples by how many nutrient thresholds were simultaneously exceeded.",
            "Darker colors indicate more nutrients above threshold at the same site."
          ),
          plotOutput("plot_burden", height = "280px")
        )
      ),

      # ---- Stoplight heatmap (full width) ---------------------------
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("table", fill = "#0077b6", height = "1em")),
            tags$strong("Water Quality Stoplight by Moku")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Percentage of samples exceeding reef health thresholds for each nutrient,",
            "grouped by traditional Hawaiian land division (moku).",
            "Moku with fewer than 3 analyzed samples are excluded."
          ),
          plotOutput("plot_stoplight", height = "500px")
        )
      ),

      # ---- Ranked ahupuaʻa hotspot (full width) ------------------
      # card(
      #   class = "mb-4",
      #   card_header(
      #     tags$span(
      #       class = "d-flex align-items-center gap-2",
      #       HTML(fa("circle-exclamation", fill = "#0077b6", height = "1em")),
      #       tags$strong("Top Ahupua\u02BBa by Pollution Burden")
      #     )
      #   ),
      #   card_body(
      #     tags$p(
      #       class = "text-muted small mb-3",
      #       "Average number of nutrient thresholds exceeded per sample (out of 4).",
      #       "Higher scores indicate broader, more severe pollution.",
      #       "Top 15 ahupua\u02BBa with \u22653 analyzed samples shown."
      #     ),
      #     plotOutput("plot_hotspot", height = "420px")
      #   )
      # ),

      # ---- Cesspool proximity (full width) ---------------------------
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("house-crack", fill = "#0077b6", height = "1em")),
            tags$strong("Nutrient Concentration vs. Distance to Nearest Cesspool")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Each dot is one water sample. X-axis shows distance (m, log scale) to the",
            "nearest cesspool; y-axis shows nutrient concentration (log\u2081p scale).",
            "Lāna\u02BBi is excluded \u2014 no cesspool location data are publicly available",
            "for that island. Open-ocean samples are excluded."
          ),
          plotOutput("plot_cesspool_proximity", height = "700px"),
          tags$p(
            class = "text-muted small mt-2",
            style = "font-size: 0.78rem;",
            tags$strong("Data source: "),
            "Cesspool locations from the Hawaiʻi Statewide GIS Program, ",
            tags$em("Hawaiʻi Cesspool Prioritization Tool (HCPT) Cesspools"),
            " (layer 32) and ",
            tags$em("Onsite Sewage Disposal Systems — Molokaʻi"),
            " (layer 23). ",
            tags$a(
              "geodata.hawaii.gov",
              href   = "https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer",
              target = "_blank"
            ),
            ". Molokaʻi data filtered to Class\u00a0I (cesspool) systems.",
            " Accessed April\u00a028, 2026."
          )
        )
      ),

      # ---- Cesspool count vs. mean nutrient concentration (full width) ---
      # card(
      #   class = "mb-4",
      #   card_header(
      #     tags$span(
      #       class = "d-flex align-items-center gap-2",
      #       HTML(fa("chart-line", fill = "#0077b6", height = "1em")),
      #       tags$strong("Mean Nutrient Concentration by Cesspool Density")
      #     )
      #   ),
      #   card_body(
      #     tags$p(
      #       class = "text-muted small mb-3",
      #       "Each dot is one ahupua\u02BBa (traditional land division), sized by number of",
      #       "water samples. X-axis shows the total number of cesspools within the",
      #       "ahupua\u02BBa (log scale); y-axis shows the mean nutrient concentration",
      #       "(log scale, labeled on normal scale). Error bars show \u00b11 SE.",
      #       "Only ahupua\u02BBa with \u22653 analyzed samples are shown.",
      #       "L\u0101na\u02BBi excluded \u2014 no cesspool location data are publicly available."
      #     ),
      #     plotOutput("plot_cesspool_burden", height = "750px"),
      #     tags$p(
      #       class = "text-muted small mt-2",
      #       style = "font-size: 0.78rem;",
      #       tags$strong("Data source: "),
      #       "Cesspool locations from the Hawaiʻi Statewide GIS Program, ",
      #       tags$em("Hawaiʻi Cesspool Prioritization Tool (HCPT) Cesspools"),
      #       " (layer 32) and ",
      #       tags$em("Onsite Sewage Disposal Systems — Molokaʻi"),
      #       " (layer 23). ",
      #       tags$a(
      #         "geodata.hawaii.gov",
      #         href   = "https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer",
      #         target = "_blank"
      #       ),
      #       ". Molokaʻi data filtered to Class\u00a0I (cesspool) systems.",
      #       " Accessed April\u00a028, 2026."
      #     )
      #   )
      # ),

      # ---- Cesspool density vs. mean nutrient concentration (full width) -
      card(
        class = "mb-4",
        card_header(
          tags$span(
            class = "d-flex align-items-center gap-2",
            HTML(fa("chart-line", fill = "#0077b6", height = "1em")),
            tags$strong("Mean Nutrient Concentration by Cesspool Density (per km\u00b2)")
          )
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Each dot is one ahupua\u02BBa, sized by number of water samples.",
            "X-axis shows cesspool density (cesspools per km\u00b2, log scale);",
            "y-axis shows mean nutrient concentration (log scale, labeled on normal scale).",
            "Error bars show \u00b11 SE.",
            "Only ahupua\u02BBa with \u22653 analyzed samples are shown.",
            "L\u0101na\u02BBi excluded \u2014 no cesspool location data are publicly available."
          ),
          plotOutput("plot_cesspool_density", height = "750px"),
          tags$p(
            class = "text-muted small mt-2",
            style = "font-size: 0.78rem;",
            tags$strong("Data source: "),
            "Cesspool locations from the Hawaiʻi Statewide GIS Program, ",
            tags$em("Hawaiʻi Cesspool Prioritization Tool (HCPT) Cesspools"),
            " (layer 32) and ",
            tags$em("Onsite Sewage Disposal Systems — Molokaʻi"),
            " (layer 23). ",
            tags$a(
              "geodata.hawaii.gov",
              href   = "https://geodata.hawaii.gov/arcgis/rest/services/Infrastructure/MapServer",
              target = "_blank"
            ),
            ". Molokaʻi data filtered to Class\u00a0I (cesspool) systems.",
            " Accessed April\u00a028, 2026."
          )
        )
      ),

      funding_banner
    )
  ),


  # ================================================================
  # Page 3: What are we measuring?
  # ================================================================

  nav_panel(
    title = "What are we measuring?",
    fillable = FALSE,

    div(
      class = "container-lg py-4",

      # ---- Section: Analysis pipeline ----------------------------
      h3(
        class = "mb-1",
        HTML(fa("flask", fill = "#0077b6", height = "1em")),
        " Sample Analysis Pipeline"
      ),
      p(
        class = "text-muted mb-4",
        "Every sample collected gets sent to a lab to figure out what's in the water.",
        "Here's how the process works — and what we're looking for."
      ),

      # ---- Timeline ----------------------------------------------
      div(
        class = "timeline-track",
        make_timeline_step(
          "snowflake", "Defrost Samples",
          "In Progress", "active",
          "Frozen samples are thawed at 4°C before analysis begins."
        ),
        make_timeline_step(
          "droplet", "Salinity",
          "Batches Arriving", "soon",
          "First batches of ~50 processed soon. Results arriving soon."
        ),
        make_timeline_step(
          "flask", "Nutrients",
          "Batches Arriving", "soon",
          "Nitrate, phosphate & silicate."
        ),
        make_timeline_step(
          "sun", "Organic Matter",
          "Coming Soon", "future",
          "Fluorescent dissolved organic matter (fDOM) via fluorometry."
        ),
      ),

      # ---- Progress indicator ------------------------------------
      uiOutput("progress_section"),

      # ---- Info banner -------------------------------------------
      div(
        class = "alert alert-info d-flex align-items-start gap-3 mt-2",
        HTML(fa("circle-info", fill = "#0c5460", height = "1.2em")),
        tags$div(
          tags$strong("A note on timelines: "),
          "We received an overwhelming number of samples — thank you! ",
          "Samples are being processed in batches of ~50 and sent to ",
          "multiple specialized labs. Salinity and nutrient results will come ",
          "in first, followed by dissolved organic matter (fDOM). ",
          "We'll update the data map as results arrive."
        )
      ),

      # ---- Section: Parameter descriptions -----------------------
      div(class = "param-section-title", "What are these parameters?"),

      p(
        class = "mb-4",
        "Each thing we measure gives us a different clue about how the March 2026",
        "Kona Low storm affected the ocean near shore — and what that could mean",
        "for the coral reefs and the communities that depend on them."
      ),

      layout_column_wrap(
        width = "320px",
        heights_equal = "row",

        make_param_card(
          "droplet", "Salinity",
          "Salinity tells us how salty the water is. Marine life — corals,
           fish, invertebrates — evolved in seawater with a very consistent
           salt concentration. A big storm disrupts that by flooding the
           nearshore ocean with fresh rainwater.",
          "Corals are built for salty ocean water. When a storm washes a lot of
           fresh water off the land, it dilutes the ocean near shore. That sudden 
           change shocks the corals and can cause
           bleaching or even death, even if the water temperature is totally normal.",
          "A big drop in salinity close to shore after a storm is one of the fastest
           signs that runoff from the land has reached the reef. It's often the
           first warning signal we look for."
        ),

        make_param_card(
          "flask", "Nitrate (NO\u2083\u207B)",
          "Nitrate is a form of nitrogen — the same ingredient found in lawn and
           garden fertilizers. Plants and algae need nitrogen to grow, but when
           too much washes into the ocean, it causes serious problems.",
          "Too much nitrate is like over-fertilizing a garden — except instead of
           your tomatoes growing huge, it's algae that goes wild. Fast-growing
           algae smothers corals, blocks their sunlight, and takes over the reef.
           This a major threat to coral reefs in Hawaiʻi.",
          "Nitrate is a telltale sign of fertilizer runoff from farms, golf
           courses, and lawns, as well as leaks from septic systems or cesspools.
           A spike after heavy rain means those nutrients washed off the land
           and into the ocean."
        ),

        make_param_card(
          "flask", "Phosphate (PO\u2084\u00B3\u207B)",
          "Phosphate is another nutrient that plants and algae need to grow —
           similar to the phosphorus in fertilizers. On its own, a small amount
           is normal. Too much causes the same kind of problems as nitrate.",
          "Phosphate teams up with nitrate to cause algae blooms on reefs. It
           can also make it harder for corals to build their hard skeletons,
           which is how they grow and provide shelter for fish and other marine
           life.",
          "Phosphate washes off fertilized land, and also comes from soaps,
           detergents, and sewage. Finding high phosphate alongside high nitrate
           after a storm is a strong clue that a mix of agricultural and urban
           runoff reached the reef."
        ),

        make_param_card(
          "flask", "Silicate (Si(OH)\u2084)",
          "Silicate is a mineral dissolved in water — it's actually related to
           what glass is made of. It comes from the natural breakdown of rocks
           and soil. Hawaiʻi's young volcanic rock breaks down easily, so our
           streams naturally carry quite a bit of silicate.",
          "Silicate feeds tiny algae called diatoms. When a flood of silicate
           hits the reef, diatoms can bloom in huge numbers. When they eventually
           die and decompose, they use up oxygen in the water — making it harder
           for reef fish, corals, and other animals to breathe.",
          "Because silicate comes straight from eroding rock and soil, a spike
           in silicate after rain is a strong sign that muddy, sediment-heavy
           water poured off the land. It acts like a fingerprint of storm runoff."
        ),

        make_param_card(
          "flask", "Ammonium (NH\u2084\u207A)",
          "Ammonium is a form of nitrogen produced when organic matter — like
           leaves, soil, animal waste, or sewage — breaks down in water. Unlike
           nitrate, which mostly comes from fertilizers, ammonium is a direct
           sign that decomposing organic material or sewage has entered the water.",
          "Ammonium can directly stress corals and the tiny algae that live
           inside them (zooxanthellae), disrupting the partnership that keeps
           corals fed and healthy. Even at low concentrations, elevated ammonium
           tips the balance toward fast-growing algae that can outcompete corals
           on the reef.",
          "A spike in ammonium after heavy rain is a strong signal that sewage,
           cesspool overflow, or decomposing organic matter washed off the land.
           Finding elevated ammonium alongside nitrate and phosphate paints a
           clear picture of mixed urban and biological runoff reaching the reef."
        ),

        make_param_card(
          "sun", "fDOM (Fluorescent Dissolved Organic Matter — pronounced \"eff-dom\")",
          "fDOM stands for fluorescent dissolved organic matter.
          There are many types of fDOM. Some fDOM components measure the murky, brownish stuff that
           dissolves in water when leaves, soil, and plants decay on land. You've
           probably seen this before: after heavy rain, streams and rivers turn
           a dark brown or tea color. That color is organic matter.",
          "When that murky water reaches a reef, it blocks sunlight. Corals
           depend on sunlight because tiny algae living inside them use
           photosynthesis — just like plants do — to make food for the coral.
           Less light means less food, and a coral that can't eat is a coral
           that's already in trouble.",
          "fDOM is one of the quickest ways to spot when storm runoff has hit
           a reef. The brown, organic-rich water that rushes off the land after
           heavy rain glows brightly when you shine a specific light on it in the
           lab — making it easy to detect even in small amounts."
        )
      ),

      # ---- FAQ ---------------------------------------------------
      div(class = "param-section-title mt-4",
        HTML(fa("circle-question", fill = "#0077b6", height = "1em")),
        " Frequently Asked Questions"
      ),

      accordion(
        open = FALSE,

        accordion_panel(
          title = "Why were the samples frozen?",
          icon  = HTML(fa("snowflake", fill = "#0077b6", height = "1em")),
          p(
            "Most of the chemical measurements we're making are very sensitive —
            even small changes in temperature or the growth of bacteria in the
            bottle can alter the results. Freezing the samples as soon as
            possible after collection puts everything on pause, like hitting
            a \"freeze frame\" button on the chemistry. This way, the water
            we analyze in the lab is as close as possible to what was in the
            ocean on the day it was collected."
          )
        ),

        accordion_panel(
          title = "How long does lab analysis take?",
          icon  = HTML(fa("clock", fill = "#0077b6", height = "1em")),
          p(
            "It depends on the test! Salinity can be measured pretty quickly,
            but nutrient analyses like nitrate and phosphate take longer because
            the samples need to be prepared carefully and use specialized equipment.
            fDOM also requires specialized equipment. On top of that, we're sending 
            samples to multiple labs,and each lab has its own queue. We expect
            results to trickle in over the coming weeks and months — we'll post
            everything to the map as data arrives."
          )
        ),

        accordion_panel(
          title = "Who collected these samples?",
          icon  = HTML(fa("users", fill = "#0077b6", height = "1em")),
          p(
            "These samples were collected by an incredible mix of community
            volunteers, surfers, teachers, families, and scientists across
            O'ahu and Maui Nui. People waded into the water right after the
            storm — sometimes in the middle of the night — to capture a
            snapshot of what the ocean looked like in those critical hours.
            We are deeply grateful to everyone who participated."
          )
        ),

        accordion_panel(
          title = "What was the March 2026 Kona Low storm?",
          icon  = HTML(fa("cloud-showers-heavy", fill = "#0077b6", height = "1em")),
          p(
            "A Kona Low is a type of storm system that forms near Hawaiʻi and
            brings heavy rainfall, strong winds, and large surf. The March 2026
            event was one of the most significant in recent memory, dropping
            record amounts of rain across O'ahu and Maui in a short period of time.
            All that rain washed over roads, fields, and neighborhoods before
            flowing into streams and eventually out to the ocean — carrying
            whatever was on the land along with it."
          )
        ),

        accordion_panel(
          title = "What will you do with the results?",
          icon  = HTML(fa("magnifying-glass-chart", fill = "#0077b6", height = "1em")),
          p(
            "All results will be shared openly on this dashboard as they come in.
            We'll also use the data to understand which reefs were most exposed
            to runoff and how conditions varied across different sites. This
            information can help reef managers and restoration teams prioritize
            where to focus their efforts. We also hope this project builds a
            community of people who care about — and keep an eye on — the health
            of Hawaiʻi's reefs long after this storm."
          )
        ),

        accordion_panel(
          title = "Are Hawaiʻi's coral reefs in danger?",
          icon  = HTML(fa("triangle-exclamation", fill = "#0077b6", height = "1em")),
          p(
            "Yes — Hawaiʻi's reefs face a number of threats. Climate change is
            warming the ocean and causing coral bleaching events. Runoff from
            land (exactly what this project is studying) brings pollution,
            sediment, and nutrients that algae love but corals don't. And reefs
            that are already stressed from warming water are less able to bounce
            back from a pulse of runoff after a major storm."
          ),
          p(
            "The good news is that people across Hawaiʻi care deeply about
            reefs, and projects like this one help us understand the problem
            better so we can protect and restore what's left."
          )
        ),

        accordion_panel(
          title = "How can I find out if there are active public health warnings about the water?",
          icon  = HTML(fa("triangle-exclamation", fill = "#0077b6", height = "1em")),
          p(
            "The Hawai\u02BBi State Department of Health maintains an interactive",
            "map of current and recent beach and water quality advisories,",
            "warnings, and closures across the state. You can search by island",
            "or location to see whether any sites near you are currently flagged",
            "for unsafe water conditions."
          ),
          p(
            tags$a(
              href   = "https://eha-cloud.doh.hawaii.gov/EHP/#!/viewer",
              target = "_blank",
              class  = "btn btn-primary btn-sm",
              HTML(fa("arrow-up-right-from-square", fill = "white", height = "0.85em")),
              " Hawai\u02BBi DOH Environmental Health Portal"
            )
          ),
          p(
            class = "small text-muted mb-0",
            "Note: the DOH portal tracks official advisories issued by state",
            "agencies. The samples in this project are part of a separate",
            "research effort and will be reported here as results come in from",
            "the lab — they are not the source of DOH advisories."
          )
        ),

        accordion_panel(
          title = "Can I make a financial contribution to the project?",
          icon  = HTML(fa("hand-holding-dollar", fill = "#0077b6", height = "1em")),
          p(
            "Yes — and thank you for asking! Sampling efforts like this one require",
            "significant funding to cover lab processing, equipment, coordination,",
            "and the many hands it takes to make community science work.",
            "We are actively seeking support to be able to conduct similar sampling",
            "efforts in the future. Thank you to SOEST for funding this first effort!"
          ),
          p(
            "If you're interested in contributing or know of funding opportunities,",
            "please reach out to one of the PIs on this project:",
            tags$ul(
              class = "mb-0",
              tags$li(tags$a(href = "mailto:nyssa.silbiger@hawaii.edu", "Dr. Nyssa Silbiger")),
              tags$li(tags$a(href = "mailto:andreake@hawaii.edu", "Dr. Andrea Kealoha")),
              tags$li(tags$a(href = "mailto:sara.kahanamoku@berkeley.edu", "Dr. Sara Kahanamoku"))
            )
          )
        ),

        accordion_panel(
          title = "What does \u201cbelow detection limit\u201d mean?",
          icon  = HTML(fa("flask", fill = "#0077b6", height = "1em")),
          p(
            "Every instrument has a lower limit of sensitivity \u2014 a concentration",
            "so small that the machine can\u2019t reliably distinguish it from background",
            "noise. When a nutrient measurement falls below that threshold, we know",
            "the concentration is very low, but we can\u2019t report an exact number.",
            "In those cases, the value is recorded as 0."
          ),
          p(
            "The detection limits for the instruments used in this study are:"
          ),
          tags$ul(
            tags$li("Nitrate+Nitrite (N+N): 0.07\u00a0\u00b5mol/L"),
            tags$li("Phosphate: 0.07\u00a0\u00b5mol/L"),
            tags$li("Silicate: 0.01\u00a0\u00b5mol/L"),
            tags$li("Ammonium: 0.08\u00a0\u00b5mol/L")
          ),
          p(
            "A value of 0 for any of these nutrients means the true concentration",
            "is somewhere between zero and the detection limit listed above \u2014",
            "effectively, the water is very clean for that parameter.",
            "You\u2019ll see a \u201cbelow detection limit\u201d note in the sample popup",
            "on the map whenever this applies."
          )
        ),

        accordion_panel(
          title = "Can I still get involved?",
          icon  = HTML(fa("hand", fill = "#0077b6", height = "1em")),
          p(
            "The sample collection window for the Kona Low event has closed,
            but there are still ways to help! Share this dashboard with your
            friends, family, and teachers. Follow along as results come in.
            And if you're interested in future sampling events or reef
            monitoring in your area, reach out to the research team below —
            community scientists are always needed."
          )
        )
      ),

      # ---- Cite these data ---------------------------------------
      div(class = "param-section-title mt-4",
        HTML(fa("quote-left", fill = "#0077b6", height = "1em")),
        " Cite These Data"
      ),

      tags$p(class = "small text-muted mb-2", "Water quality data:"),
      div(
        class = "alert alert-light border",
        style = "font-size: 0.9rem; font-family: monospace;",
        "Silbiger, N., Kealoha, A., & Kahanamoku, S. (2026).",
        tags$i(
          "What\u2019s In Your Water? \u2014 Hawai\u02BBi Coastal Water Quality",
          "Samples, March 2026 Kona Low."
        ),
        "Zenodo.",
        tags$span(class = "text-muted", "DOI: 10.5281/zenodo.19410137")
      ),

      tags$p(class = "small text-muted mb-2", "Ahupua\u02BBa boundary data:"),
      div(
        class = "alert alert-light border",
        style = "font-size: 0.9rem; font-family: monospace;",
        "Office of Hawaiian Affairs (OHA) and Hawai\u02BBi Statewide GIS Program,",
        "Office of Planning and Sustainable Development, State of Hawai\u02BBi.",
        tags$i("Ahupua\u02BBa (Historic Land Divisions)."),
        "Original layer: October 2009; last updated February 2024.",
        "Distributed via Hawai\u02BBi Statewide GIS Program.",
        tags$a(
          href   = "https://geodata.hawaii.gov/arcgis/rest/services/HistoricCultural/MapServer/1",
          target = "_blank",
          style  = "word-break: break-all;",
          "geodata.hawaii.gov"
        ),
        "; full metadata:",
        tags$a(
          href   = "https://files.hawaii.gov/dbedt/op/gis/data/ahupuaa.pdf",
          target = "_blank",
          "ahupuaa.pdf"
        )
      ),

      # ---- Contact -----------------------------------------------
      div(class = "param-section-title mt-4",
        HTML(fa("envelope", fill = "#0077b6", height = "1em")),
        " Contact"
      ),

      layout_column_wrap(
        width = "280px",
        fill  = FALSE,
        card(
          card_body(
            tags$p(tags$strong("Dr. Nyssa Silbiger"), class = "mb-1"),
            tags$p(
              class = "mb-1",
              tags$a(href = "mailto:silbiger@hawaii.edu", "silbiger@hawaii.edu")
            ),
            tags$p(
              class = "mb-0",
              tags$a(href = "https://silbigerlab.com", target = "_blank",
                     "silbigerlab.com")
            )
          )
        ),
        card(
          card_body(
            tags$p(tags$strong("Dr. Andrea Kealoha"), class = "mb-1"),
            tags$p(
              class = "mb-1",
              tags$a(href = "mailto:andreake@hawaii.edu", "andreake@hawaii.edu")
            ),
            tags$p(
              class = "mb-0",
              tags$a(href = "https://andreake6.wixsite.com/andreakealoha",
                     target = "_blank", "andreakealoha")
            )
          )
        ),
        card(
          card_body(
            tags$p(tags$strong("Dr. Sara Kahanamoku"), class = "mb-1"),
            tags$p(
              class = "mb-1",
              tags$a(href = "mailto:sara.kahanamoku@hawaii.edu", "sara.kahanamoku@hawaii.edu")
            ),
            tags$p(
              class = "mb-0",
              tags$a(href = "https://www.skahanamoku.com/", target = "_blank",
                     "skahanamoku.com")
            )
          )
        )
      ),

      funding_banner
    )
  ),


  # ================================================================
  # Page 3: About Us
  # ================================================================

  nav_panel(
    title = "About Us",
    fillable = FALSE,

    div(
      class = "container-lg py-4",

      h3(
        class = "mb-1",
        HTML(fa("users", fill = "#0077b6", height = "1em")),
        " Meet the Team"
      ),
      p(
        class = "text-muted mb-4",
        "The scientists behind What's in Your Water."
      ),

      layout_column_wrap(
        width = "280px",

        # --- Nyssa Silbiger ---
        card(
          card_image(
            src   = "https://nyssasilbiger.com/x/cdn/?https://storage.googleapis.com/production-sitebuilder-v1-0-1/231/293231/6KnGbg0A/3d4b35a0e35a4baea7fa48ee592a4b29",
            alt   = "Dr. Nyssa Silbiger",
            style = "object-fit: contain; height: 280px; width: 100%; background-color: #f8f9fa;"
          ),
          card_body(
            h5("Dr. Nyssa Silbiger", class = "mb-0"),
            tags$p(class = "text-muted small mb-2",
              "Associate Professor, Department of Oceanography, UH Mānoa",
              tags$br(),
              "Associate Director, Uehiro Center for the Advancement of Oceanography"
            ),
            p(class = "small",
              "Nyssa is a marine ecologist and data scientist studying how climate change ",
              "affects coral reef ecosystems. Her research focuses on the interplay between ",
              "ocean acidification, nutrient pollution, and reef carbonate dynamics across ",
              "the Hawaiian Archipelago and beyond."
            ),
            tags$a(
              href   = "https://silbigerlab.com",
              target = "_blank",
              class  = "btn btn-outline-primary btn-sm",
              "Silbiger Website"
            )
          )
        ),

        # --- Andrea Kealoha ---
        card(
          card_image(
            src   = "https://static.wixstatic.com/media/77a9be_e0c64ab3a14a45509fb87d197d512559~mv2.png",
            alt   = "Dr. Andrea Kealoha",
            style = "object-fit: contain; height: 280px; width: 100%; background-color: #f8f9fa;"
          ),
          card_body(
            h5("Dr. Andrea Kealoha", class = "mb-0"),
            tags$p(class = "text-muted small mb-2",
              "Assistant Professor, Department of Oceanography, UH Mānoa"
            ),
            p(class = "small",
              "Andrea is a Kanaka ʻŌiwi (Native Hawaiian) scientist raised in Maui. Her research focuses on global and local stressors to marine ecosystems, ",
              "particularly coral reefs. Her lab investigates the physical and chemical conditions ",
              "that support or inhibit coral reef functioning using carbonate chemistry, ",
              "biogeochemistry, and in-situ sensors."
            ),
            tags$a(
              href   = "https://andreake6.wixsite.com/andreakealoha",
              target = "_blank",
              class  = "btn btn-outline-primary btn-sm",
              "Kealoha Website"
            )
          )
        ),

        # --- Sara Kahanamoku ---
        card(
          card_image(
            src   = "Kahanamoku-SR2422.JPEG",
            alt   = "Dr. Sara Kahanamoku",
            style = "object-fit: cover; height: 280px; width: 100%;"
          ),
          card_body(
            h5("Dr. Sara Kahanamoku", class = "mb-0"),
            tags$p(class = "text-muted small mb-2",
              "Early Career Research Fellow, Hawaiʻi Sea Grant / SOEST, UH Mānoa"
            ),
            p(class = "small",
              "Sara is a geologist and ecologist who reconstructs past marine ecosystem ",
              "change using high-resolution sediment core records to understand how climate ",
              "warming, land-use change, and habitat loss have shaped modern reef systems ",
              "across intermediate timescales relevant to conservation. A Kanaka ʻŌiwi ",
              "(Native Hawaiian) and Maʻohi (Indigenous Tahitian) scientist, they integrate ",
              "Indigenous knowledge and cutting-edge science to advance community-driven ",
              "climate resilience."
            ),
            tags$a(
              href   = "https://www.skahanamoku.com",
              target = "_blank",
              class  = "btn btn-outline-primary btn-sm",
              "Kahanamoku Website"
            )
          )
        )
      ),

      funding_banner
    )
  ),

  # ================================================================
  # Page 4: Press
  # ================================================================

  nav_panel(
    title = "Press",
    fillable = FALSE,

    div(
      class = "container-lg py-4",

      h3(
        class = "mb-1",
        HTML(fa("newspaper", fill = "#0077b6", height = "1em")),
        " In the News"
      ),
      p(
        class = "text-muted mb-4",
        "Media coverage of the What\u2019s In Your Water? project and the",
        "March 2026 Kona Low storm."
      ),

      layout_column_wrap(
        width = "280px",
        heights_equal = "row",

        make_press_card(
          outlet   = "Hawai\u02BBi Public Radio",
          date_str = "April 1, 2026",
          headline = "Citizen scientists collect hundreds of ocean samples to test storm runoff effects",
          excerpt  = paste(
            "More than 100 community volunteers — nurses, students, surfers,",
            "and families — collected over 700 samples from coastlines across",
            "O\u02BBahu and Maui Nui. Reported by Savannah Harriman-Pote."
          ),
          url     = paste0(
            "https://www.hawaiipublicradio.org/local-news/2026-04-01/",
            "citizen-scientists-collect-hundreds-of-ocean-samples-to-test-storm-runoff-effects"
          ),
          img_url = paste0(
            "https://npr.brightspotcdn.com/dims4/default/5cf751e/2147483647/",
            "strip/true/crop/1920x1440+0+0/resize/880x660!/quality/90/",
            "?url=http%3A%2F%2Fnpr-brightspot.s3.amazonaws.com%2F75%2F72%2F",
            "7a641eea47f1b16fbd5fe1f19292%2Fsurfingsample.jpg"
          ),
          img_alt = "A volunteer holds a water sample collected while out surfing near L\u0113\u02BAahi."
        ),

        make_press_card(
          outlet   = "Maui Now",
          date_str = "March 27, 2026",
          headline = "Community event this Sunday to test coastal water quality following kona storm",
          excerpt  = paste(
            "The University of Hawai\u02BBi called on Maui volunteers to collect",
            "ocean water samples at beaches, surf spots, and fishing spots ahead",
            "of the March 29 community sampling event."
          ),
          url     = paste0(
            "https://mauinow.com/2026/03/27/",
            "community-event-this-sunday-to-test-coastal-water-quality-following-kona-storm/"
          ),
          img_url = paste0(
            "https://media.mauinow.com/file/mauinow/2026/03/",
            "Screenshot-2026-03-23-at-1.28.29-PM-824x1024.png"
          ),
          img_alt = "Kona storm impacts, March 2026. PC: County of Maui."
        ),

        make_press_card(
          outlet   = "Hawaii News Now",
          date_str = "March 28, 2026",
          headline = "Nearly a week after the kona storms, more effects on health are emerging",
          excerpt  = paste(
            "Health officials warned of bacterial contamination risks from",
            "floodwaters entering the ocean, as UH researchers mobilized",
            "community scientists to sample beaches statewide. Reported by Ben Gutierrez."
          ),
          url     = paste0(
            "https://www.hawaiinewsnow.com/2026/03/28/",
            "nearly-week-after-kona-storms-more-effects-health-are-emerging/"
          )
        )
      ),

      funding_banner
    )
  )

) # end page_navbar
) # end tagList


# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------

server <- function(input, output, session) {

  # Track when data was last successfully loaded
  data_loaded_rv <- reactiveVal(format(Sys.time(), "%b %d, %Y %I:%M %p HST"))

  output$data_loaded_at_text <- renderText({ data_loaded_rv() })

  # Live data: re-reads Google Sheets every 5 minutes.
  # Falls back to startup data if the network call fails.
  live_data <- reactive({
    invalidateLater(5 * 60 * 1000)

    tryCatch({
      suppressMessages(source("01_load_clean_data.R", local = TRUE))
      data_loaded_rv(format(Sys.time(), "%b %d, %Y %I:%M %p HST"))
      df_joined <- df_clean |>
        mutate(
          hour        = hour(collected_hst),
          minute      = minute(collected_hst),
          time_of_day = if_else(
            (hour * 60 + minute) >= (6 * 60) & (hour * 60 + minute) < (18 * 60 + 45),
            "Daytime", "Nighttime"
          ),
          date = as.Date(collected_hst, tz = "Pacific/Honolulu")
        ) |>
        full_join(chem_data, by = "sample_id") |>
        left_join(cesspool_dist, by = "sample_id")
      list(
        df                 = df_joined,
        samples_analyzed   = samples_analyzed,
        salinity_analyzed  = df_joined |>
          filter(!is.na(Salinity)) |> distinct(sample_id) |> nrow(),
        nutrients_analyzed = df_joined |>
          filter(!is.na(NO3NO2)) |> distinct(sample_id) |> nrow()
      )
    }, error = function(e) {
      list(
        df                 = df_app,
        samples_analyzed   = 0L,
        salinity_analyzed  = df_app |>
          filter(!is.na(Salinity)) |> distinct(sample_id) |> nrow(),
        nutrients_analyzed = df_app |>
          filter(!is.na(NO3NO2)) |> distinct(sample_id) |> nrow()
      )
    })
  })

  # Update filter choices when new islands or dates appear in the data.
  # Preserves the user's current selections; auto-selects genuinely new choices.
  prev_island_choices <- reactiveVal(island_choices)
  prev_date_choices   <- reactiveVal(unname(date_choices))

  observe({
    ld <- live_data()
    df <- ld$df

    new_island_choices <- sort(unique(df$island))
    new_date_vals      <- sort(unique(df$date))
    new_date_choices   <- setNames(as.character(new_date_vals),
                                   format(new_date_vals, "%b %d, %Y"))

    new_islands <- setdiff(new_island_choices, prev_island_choices())
    if (length(new_islands) > 0 ||
        !identical(new_island_choices, prev_island_choices())) {
      updateCheckboxGroupInput(session, "island",
        choices  = new_island_choices,
        selected = union(isolate(input$island), new_islands)
      )
      prev_island_choices(new_island_choices)
    }

    new_dates <- setdiff(as.character(new_date_vals), prev_date_choices())
    if (length(new_dates) > 0 ||
        !identical(as.character(new_date_vals), prev_date_choices())) {
      updateCheckboxGroupInput(session, "sampling_date",
        choices  = new_date_choices,
        selected = union(isolate(input$sampling_date), new_dates)
      )
      prev_date_choices(as.character(new_date_vals))
    }
  })

  # ---- Reset all filters -------------------------------------------------
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "island",         selected = island_choices)
    updateSelectizeInput(    session, "moku_filter",    selected = "")
    updateSelectizeInput(    session, "ahupuaa_filter", selected = "")
    updateCheckboxGroupInput(session, "sampling_date",  selected = date_choices)
  })

  # ---- Cascading moku / ahupuaʻa filters --------------------------------

  # When island selection changes, repopulate moku choices and reset ahupuaʻa
  observeEvent(input$island, {
    moku_opts <- live_data()$df |>
      filter(island %in% input$island, !is.na(moku)) |>
      pull(moku) |>
      unique() |>
      sort()
    updateSelectizeInput(session, "moku_filter",
      choices  = c("All moku" = "", moku_opts),
      selected = ""
    )
    updateSelectizeInput(session, "ahupuaa_filter",
      choices  = c("All ahupua\u02BBa" = ""),
      selected = ""
    )
  }, ignoreInit = TRUE)

  # When moku changes, repopulate ahupuaʻa choices and reset selection
  observeEvent(input$moku_filter, {
    ahupuaa_opts <- live_data()$df |>
      filter(
        island %in% input$island,
        if (nzchar(input$moku_filter)) moku == input$moku_filter else TRUE,
        !is.na(ahupuaa)
      ) |>
      pull(ahupuaa) |>
      unique() |>
      sort()
    updateSelectizeInput(session, "ahupuaa_filter",
      choices  = c("All ahupua\u02BBa" = "", ahupuaa_opts),
      selected = ""
    )
  }, ignoreInit = TRUE)

  # Progress bar (reactive so it updates when samples_analyzed changes)
  make_progress_bar <- function(label, n, tot) {
    pct       <- if (tot > 0) round(n / tot * 100) else 0
    bar_color <- if (pct == 0) "bg-secondary" else "bg-primary"
    bar_width <- paste0(max(pct, 2), "%")
    div(
      class = "mb-3",
      div(
        class = "progress-label",
        tags$span(
          class = "big-count", n,
          tags$span(class = "total-count", paste0(" of ~", tot, " ", label))
        ),
        tags$span(class = "total-count", paste0(pct, "% complete"))
      ),
      tags$div(
        class = "progress",
        role  = "progressbar",
        `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
        tags$div(class = paste("progress-bar", bar_color),
                 style = paste0("width: ", bar_width, ";"))
      )
    )
  }

  output$progress_section <- renderUI({
    ld  <- live_data()
    tot <- nrow(ld$df)

    div(
      class = "progress-section",
      make_progress_bar("salinity samples processed", ld$salinity_analyzed,  tot),
      make_progress_bar("nutrient samples processed", ld$nutrients_analyzed, tot),
      tags$small(
        class = "text-muted mt-2 d-block",
        HTML(fa("rotate", height = "0.85em")),
        " Results are added to the map as each batch comes back from the lab."
      ),
      tags$div(
        class = "mt-3 text-center",
        tags$a(
          href    = "javascript:void(0);",
          onclick = paste0(
            "document.querySelectorAll('.nav-link').forEach(",
            "function(el){",
            "  if(el.textContent.trim()==='Data') el.click();",
            "});"
          ),
          class = "btn btn-primary btn-sm",
          HTML(fa("map-location-dot", fill = "white", height = "0.9em")),
          " Show me the data!"
        )
      )
    )
  })

  # Download filtered data (public columns only)
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("whats_in_your_water_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_filtered() |>
        select(island, sample_id, latitude, longitude) |>
        write.csv(file, row.names = FALSE)
    }
  )

  # Select all / deselect all dates (uses current live choices)
  observeEvent(input$date_select_all, {
    updateCheckboxGroupInput(session, "sampling_date",
                             selected = prev_date_choices())
  })
  observeEvent(input$date_deselect_all, {
    updateCheckboxGroupInput(session, "sampling_date", selected = character(0))
  })

  # ---- Sample ID search --------------------------------------------------
  sample_id_query <- reactiveVal("")

  observeEvent(input$search_sample_btn, {
    sample_id_query(toupper(trimws(input$sample_id_search)))
  })

  observeEvent(input$clear_sample_btn, {
    sample_id_query("")
    updateTextInput(session, "sample_id_search", value = "")
  })

  output$sample_search_result <- renderUI({
    q <- sample_id_query()
    if (!nzchar(q)) return(NULL)
    match <- live_data()$df |> filter(toupper(sample_id) == q)
    if (nrow(match) == 0) {
      tags$small(class = "text-danger", paste0("No sample found for \u201c", q, "\u201d."))
    } else {
      tags$small(class = "text-success",
        HTML(fa("circle-check", height = "0.85em")),
        paste0(" Found: ", match$sample_id[1], " \u2014 zoomed to marker.")
      )
    }
  })



  # Reactive filtered data (uses live_data so updates automatically)
  df_filtered <- reactive({
    q <- sample_id_query()

    # When a sample ID search is active, bypass island/date filters so the
    # marker is always visible on the map regardless of current filter state
    if (nzchar(q)) {
      return(
        live_data()$df |> filter(toupper(sample_id) == q, !is.na(latitude))
      )
    }

    df <- live_data()$df |>
      filter(
        island %in% input$island,
        as.character(date) %in% input$sampling_date
      )

    if (nzchar(input$moku_filter)) {
      df <- df |> filter(moku == input$moku_filter)
    }

    if (nzchar(input$ahupuaa_filter)) {
      df <- df |> filter(ahupuaa == input$ahupuaa_filter)
    }

    df
  })

  # Value box outputs
  output$samples_box <- renderUI({
    df  <- df_filtered()
    tot <- nrow(live_data()$df)

    col_info <- list(
      salinity  = list(col = "Salinity", label = "Salinity"),
      nitrate   = list(col = "NO3NO2",   label = "Nitrate+Nitrite"),
      phosphate = list(col = "PO4",      label = "Phosphate"),
      silicate  = list(col = "SiO2",     label = "Silicate"),
      ammonium  = list(col = "NH3",      label = "Ammonium")
    )

    if (input$color_by %in% names(col_info)) {
      info  <- col_info[[input$color_by]]
      n_sub <- df |> filter(!is.na(.data[[info$col]])) |> nrow()
      value_box(
        title  = paste(info$label, "Samples Shown out of Total"),
        value  = paste(n_sub, "of", tot),
        theme  = "primary",
        height = "90px"
      )
    } else {
      value_box(
        title  = "Samples Shown out of Total",
        value  = paste(nrow(df), "of", tot),
        theme  = "primary",
        height = "90px"
      )
    }
  })

  output$n_islands <- renderText(n_distinct(df_filtered()$island))

  output$n_samplers <- renderText(n_distinct(df_filtered()$name))

  output$collection_date <- renderText({
    dates <- as.Date(df_filtered()$collected_hst, tz = "Pacific/Honolulu")
    if (length(dates) == 0) return("—")
    min_date <- format(min(dates), "%b %d, %Y")
    max_date <- format(max(dates), "%b %d, %Y")
    if (min_date == max_date) min_date else paste(min_date, "\u2013", max_date)
  })

  # Base map — rendered once; markers updated reactively via leafletProxy
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      setView(lng = -157.5, lat = 20.5, zoom = 7)
      # |> addPolygons(
      #   data        = cesspool_tracts,
      #   fillColor   = ~cesspool_pal(final_cat_),
      #   fillOpacity = 0.45,
      #   color       = "white",
      #   weight      = 0.4,
      #   group       = "cesspools",
      #   label       = ~paste0("Cesspool Priority: ", final_cat_),
      #   labelOptions = labelOptions(textsize = "12px")
      # ) |>
      # hideGroup("cesspools")
  })

  # # Show/hide cesspool layer in response to the checkbox
  # observeEvent(input$show_cesspools, {
  #   proxy <- leafletProxy("map")
  #   if (input$show_cesspools) {
  #     proxy |> showGroup("cesspools")
  #   } else {
  #     proxy |> hideGroup("cesspools")
  #   }
  # }, ignoreInit = TRUE)

  # Empty map overlay — shown when no samples match the current filters
  output$empty_map_overlay <- renderUI({
    req(nrow(df_filtered()) == 0)
    div(
      style = paste(
        "position: absolute; top: 0; left: 0; right: 0; bottom: 0;",
        "display: flex; flex-direction: column;",
        "align-items: center; justify-content: center;",
        "background: rgba(255,255,255,0.82); z-index: 500; pointer-events: none;"
      ),
      tags$div(
        style = "text-align: center; padding: 24px;",
        HTML(fa("map-location-dot", height = "2.5em", fill = "#adb5bd")),
        tags$h5("No samples match these filters", class = "mt-3 mb-1 text-muted"),
        tags$p(
          class = "text-muted small mb-0",
          "Try adjusting the mokupuni, moku, or date filters,",
          tags$br(),
          "or click ",
          tags$strong("Reset All Filters"),
          " to start over."
        )
      )
    )
  })

  # ------------------------------------------------------------------
  # Popup builder — defined at server scope so the map observer and
  # the strip plot click handler can both use it.
  # ------------------------------------------------------------------
  sal_tooltip    <- paste0(
    "Normal seawater is \u223434\u201336\u00a0psu. ",
    "Lower values may indicate freshwater input ",
    "from runoff, streams, or submarine groundwater discharge (SGD)."
  )
  no3no2_tooltip <- "Normal range for Hawaiian nearshore reefs: <1\u00a0\u00b5mol/L"
  po4_tooltip    <- "Normal range for Hawaiian nearshore reefs: <0.1\u00a0\u00b5mol/L"
  sio2_tooltip   <- "Normal range for Hawaiian nearshore waters: <5\u00a0\u00b5mol/L"
  nh3_tooltip    <- "Normal range for Hawaiian nearshore reefs: <1\u00a0\u00b5mol/L"

  make_popup <- function(d) {
    salinity_line <- if_else(
      !is.na(d$Salinity),
      paste0(
        "<br>Salinity: ", round(d$Salinity, 2), " psu ",
        "<span title='", sal_tooltip, "' ",
        "style='cursor:help; color:#0077b6; font-size:0.9em;'>\u24d8</span>"
      ),
      "<br>Salinity: <i>Not yet processed</i>"
    )
    no3no2_line <- if_else(
      !is.na(d$NO3NO2),
      paste0(
        "<br>Nitrate+Nitrite: ", round(d$NO3NO2, 2), " \u00b5mol/L",
        if_else(d$NO3NO2 < 0.07, " <small style='color:#6c757d;'>(below detection limit)</small>", ""),
        " <span title='", no3no2_tooltip, "' ",
        "style='cursor:help; color:#0077b6; font-size:0.9em;'>\u24d8</span>"
      ),
      ""
    )
    po4_line <- if_else(
      !is.na(d$PO4),
      paste0(
        "<br>Phosphate: ", round(d$PO4, 2), " \u00b5mol/L",
        if_else(d$PO4 < 0.07, " <small style='color:#6c757d;'>(below detection limit)</small>", ""),
        " <span title='", po4_tooltip, "' ",
        "style='cursor:help; color:#0077b6; font-size:0.9em;'>\u24d8</span>"
      ),
      ""
    )
    sio2_line <- if_else(
      !is.na(d$SiO2),
      paste0(
        "<br>Silicate: ", round(d$SiO2, 2), " \u00b5mol/L",
        if_else(d$SiO2 < 0.01, " <small style='color:#6c757d;'>(below detection limit)</small>", ""),
        " <span title='", sio2_tooltip, "' ",
        "style='cursor:help; color:#0077b6; font-size:0.9em;'>\u24d8</span>"
      ),
      ""
    )
    nh3_line <- if_else(
      !is.na(d$NH3),
      paste0(
        "<br>Ammonium: ", round(d$NH3, 2), " \u00b5mol/L",
        if_else(d$NH3 < 0.08, " <small style='color:#6c757d;'>(below detection limit)</small>", ""),
        " <span title='", nh3_tooltip, "' ",
        "style='cursor:help; color:#0077b6; font-size:0.9em;'>\u24d8</span>"
      ),
      ""
    )
    ahupuaa_line <- if_else(
      !is.na(d$ahupuaa),
      paste0("<br>Ahupua\u02BBa: ", d$ahupuaa, " (", d$moku, ")"),
      ""
    )
    paste0(
      "<b>Sample: ", d$sample_id, "</b><br>",
      "Mokupuni: ", d$island, "<br>",
      "Collected: ", format(d$collected_hst, "%b %d, %Y %I:%M %p HST"),
      ahupuaa_line,
      salinity_line,
      no3no2_line,
      po4_line,
      sio2_line,
      nh3_line,
      if_else(
        !is.na(d$notes) & nchar(trimws(d$notes)) > 0,
        paste0("<br><i>", d$notes, "</i>"),
        ""
      )
    )
  }

  # Update markers when filters change
  observe({
    df <- df_filtered()

    proxy <- leafletProxy("map") |>
      clearMarkers() |>
      clearControls()

    if (nrow(df) == 0) return()

    if (input$color_by == "moku") {
      all_moku <- sort(unique(na.omit(live_data()$df$moku)))
      moku_colors <- c(
        RColorBrewer::brewer.pal(9, "Set1"),
        RColorBrewer::brewer.pal(min(length(all_moku) - 9, 8), "Set2")
      )
      pal <- colorFactor(palette = moku_colors, domain = all_moku)

      proxy |>
        addCircleMarkers(
          data        = df,
          lng         = ~longitude,
          lat         = ~latitude,
          radius      = 7,
          color       = "white",
          fillColor   = ~pal(moku),
          fillOpacity = 0.85,
          weight      = 1.5,
          popup       = make_popup(df)
        ) |>
        addLegend(
          position  = "topright",
          pal       = pal,
          values    = all_moku,
          title     = "Moku",
          opacity   = 0.85
        )
      return()
    }

    # ---- Numeric color scales (salinity + nutrients) -------------------
    numeric_cols <- list(
      salinity  = list(col = "Salinity", title = "Salinity<br><small>(psu)</small>"),
      nitrate   = list(col = "NO3NO2",   title = "Nitrate+Nitrite<br><small>(\u00b5mol/L)</small>"),
      phosphate = list(col = "PO4",      title = "Phosphate<br><small>(\u00b5mol/L)</small>"),
      silicate  = list(col = "SiO2",     title = "Silicate<br><small>(\u00b5mol/L)</small>"),
      ammonium  = list(col = "NH3",      title = "Ammonium<br><small>(\u00b5mol/L)</small>")
    )

    if (input$color_by %in% names(numeric_cols)) {
      info     <- numeric_cols[[input$color_by]]
      all_vals <- live_data()$df[[info$col]]
      val_rng  <- range(all_vals, na.rm = TRUE)
      has_data <- any(!is.na(all_vals)) && val_rng[1] != val_rng[2]

      if (has_data) {
        df_sub <- df |> filter(!is.na(.data[[info$col]]))

        if (input$color_by == "salinity") {
          # Power transform (p > 1) compresses the low outlier into a narrow
          # dark band and stretches colour differences across the high end,
          # while the full value range remains visible in the legend.
          sal_vals <- all_vals[!is.na(all_vals)]
          min_sal  <- min(sal_vals)
          max_sal  <- max(sal_vals)
          p        <- 2

          t_fwd <- function(x) ((x - min_sal) / (max_sal - min_sal)) ^ p
          t_inv <- function(t) min_sal + (max_sal - min_sal) * t ^ (1 / p)

          pal <- colorNumeric(palette = "viridis", domain = c(0, 1))

          proxy |>
            addCircleMarkers(
              data        = df_sub,
              lng         = ~longitude,
              lat         = ~latitude,
              radius      = 7,
              color       = "white",
              fillColor   = pal(t_fwd(df_sub$Salinity)),
              fillOpacity = 0.85,
              weight      = 1.5,
              popup       = make_popup(df_sub)
            ) |>
            addLegend(
              position  = "topright",
              pal       = pal,
              values    = t_fwd(sal_vals),
              title     = info$title,
              labFormat = labelFormat(transform = t_inv, digits = 1),
              opacity   = 0.85
            )
          return()
        }

        nutrient_cols <- c("nitrate", "phosphate", "silicate", "ammonium")
        if (input$color_by %in% nutrient_cols) {
          nut_vals <- all_vals[!is.na(all_vals)]
          log_vals <- log1p(nut_vals)
          min_log  <- min(log_vals)
          max_log  <- max(log_vals)

          t_fwd <- function(x) (log1p(x) - min_log) / (max_log - min_log)
          t_inv <- function(t) expm1(min_log + t * (max_log - min_log))

          pal <- colorNumeric(palette = "viridis", domain = c(0, 1))

          proxy |>
            addCircleMarkers(
              data        = df_sub,
              lng         = ~longitude,
              lat         = ~latitude,
              radius      = 7,
              color       = "white",
              fillColor   = pal(t_fwd(df_sub[[info$col]])),
              fillOpacity = 0.85,
              weight      = 1.5,
              popup       = make_popup(df_sub)
            ) |>
            addLegend(
              position  = "topright",
              pal       = pal,
              values    = t_fwd(nut_vals),
              title     = info$title,
              labFormat = labelFormat(transform = t_inv, digits = 2),
              opacity   = 0.85
            )
          return()
        }

        pal <- colorNumeric(palette = "viridis", domain = val_rng)

        proxy |>
          addCircleMarkers(
            data        = df_sub,
            lng         = ~longitude,
            lat         = ~latitude,
            radius      = 7,
            color       = "white",
            fillColor   = pal(df_sub[[info$col]]),
            fillOpacity = 0.85,
            weight      = 1.5,
            popup       = make_popup(df_sub)
          ) |>
          addLegend(
            position  = "topright",
            pal       = pal,
            values    = all_vals[!is.na(all_vals)],
            title     = info$title,
            opacity   = 0.85
          )
        return()
      }
    }

    proxy |>
      addCircleMarkers(
        data        = df,
        lng         = ~longitude,
        lat         = ~latitude,
        radius      = 7,
        color       = "white",
        fillColor   = marker_color,
        fillOpacity = 0.85,
        weight      = 1.5,
        popup       = make_popup(df)
      )
  })

  # ---- Results tab: chemistry summary plots ----------------------------

  # Shared reactive: compute island colors and check data availability
  chem_summary <- reactive({
    df      <- live_data()$df |>
      filter(
        ahupuaa != "Open Ocean" | is.na(ahupuaa),
        sample_id != "M226"   # extreme Moloka'i outlier (Kaluako'i)
      )
    islands <- sort(unique(na.omit(df$island)))
    list(
      df           = df,
      has_chem     = any(!is.na(df$NO3NO2)) || any(!is.na(df$PO4)) ||
                       any(!is.na(df$SiO2)) || any(!is.na(df$NH3)),
      island_colors = setNames(island_palette[seq_along(islands)], islands)
    )
  })

  output$results_no_data_msg <- renderUI({
    if (!chem_summary()$has_chem) {
      div(
        class = "alert alert-info d-flex align-items-start gap-3 mb-4",
        HTML(fa("circle-info", fill = "#0c5460", height = "1.2em")),
        tags$div(
          tags$strong("Chemistry results are still being processed. "),
          "Check back soon \u2014 this page updates automatically as",
          "lab data arrives."
        )
      )
    }
  })

  output$plot_report_card <- renderPlot({
    cs <- chem_summary()
    req(cs$has_chem)
    df <- cs$df

    exceed_pcts <- df |>
      select(NO3NO2, PO4, SiO2, NH3) |>
      summarise(across(
        everything(),
        ~round(mean(. > thresholds[cur_column()], na.rm = TRUE) * 100)
      )) |>
      unlist()

    (make_gauge(exceed_pcts["NO3NO2"], "Nitrate + Nitrite", 1) +
     make_gauge(exceed_pcts["PO4"],    "Phosphate",         0.1) +
     make_gauge(exceed_pcts["SiO2"],   "Silicate",          5) +
     make_gauge(exceed_pcts["NH3"],    "Ammonium",          1)) +
      plot_layout(ncol = 4, nrow = 1) +
      plot_annotation(
        title    = "Hawaiian Nearshore Water Quality Report",
        subtitle = "Percentage of water samples exceeding reef health thresholds",
        caption  = "Thresholds based on normal ranges for Hawaiian nearshore reefs",
        theme    = theme(
          plot.title    = element_text(face="bold", size=17, hjust=0.5,
                                       margin=margin(b=4)),
          plot.subtitle = element_text(size=12, hjust=0.5, color="grey40",
                                       margin=margin(b=8)),
          plot.caption  = element_text(size=9, color="grey55", hjust=0.5)
        )
      )
  }, res = 120)

  # ---- Strip plots: one per nutrient so click handlers know which column ----
  # (Shiny does not return panelvar1 for facet_wrap clicks, so a single
  #  faceted plot with a shared click handler cannot determine the nutrient.)

  strip_params <- list(
    list(id = "no3no2", col = "NO3NO2", label = "Nitrate+Nitrite (\u00b5mol/L)", thresh = 1),
    list(id = "po4",    col = "PO4",    label = "Phosphate (\u00b5mol/L)",       thresh = 0.1),
    list(id = "sio2",   col = "SiO2",   label = "Silicate (\u00b5mol/L)",        thresh = 5),
    list(id = "nh3",    col = "NH3",    label = "Ammonium (\u00b5mol/L)",         thresh = 1)
  )

  # Wide-format data shared across all four strip plots
  strip_data <- reactive({
    chem_summary()$df |>
      filter(!is.na(island)) |>
      select(sample_id, island, NO3NO2, PO4, SiO2, NH3) |>
      mutate(island = factor(island, levels = sort(unique(island))))
  })

  # Helper: build one nutrient strip plot
  make_one_strip <- function(sd_col, icolors, display_label, thresh) {
    if (nrow(sd_col) == 0) return(ggplot() + theme_void())
    ggplot(sd_col, aes(x = value, y = island, color = island)) +
      geom_jitter(height = 0.2, width = 0, alpha = 0.55, size = 1.8) +
      geom_vline(xintercept = thresh, color = "firebrick",
                 linetype = "dashed", linewidth = 0.8) +
      scale_color_manual(values = icolors, guide = "none") +
      scale_x_continuous(trans  = "log1p",
                         labels = scales::label_number(drop0trailing = TRUE)) +
      labs(title = display_label, x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold", size = 11)
      )
  }

  # Render + click handler for each nutrient (lapply captures sp correctly)
  lapply(strip_params, function(sp) {
    output[[paste0("plot_strip_", sp$id)]] <- renderPlot({
      req(chem_summary()$has_chem)
      sd      <- strip_data()
      icolors <- chem_summary()$island_colors
      sd_col  <- sd |>
        filter(!is.na(.data[[sp$col]])) |>
        mutate(value = .data[[sp$col]])
      make_one_strip(sd_col, icolors, sp$label, sp$thresh)
    }, res = 120)
  })

  output$plot_ridgeline <- renderPlot({
    cs  <- chem_summary()
    df  <- cs$df
    icolors <- cs$island_colors

    sal_df <- df |> filter(!is.na(Salinity), !is.na(island))
    req(nrow(sal_df) >= 2)

    ggplot(sal_df, aes(x = Salinity, y = island, fill = island)) +
      geom_density_ridges(
        alpha          = 0.75,
        scale          = 0.9,
        quantile_lines = TRUE,
        quantiles      = 2,
        color          = "white"
      ) +
      geom_vline(xintercept = 34, color = "#2E7D32",
                 linetype = "dotted", linewidth = 1.1) +
      geom_vline(xintercept = 36, color = "#2E7D32",
                 linetype = "dotted", linewidth = 1.1) +
      
      scale_fill_manual(values = icolors, guide = "none") +
      labs(
        title    = "Salinity distribution by island",
        subtitle = "Median shown as white line. Dotted green line = normal seawater (34\u00a0psu - 36\u00a0psu). Lower values = more freshwater.",
        x        = "Salinity (psu)",
        y        = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold")
      )
  }, res = 120)

  output$plot_stoplight <- renderPlot({
    cs <- chem_summary()
    req(cs$has_chem)
    df <- cs$df

    heatmap_df <- df |>
      filter(!is.na(moku)) |>
      group_by(island, moku) |>
      summarise(
        n                 = n(),
        `Nitrate+Nitrite` = mean(NO3NO2 > thresholds["NO3NO2"], na.rm = TRUE) * 100,
        `Phosphate`       = mean(PO4    > thresholds["PO4"],    na.rm = TRUE) * 100,
        `Silicate`        = mean(SiO2   > thresholds["SiO2"],   na.rm = TRUE) * 100,
        `Ammonium`        = mean(NH3    > thresholds["NH3"],     na.rm = TRUE) * 100,
        .groups = "drop"
      ) |>
      filter(n >= 3) |>
      pivot_longer(
        c(`Nitrate+Nitrite`, Phosphate, Silicate, Ammonium),
        names_to = "nutrient", values_to = "pct"
      ) |>
      filter(!is.nan(pct)) |>
      mutate(
        label = if_else(
          moku %in% c("L\u0101hain\u0101", "Kona"),
          paste0(moku, " (", island, ")"),
          moku
        ),
        label    = fct_reorder(label, pct, mean, .na_rm = TRUE),
        nutrient = factor(nutrient,
                          levels = c("Nitrate+Nitrite", "Phosphate",
                                     "Silicate", "Ammonium")),
        grade = case_when(
          pct < 25 ~ "Low",
          pct < 50 ~ "Moderate",
          TRUE     ~ "High"
        ),
        grade = factor(grade, levels = c("Low", "Moderate", "High"))
      )

    req(nrow(heatmap_df) > 0)

    ggplot(heatmap_df, aes(x = nutrient, y = label, fill = grade)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(
        aes(label = paste0(round(pct), "%"),
            color = grade == "Moderate"),
        size = 3.5, fontface = "bold"
      ) +
      scale_fill_manual(
        values = c("Low" = "#43A047", "Moderate" = "#FB8C00", "High" = "#E53935"),
        name   = NULL,
        labels = c("Low (<25%)", "Moderate (25\u201350%)", "High (>50%)")
      ) +
      scale_color_manual(
        values = c("TRUE" = "grey15", "FALSE" = "white"),
        guide  = "none"
      ) +
      labs(
        title    = "Water quality stoplight by moku",
        subtitle = "% of samples exceeding reef health threshold. Moku with \u22653 analyzed samples shown.",
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid      = element_blank(),
        plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(face = "bold", size = 11),
        legend.position = "bottom",
        legend.key.size = unit(1, "lines")
      )
  }, res = 120)

  # output$plot_hotspot <- renderPlot({
  #   cs <- chem_summary()
  #   req(cs$has_chem)
  #   df      <- cs$df
  #   icolors <- cs$island_colors
  #
  #   hotspot_df <- df |>
  #     filter(!is.na(ahupuaa)) |>
  #     filter(!is.na(NO3NO2) | !is.na(PO4) | !is.na(SiO2) | !is.na(NH3)) |>
  #     mutate(
  #       n_exceeded =
  #         (!is.na(NO3NO2) & NO3NO2 > thresholds["NO3NO2"]) +
  #         (!is.na(PO4)    & PO4    > thresholds["PO4"])    +
  #         (!is.na(SiO2)   & SiO2   > thresholds["SiO2"])   +
  #         (!is.na(NH3)    & NH3    > thresholds["NH3"])
  #     ) |>
  #     group_by(island, ahupuaa) |>
  #     summarise(
  #       n     = n(),
  #       score = mean(n_exceeded, na.rm = TRUE),
  #       .groups = "drop"
  #     ) |>
  #     filter(n >= 3, !is.na(score)) |>
  #     arrange(desc(score)) |>
  #     slice_head(n = 15) |>
  #     mutate(ahupuaa = fct_reorder(ahupuaa, score))
  #
  #   req(nrow(hotspot_df) > 0)
  #
  #   ggplot(hotspot_df, aes(x = score, y = ahupuaa, fill = island)) +
  #     geom_col() +
  #     scale_fill_manual(values = icolors, name = "Island") +
  #     scale_x_continuous(
  #       limits = c(0, 4),
  #       breaks = 0:4,
  #       labels = c("0\n(none)", "1", "2", "3", "4\n(all)")
  #     ) +
  #     labs(
  #       title    = "Top ahupua\u02BBa by pollution burden",
  #       subtitle = "Average number of nutrient thresholds exceeded per sample (max\u00a0=\u00a04). Top 15 shown.",
  #       x        = "Avg. nutrients above threshold",
  #       y        = NULL
  #     ) +
  #     theme_minimal(base_size = 13) +
  #     theme(
  #       panel.grid.major.y = element_blank(),
  #       panel.grid.minor   = element_blank(),
  #       plot.title         = element_text(face = "bold")
  #     )
  # }, res = 120)

  output$plot_burden <- renderPlot({
    cs <- chem_summary()
    req(cs$has_chem)
    df      <- cs$df

    burden_df <- df |>
      filter(!is.na(island)) |>
      filter(!is.na(NO3NO2) | !is.na(PO4) | !is.na(SiO2) | !is.na(NH3)) |>
      mutate(
        n_exceeded =
          (!is.na(NO3NO2) & NO3NO2 > thresholds["NO3NO2"]) +
          (!is.na(PO4)    & PO4    > thresholds["PO4"])    +
          (!is.na(SiO2)   & SiO2   > thresholds["SiO2"])   +
          (!is.na(NH3)    & NH3    > thresholds["NH3"]),
        n_exceeded = factor(
          n_exceeded,
          levels = 0:4,
          labels = c("0 \u2014 None", "1", "2", "3", "4 \u2014 All")
        )
      ) |>
      count(island, n_exceeded) |>
      group_by(island) |>
      mutate(prop = n / sum(n)) |>
      ungroup()

    req(nrow(burden_df) > 0)

    ggplot(burden_df, aes(x = prop, y = island, fill = n_exceeded)) +
      geom_col(position = "stack", width = 0.55) +
      scale_fill_manual(
        values = c(
          "0 \u2014 None" = "#43A047",
          "1"            = "#A5D6A7",
          "2"            = "#FB8C00",
          "3"            = "#E53935",
          "4 \u2014 All"  = "#7B1FA2"
        ),
        name = "Nutrients above\nthreshold"
      ) +
      scale_x_continuous(
        labels = scales::percent,
        expand = expansion(mult = c(0, 0.03))
      ) +
      labs(
        title    = "Pollution burden by island",
        subtitle = "Share of analyzed samples by number of nutrient thresholds simultaneously exceeded",
        x        = NULL,
        y        = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold"),
        legend.position    = "right"
      )
  }, res = 120)

  # ---- Cesspool proximity scatter plot ----------------------------------
  output$plot_cesspool_proximity <- renderPlot({
    df <- live_data()$df

    nutrient_labels <- c(
      NO3NO2 = "Nitrate+Nitrite (\u00b5mol/L)",
      PO4    = "Phosphate (\u00b5mol/L)",
      SiO2   = "Silicate (\u00b5mol/L)",
      NH3    = "Ammonium (\u00b5mol/L)"
    )

    plot_df <- df |>
      filter(
        !is.na(dist_to_nearest_cesspool_m),
        !is.na(island),
        !grepl("L\u0101na", island)    # exclude Lāna'i — no cesspool data
      ) |>
      pivot_longer(c(NO3NO2, PO4, SiO2, NH3),
                   names_to  = "nutrient",
                   values_to = "concentration") |>
      filter(!is.na(concentration)) |>
      mutate(nutrient = factor(nutrient,
               levels = names(nutrient_labels),
               labels = unname(nutrient_labels)))

    req(nrow(plot_df) > 0)

    ggplot(plot_df, aes(x = dist_to_nearest_cesspool_m, y = concentration)) +
      geom_point(alpha = 0.45, size = 1.4) +
      scale_x_log10(labels = scales::label_comma()) +
      scale_y_continuous(
        trans  = "log1p",
        labels = scales::label_number(drop0trailing = TRUE)
      ) +
      facet_wrap(island ~ nutrient, scales = "free", ncol = 4) +
      labs(
        x = "Distance to nearest cesspool (m, log\u2081\u2080 scale)",
        y = "Concentration (\u00b5mol/L, log\u2081p scale)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        strip.text    = element_text(face = "bold"),
        panel.spacing = unit(0.6, "lines")
      )
  }, res = 120)

  # output$plot_cesspool_burden <- renderPlot({
  #   df <- chem_summary()$df
  #   req(chem_summary()$has_chem)
  #
  #   nutrient_labels <- c(
  #     NO3NO2 = "Nitrate+Nitrite (\u00b5mol/L)",
  #     PO4    = "Phosphate (\u00b5mol/L)",
  #     SiO2   = "Silicate (\u00b5mol/L)",
  #     NH3    = "Ammonium (\u00b5mol/L)"
  #   )
  #
  #   cp_join <- cesspool_by_ahupuaa |>
  #     mutate(island_key = normalize_island(island)) |>
  #     select(ahupuaa, island_key, n_cesspools)
  #
  #   summary_df <- df |>
  #     filter(!grepl("L\u0101na", island)) |>
  #     mutate(island_key = normalize_island(island)) |>
  #     inner_join(cp_join, by = c("ahupuaa", "island_key")) |>
  #     pivot_longer(c(NO3NO2, PO4, SiO2, NH3),
  #                  names_to  = "nutrient",
  #                  values_to = "concentration") |>
  #     filter(!is.na(concentration)) |>
  #     group_by(ahupuaa, island, n_cesspools, nutrient) |>
  #     summarise(
  #       mean_conc = mean(concentration),
  #       se_conc   = sd(concentration) / sqrt(n()),
  #       n         = n(),
  #       .groups   = "drop"
  #     ) |>
  #     filter(n >= 3) |>
  #     mutate(nutrient = factor(nutrient,
  #              levels = names(nutrient_labels),
  #              labels = unname(nutrient_labels)))
  #
  #   req(nrow(summary_df) > 0)
  #
  #   label_df <- summary_df |>
  #     group_by(island, nutrient) |>
  #     slice_max(mean_conc, n = 2) |>
  #     ungroup()
  #
  #   ggplot(summary_df, aes(x = n_cesspools, color = island)) +
  #     geom_errorbar(
  #       aes(y = mean_conc, ymin = mean_conc - se_conc, ymax = mean_conc + se_conc),
  #       width = 0, linewidth = 0.8
  #     ) +
  #     geom_point(aes(y = mean_conc, size = n)) +
  #     ggrepel::geom_text_repel(
  #       data = label_df,
  #       aes(y = mean_conc, label = ahupuaa),
  #       size = 3, show.legend = FALSE,
  #       min.segment.length = 0.2, box.padding = 0.4, max.overlaps = 20
  #     ) +
  #     scale_x_log10(labels = scales::label_comma()) +
  #     scale_y_continuous(
  #       trans  = "log1p",
  #       labels = scales::label_number(drop0trailing = TRUE)
  #     ) +
  #     scale_size_continuous(
  #       name   = "Samples\nin ahupua\u02BBa",
  #       range  = c(2, 6),
  #       breaks = c(3, 5, 10, 20)
  #     ) +
  #     ggh4x::facet_grid2(island ~ nutrient, scales = "free", independent = "y") +
  #     labs(
  #       x     = "Total cesspools in ahupua\u02BBa (log10 scale)",
  #       y     = "Mean concentration (\u00b5mol/L, log scale)",
  #       color = "Island"
  #     ) +
  #     theme_minimal(base_size = 12) +
  #     theme(
  #       panel.grid.minor = element_blank(),
  #       strip.text       = element_text(face = "bold"),
  #       legend.position  = "bottom"
  #     )
  # }, res = 120)

  output$plot_cesspool_density <- renderPlot({
    df <- chem_summary()$df
    req(chem_summary()$has_chem)

    nutrient_labels <- c(
      NO3NO2 = "Nitrate+Nitrite (\u00b5mol/L)",
      PO4    = "Phosphate (\u00b5mol/L)",
      SiO2   = "Silicate (\u00b5mol/L)",
      NH3    = "Ammonium (\u00b5mol/L)"
    )

    cp_join <- cesspool_by_ahupuaa |>
      mutate(island_key = normalize_island(island)) |>
      select(ahupuaa, island_key, cesspool_density_per_km2)

    summary_df <- df |>
      filter(!grepl("L\u0101na", island)) |>
      mutate(island_key = normalize_island(island)) |>
      inner_join(cp_join, by = c("ahupuaa", "island_key")) |>
      filter(!is.na(cesspool_density_per_km2)) |>
      pivot_longer(c(NO3NO2, PO4, SiO2, NH3),
                   names_to  = "nutrient",
                   values_to = "concentration") |>
      filter(!is.na(concentration)) |>
      group_by(ahupuaa, island, cesspool_density_per_km2, nutrient) |>
      summarise(
        mean_conc = mean(concentration),
        se_conc   = sd(concentration) / sqrt(n()),
        n         = n(),
        .groups   = "drop"
      ) |>
      filter(n >= 3) |>
      mutate(nutrient = factor(nutrient,
               levels = names(nutrient_labels),
               labels = unname(nutrient_labels)))

    req(nrow(summary_df) > 0)

    label_df <- summary_df |>
      group_by(island, nutrient) |>
      slice_max(mean_conc, n = 2) |>
      ungroup()

    ggplot(summary_df, aes(x = cesspool_density_per_km2, color = island)) +
      geom_errorbar(
        aes(y = mean_conc, ymin = mean_conc - se_conc, ymax = mean_conc + se_conc),
        width = 0, linewidth = 0.8
      ) +
      geom_point(aes(y = mean_conc, size = n)) +
      ggrepel::geom_text_repel(
        data = label_df,
        aes(y = mean_conc, label = ahupuaa),
        size = 3, show.legend = FALSE,
        min.segment.length = 0.2, box.padding = 0.4, max.overlaps = 20
      ) +
      scale_x_log10(labels = scales::label_comma()) +
      scale_y_continuous(
        trans  = "log1p",
        labels = scales::label_number(drop0trailing = TRUE)
      ) +
      scale_size_continuous(
        name   = "Samples\nin ahupua\u02BBa",
        range  = c(2, 6),
        breaks = c(3, 5, 10, 20)
      ) +
      ggh4x::facet_grid2(island ~ nutrient, scales = "free", independent = "y") +
      labs(
        x     = "Cesspool density (per km\u00b2, log10 scale)",
        y     = "Mean concentration (\u00b5mol/L, log scale)",
        color = "Island"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold"),
        legend.position  = "bottom"
      )
  }, res = 120)

}


shinyApp(ui, server)
