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


# ------------------------------------------------------------------
# Load and prepare data
# Data is loaded fresh from Google Sheets each time the app starts.
# For deployment, ensure gs4_auth() credentials are available on the
# server (e.g., via gargle's encrypted token or environment variables).
# ------------------------------------------------------------------

source("01_load_clean_data.R")

# Read chemistry results and full join with sample metadata
chem_data <- read_csv("data/ChemData.csv", show_col_types = FALSE) |>
  rename(sample_id = sample_ID) |>
  mutate(sample_id = toupper(sample_id))

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
  full_join(chem_data, by = "sample_id")

island_choices   <- sort(unique(df_app$island))
tod_choices      <- c("Daytime", "Nighttime")

# Named vector: display label -> ISO date string used for filtering
date_vals    <- sort(unique(df_app$date))
date_choices <- setNames(as.character(date_vals), format(date_vals, "%b %d, %Y"))
total_samples    <- nrow(df_app)

marker_color <- "#f4a261"


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

  /* ---- Parameter section heading ---- */
  .param-section-title {
    font-size: 1.25rem;
    font-weight: 700;
    color: #0077b6;
    border-bottom: 2px solid #0077b6;
    padding-bottom: 8px;
    margin: 28px 0 16px 0;
  }

  /* ---- Mobile responsive ---- */
  @media (max-width: 767px) {

    /* ---- Data page: zoom out so desktop layout fits on phone ---- */
    #data-tab {
      zoom: 0.5;
      overflow-x: hidden;
    }

    /* ---- Data page layout ----
       fillable = TRUE sets overflow:hidden on the sidebar layout, which
       collapses the map to zero height on mobile. Override to scrollable. */
    .bslib-sidebar-layout {
      height: auto !important;
      overflow: visible !important;
    }
    .bslib-sidebar-layout > .main {
      height: auto !important;
      overflow-y: auto !important;
    }

    /* Map card and leaflet output: viewport-relative height */
    #map-card {
      height: auto !important;
    }
    #map {
      height: calc(100svh - 310px) !important;
      min-height: 280px;
    }

    /* Value boxes: tighter on mobile to leave room for the map */
    .bslib-value-box { height: 70px !important; }
    .value-box-title { font-size: 0.65rem !important; }

    /* Timeline: stack vertically on mobile */
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

    /* Navbar: shorter tab labels via abbreviated text trick */
    .navbar-nav .nav-link { font-size: 0.82rem; padding: 0.4rem 0.5rem; }

    /* Funding logos: constrain and stack on small screens */
    .funding-logos { width: 100%; }
    .funding-logos img { max-height: 32px !important; }

    /* Data page: remove horizontal padding so map fills width */
    .bslib-main-col { padding-left: 6px !important; padding-right: 6px !important; }

    /* Prevent value boxes from being too narrow */
    .bslib-value-box { min-width: 140px; }
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

ui <- page_navbar(
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
              choices  = c("Default" = "default", "Salinity (psu)" = "salinity"),
              selected = "default"
            )
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
  # Page 2: What are we measuring?
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
        tags$span(class = "text-muted", "DOI: TBD")
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

)


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
        full_join(chem_data, by = "sample_id")
      list(
        df                 = df_joined,
        samples_analyzed   = samples_analyzed,
        salinity_analyzed  = df_joined |>
          filter(!is.na(Salinity)) |> distinct(sample_id) |> nrow(),
        nutrients_analyzed = nutrients_analyzed
      )
    }, error = function(e) {
      list(
        df                 = df_app,
        samples_analyzed   = 0L,
        salinity_analyzed  = 0L,
        nutrients_analyzed = 0L
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

    if (input$color_by == "salinity") {
      df_sal <- df |> filter(!is.na(Salinity))
      value_box(
        title  = "Salinity Samples Shown out of Total",
        value  = paste(nrow(df_sal), "of", tot),
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
  })

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

  # Update markers when filters change
  observe({
    df <- df_filtered()

    proxy <- leafletProxy("map") |>
      clearMarkers() |>
      clearControls()

    if (nrow(df) == 0) {
      proxy |>
        addControl(
          html = '<div style="background:white;padding:10px 14px;border-radius:6px;
                              border:1px solid #dee2e6;font-size:13px;color:#6c757d;">
                    No samples match the current filters.
                  </div>',
          position = "topright"
        )
      return()
    }

    make_popup <- function(d) {
      salinity_line <- if_else(
        !is.na(d$Salinity),
        paste0("<br>Salinity: ", round(d$Salinity, 2), " psu"),
        "<br>Salinity: <i>Not yet processed</i>"
      )
      ahupuaa_line <- if_else(
        !is.na(d$ahupuaa),
        paste0("<br>Ahupuaʻa: ", d$ahupuaa, " (", d$moku, ")"),
        ""
      )
      paste0(
        "<b>Sample: ", d$sample_id, "</b><br>",
        "Mokupuni: ", d$island, "<br>",
        "Collected: ", format(d$collected_hst, "%b %d, %Y %I:%M %p HST"),
        ahupuaa_line,
        salinity_line,
        if_else(
          !is.na(d$notes) & nchar(trimws(d$notes)) > 0,
          paste0("<br><i>", d$notes, "</i>"),
          ""
        )
      )
    }

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
          position  = "bottomright",
          pal       = pal,
          values    = all_moku,
          title     = "Moku",
          opacity   = 0.85
        )
      return()
    }

    if (input$color_by == "salinity") {
      all_sal   <- live_data()$df$Salinity
      sal_range <- range(all_sal, na.rm = TRUE)
      has_data  <- any(!is.na(all_sal)) && sal_range[1] != sal_range[2]

      if (has_data) {
        df_sal <- df |> filter(!is.na(Salinity))
        pal    <- colorNumeric(palette = "viridis", domain = sal_range)

        proxy |>
          addCircleMarkers(
            data        = df_sal,
            lng         = ~longitude,
            lat         = ~latitude,
            radius      = 7,
            color       = "white",
            fillColor   = ~pal(Salinity),
            fillOpacity = 0.85,
            weight      = 1.5,
            popup       = make_popup(df_sal)
          ) |>
          addLegend(
            position  = "bottomright",
            pal       = pal,
            values    = all_sal[!is.na(all_sal)],
            title     = "Salinity (psu)",
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
}


shinyApp(ui, server)
