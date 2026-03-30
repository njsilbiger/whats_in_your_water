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

df_app <- df_clean |>
  mutate(
    hour        = hour(collected_hst),
    minute      = minute(collected_hst),
    # Classify samples: 6:00 AM–6:45 PM = Daytime, otherwise Nighttime
    time_of_day = if_else(
      (hour * 60 + minute) >= (6 * 60) & (hour * 60 + minute) < (18 * 60 + 45),
      "Daytime", "Nighttime"
    )
  )

island_choices   <- sort(unique(df_app$island))
tod_choices      <- c("Daytime", "Nighttime")

# Marker colors by time of day
tod_colors <- c("Daytime" = "#f4a261", "Nighttime" = "#023e8a")
tod_pal    <- colorFactor(palette = tod_colors, domain = names(tod_colors))


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

  /* ---- Parameter section heading ---- */
  .param-section-title {
    font-size: 1.25rem;
    font-weight: 700;
    color: #0077b6;
    border-bottom: 2px solid #0077b6;
    padding-bottom: 8px;
    margin: 28px 0 16px 0;
  }
"


# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------

ui <- page_navbar(
  title = "What's In Your Water? — Hawaiʻi Ocean Sampling",
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0077b6"
  ),

  tags$style(HTML(app_css)),

  # ================================================================
  # Page 1: Data
  # ================================================================

  nav_panel(
    title = "Data",

    layout_sidebar(
      fillable = TRUE,

      # ---- Sidebar -----------------------------------------------
      sidebar = sidebar(
        width = 310,

        p(
          strong("What's In Your Water?"), "is a community science project",
          "collecting ocean water samples across O'ahu and Maui Nui to",
          "assess coastal water quality after the March 2026 Kona Low storm.",
          "Samples are gathered by volunteer community members and scientists.",
          "We are in the process of analyzing these samples for salinity, nutrients,",
          "and dissolved organic matter — important indicators of coral reef health."
        ),
        p(
          "Use the filters below to explore samples by island and time of",
          "collection. Click any marker on the map for collection details."
        ),

        hr(),

        checkboxGroupInput(
          inputId  = "island",
          label    = "Island",
          choices  = island_choices,
          selected = island_choices
        ),

        hr(),

        checkboxGroupInput(
          inputId  = "time_of_day",
          label    = "Time of Day",
          choices  = tod_choices,
          selected = tod_choices
        )
      ),

      # ---- Main content ------------------------------------------
      layout_column_wrap(
        width = 1 / 4,
        fill  = FALSE,
        heights_equal = "row",
        value_box(
          title = "Samples Shown",
          value = textOutput("n_samples"),
          theme = "primary",
          height = "90px"
        ),
        value_box(
          title = "Islands",
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
          title = "Samplers",
          value = textOutput("n_samplers"),
          showcase = fa("users"),
          theme = "success",
          height = "90px"
        )
      ),

      card(
        full_screen = TRUE,
        card_header("Sample Collection Locations"),
        leafletOutput("map", height = "650px")
      )
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
        "Every sample collected goes through a series of lab analyses to",
        "characterize the water chemistry at each site."
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
          "First batches of ~50 processed. Results arriving soon."
        ),
        make_timeline_step(
          "flask", "Nutrients",
          "Batches Arriving", "soon",
          "Nitrate, phosphate & silicate — run alongside salinity."
        ),
        make_timeline_step(
          "sun", "Organic Matter",
          "Coming Soon", "future",
          "Fluorescent dissolved organic matter (fDOM) via fluorometry."
        ),
        make_timeline_step(
          "skull-crossbones", "Toxins",
          "Select Subsamples", "future",
          "A small subset of samples screened for coastal toxins."
        )
      ),

      # ---- Info banner -------------------------------------------
      div(
        class = "alert alert-info d-flex align-items-start gap-3 mt-2",
        HTML(fa("circle-info", fill = "#0c5460", height = "1.2em")),
        tags$div(
          tags$strong("A note on timelines: "),
          "We received an overwhelming number of samples — thank you! ",
          "Samples are being processed in batches of ~50 and shipped to ",
          "multiple specialized labs. Salinity and nutrient results will come ",
          "in first, followed by dissolved organic matter (fDOM) and any toxin ",
          "screens. We'll update the data map as results arrive."
        )
      ),

      # ---- Section: Parameter descriptions -----------------------
      div(class = "param-section-title", "What are these parameters?"),

      p(
        class = "mb-4",
        "Each measurement tells us something different about how the ",
        "March 2026 Kona Low storm affected coastal water quality — and ",
        "what that means for coral reefs and the communities that depend on them."
      ),

      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",

        make_param_card(
          "droplet", "Salinity",
          "Salinity measures how much dissolved salt is in seawater, typically
           reported in practical salinity units (PSU). Healthy open ocean water
           sits around 34–36 PSU.",
          "Corals evolved in stable-salinity environments. A surge of freshwater
           from heavy rain and runoff can rapidly dilute nearshore seawater,
           causing osmotic stress, bleaching, and in severe cases, mortality
           — even without elevated temperatures.",
          "Salinity is the fastest and most direct signal of freshwater intrusion.
           A sharp drop at a coastal site after a storm almost always indicates
           significant land-based runoff has reached the reef."
        ),

        make_param_card(
          "flask", "Nitrate (NO\u2083\u207B)",
          "Nitrate is the most abundant form of dissolved inorganic nitrogen in
           coastal waters. It is an essential nutrient for primary producers,
           but problems arise when concentrations become elevated.",
          "Excess nitrate fuels the growth of macroalgae and phytoplankton,
           which compete with corals for space and light. Chronic nutrient
           loading is one of the leading stressors on Hawaiʻi's reefs, shifting
           reef communities from coral-dominated to algae-dominated states.",
          "Nitrate is a key tracer of agricultural fertilizer runoff, wastewater
           leakage, and atmospheric nitrogen deposition washed into streams.
           Elevated levels after a storm indicate that terrestrial nitrogen
           sources have reached the coast."
        ),

        make_param_card(
          "flask", "Phosphate (PO\u2084\u00B3\u207B)",
          "Phosphate is dissolved inorganic phosphorus — another essential nutrient
           that, in excess, drives coastal eutrophication alongside nitrogen.",
          "Together with nitrate, elevated phosphate accelerates algal growth on
           reefs. Phosphate can also directly impair coral calcification at high
           concentrations, weakening skeletal structure over time.",
          "Phosphate originates from fertilizers, detergents, soil erosion, and
           septic systems. Its presence alongside elevated nitrate after a storm
           is a strong indicator of mixed agricultural and urban runoff."
        ),

        make_param_card(
          "flask", "Silicate (Si(OH)\u2084)",
          "Silicate (silicic acid) is dissolved silicon derived primarily from the
           weathering of volcanic rock and the erosion of terrestrial soils —
           particularly relevant in Hawaiʻi's geologically young landscape.",
          "Silicate itself is not directly harmful to corals, but elevated silicate
           promotes diatom blooms. Diatoms can shade reef communities and, when
           they die, consume oxygen as they decompose — stressing reef organisms.",
          "Silicate is one of the best geochemical tracers of storm-driven
           terrestrial input. A spike in silicate after a rain event indicates
           that sediment-laden freshwater from stream discharge is reaching
           nearshore reefs."
        ),

        make_param_card(
          "sun", "fDOM (Fluorescent Dissolved Organic Matter)",
          "fDOM refers to the fraction of dissolved organic matter that absorbs
           UV light and re-emits it as fluorescence. It is measured with a
           fluorometer and is a proxy for humic and fulvic acids — organic
           compounds produced by the decomposition of plant material and soil.",
          "fDOM can reduce water clarity by absorbing and scattering light,
           limiting the photosynthetically active radiation available to
           zooxanthellae — the symbiotic algae that power coral growth. Prolonged
           light limitation after storm events can weaken corals already stressed
           by other factors.",
          "fDOM is a sensitive indicator of terrestrial organic matter entering
           coastal waters. It is particularly useful because it responds quickly
           to runoff and can be measured continuously with in-water sensors.
           High fDOM values after a storm suggest a significant pulse of
           soil-derived organic material has washed offshore."
        )
      )
    )
  )
)


# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive filtered data
  df_filtered <- reactive({
    df_app |>
      filter(
        island      %in% input$island,
        time_of_day %in% input$time_of_day
      )
  })

  # Value box outputs
  output$n_samples <- renderText(nrow(df_filtered()))

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
      fitBounds(
        lng1 = min(df_app$longitude) - 0.05,
        lat1 = min(df_app$latitude)  - 0.05,
        lng2 = max(df_app$longitude) + 0.05,
        lat2 = max(df_app$latitude)  + 0.05
      )
  })

  # Update markers when filters change
  observe({
    df <- df_filtered()

    proxy <- leafletProxy("map") |>
      clearMarkers() |>
      clearControls()

    if (nrow(df) == 0) return()

    proxy |>
      addCircleMarkers(
        data        = df,
        lng         = ~longitude,
        lat         = ~latitude,
        radius      = 7,
        color       = "white",
        fillColor   = ~tod_pal(time_of_day),
        fillOpacity = 0.85,
        weight      = 1.5,
        popup       = ~paste0(
          "<b>Sample: ", sample_id, "</b><br>",
          "Island: ", island, "<br>",
          "Collected: ", format(collected_hst, "%b %d, %Y %I:%M %p HST"),
          if_else(
            !is.na(notes) & nchar(trimws(notes)) > 0,
            paste0("<br><i>", notes, "</i>"),
            ""
          )
        )
      ) |>
      addLegend(
        position  = "bottomright",
        pal       = tod_pal,
        values    = df$time_of_day,
        title     = "Time of Day",
        opacity   = 0.9
      )
  })
}


shinyApp(ui, server)
