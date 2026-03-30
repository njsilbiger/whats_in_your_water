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
total_samples    <- nrow(df_app)

# Update this number as lab results come in
samples_analyzed <- 0

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

      # ---- Progress indicator ------------------------------------
      {
        pct        <- round(samples_analyzed / total_samples * 100)
        bar_color  <- if (pct == 0) "bg-secondary" else "bg-primary"
        bar_width  <- paste0(max(pct, 2), "%")   # keep bar visible at 0%

        div(
          class = "progress-section",
          div(
            class = "progress-label",
            tags$span(
              class = "big-count",
              samples_analyzed,
              tags$span(
                class = "total-count",
                paste0(" of ~", total_samples, " samples analyzed")
              )
            ),
            tags$span(class = "total-count", paste0(pct, "% complete"))
          ),
          tags$div(
            class = "progress",
            role  = "progressbar",
            `aria-valuenow`  = pct,
            `aria-valuemin`  = 0,
            `aria-valuemax`  = 100,
            tags$div(
              class = paste("progress-bar", bar_color),
              style = paste0("width: ", bar_width, ";")
            )
          ),
          tags$small(
            class = "text-muted mt-2 d-block",
            HTML(fa("rotate", height = "0.85em")),
            " Results are added to the map as each batch comes back from the lab."
          )
        )
      },

      # ---- Info banner -------------------------------------------
      div(
        class = "alert alert-info d-flex align-items-start gap-3 mt-2",
        HTML(fa("circle-info", fill = "#0c5460", height = "1.2em")),
        tags$div(
          tags$strong("A note on timelines: "),
          "We received an overwhelming number of samples — thank you! ",
          "Samples are being processed in batches of ~50 and sent to ",
          "multiple specialized labs. Salinity and nutrient results will come ",
          "in first, followed by dissolved organic matter (fDOM) and any toxin ",
          "screens. We'll update the data map as results arrive."
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
        width = 1 / 2,
        heights_equal = "row",

        make_param_card(
          "droplet", "Salinity",
          "Salinity tells us how salty the water is. Ocean water has a natural
           saltiness — think of it like a recipe that ocean animals have adapted
           to over millions of years. A big storm can throw that recipe off by
           dumping huge amounts of fresh rainwater into the ocean.",
          "Corals are built for salty ocean water. When a storm washes a lot of
           fresh water off the land, it dilutes the ocean near shore — like watering
           down a sports drink. That sudden change shocks the corals and can cause
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
          "fDOM stands for fluorescent dissolved organic matter — a mouthful, but
           the idea is simple. It measures the murky, brownish stuff that
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
          title = "Can I still get involved?",
          icon  = HTML(fa("hand", fill = "#0077b6", height = "1em")),
          p(
            "The sample collection window for the Kona Low event has closed,
            but there are still ways to help! Share this dashboard with your
            friends, family, and teachers. Follow along as results come in.
            And if you're interested in future sampling events or reef
            monitoring in your area, reach out — community scientists are
            always needed."
          )
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
