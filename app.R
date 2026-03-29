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
    # Classify samples: 6 AM–6 PM = Daytime, otherwise Nighttime
    time_of_day = if_else(hour >= 6 & hour < 18, "Daytime", "Nighttime")
  )

island_choices   <- sort(unique(df_app$island))
tod_choices      <- c("Daytime", "Nighttime")

# Marker colors by time of day
tod_colors <- c("Daytime" = "#f4a261", "Nighttime" = "#023e8a")
tod_pal    <- colorFactor(palette = tod_colors, domain = names(tod_colors))


# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------

ui <- page_sidebar(
  title = "What's In Your Water? — Hawaiʻi Ocean Sampling",
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    primary   = "#0077b6"
  ),
  fillable = TRUE,

  # ---- Sidebar -----------------------------------------------
  sidebar = sidebar(
    width = 310,

    # Project description
    p(
      strong("What's In Your Water?"), "is a community science project",
      "collecting ocean water samples across O'ahu and Maui Nui to",
      "assessed coastal water quality after the March 2026 Kona Low storm.",
      "Samples are gathered by volunteer community members and scientists.",
      "We are in the process of analyzing these samples for salinity, nutrients,",
      "and dissolved organic matter - important indicators of coral reef health."
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
  tags$style(HTML("
    .value-box-value { font-size: 1rem !important; }
    .value-box-title { font-size: 0.75rem !important; }
  ")),
  layout_column_wrap(
    width = 1 / 4,
    fill  = FALSE,
    heights_equal = "row",
    value_box(
      title = "Samples Shown",
      value = textOutput("n_samples"),
      showcase = fa("droplet"),
      theme = "primary",
      height = "90px"
    ),
    value_box(
      title = "Islands",
      value = textOutput("n_islands"),
      showcase = fa("location-dot"),
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
