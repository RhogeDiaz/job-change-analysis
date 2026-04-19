# =============================================================================
# Global Food Security Intelligence Dashboard
# Implements: SDG Tracking | Time-Series Forecasting | Geospatial Analysis
# =============================================================================

# ── Install & load packages ──────────────────────────────────────────────────
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "tidyverse", "plotly", "leaflet", "leaflet.extras",
  "forecast", "zoo", "scales", "DT", "RColorBrewer",
  "fresh"  # for custom themes
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(forecast)
library(zoo)
library(scales)
library(DT)
library(RColorBrewer)
library(fresh)

# =============================================================================
# DATA LOADING & PREPARATION
# =============================================================================

# ── Load CSV (adjust path as needed) ─────────────────────────────────────────
df_raw <- read_csv(
  "global_food_security_intelligence.csv",
  show_col_types = FALSE,
  na = c("", "NA", "nan", "NaN", "N/A")
)

# ── Clean & standardise ───────────────────────────────────────────────────────
df <- df_raw %>%
  mutate(
    region           = str_trim(region),
    income_group     = str_trim(income_group),
    year             = as.integer(year),
    food_crisis_flag = as.integer(food_crisis_flag),
    across(c(hunger_severity_index, fao_undernourishment_pct, undernourishment_pct,
             fao_child_stunting_pct, stunting_prevalence_pct,
             fao_dietary_energy_adequacy_pct, fao_severe_food_insecurity_pct,
             gdp_per_capita_usd, food_production_index,
             latitude, longitude), as.numeric)
  ) %>%
  filter(!is.na(year), !is.na(country_name)) %>%
  mutate(
    undernourishment = coalesce(fao_undernourishment_pct, undernourishment_pct),
    stunting         = coalesce(fao_child_stunting_pct,   stunting_prevalence_pct)
  )

# ── Helper vectors ────────────────────────────────────────────────────────────
all_regions   <- sort(unique(df$region[!is.na(df$region)]))
all_countries <- sort(unique(df$country_name))
year_range    <- range(df$year, na.rm = TRUE)

# SDG-2 indicator labels
sdg_vars <- c(
  "Undernourishment (%)"       = "undernourishment",
  "Child Stunting (%)"         = "stunting",
  "Hunger Severity Index"      = "hunger_severity_index",
  "Dietary Energy Adequacy (%)"= "fao_dietary_energy_adequacy_pct",
  "Severe Food Insecurity (%)" = "fao_severe_food_insecurity_pct"
)

# Forecasting-capable indicators
ts_vars <- c(
  "Undernourishment (%)"  = "undernourishment",
  "Child Stunting (%)"    = "stunting",
  "Hunger Severity Index" = "hunger_severity_index",
  "GDP per Capita (USD)"  = "gdp_per_capita_usd",
  "Food Production Index" = "food_production_index"
)

# Geo indicators
geo_vars <- c(
  "Hunger Severity Index"      = "hunger_severity_index",
  "Undernourishment (%)"       = "undernourishment",
  "Child Stunting (%)"         = "stunting",
  "Food Crisis Flag"            = "food_crisis_flag",
  "Severe Food Insecurity (%)" = "fao_severe_food_insecurity_pct"
)

# =============================================================================
# CUSTOM THEME
# =============================================================================

my_theme <- create_theme(
  adminlte_color(
    light_blue    = "#1a6b4a",
    green         = "#2ecc71",
    olive         = "#27ae60",
    yellow        = "#f39c12"
  ),
  adminlte_sidebar(
    dark_bg       = "#0d1f17",
    dark_hover_bg = "#1a6b4a",
    dark_color    = "#c8e6c9",
    dark_hover_color = "#ffffff"
  ),
  adminlte_global(
    content_bg    = "#f0f4f1",
    box_bg        = "#ffffff",
    info_box_bg   = "#ffffff"
  )
)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "green",

  # ── Header ─────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/FAO_logo.svg/120px-FAO_logo.svg.png",
               height = "28px", style = "margin-right:8px; vertical-align:middle;"),
      "Food Security Intelligence"
    ),
    titleWidth = 300
  ),

  # ── Sidebar ────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar",
      menuItem("🌍 Overview",         tabName = "overview",    icon = icon("globe")),
      menuItem("📈 SDG Tracking",     tabName = "sdg",         icon = icon("chart-line")),
      menuItem("🔮 Forecasting",      tabName = "forecast",    icon = icon("magic")),
      menuItem("🗺️ Geospatial",       tabName = "geo",         icon = icon("map-marked-alt"))
    ),
    tags$hr(style = "border-color:#1a6b4a;"),
    tags$div(
      style = "padding:12px 16px; color:#90c8a0; font-size:11px; line-height:1.6;",
      tags$b("Dataset Info"),
      tags$br(),
      paste0("📅 Years: ", year_range[1], "–", year_range[2]),
      tags$br(),
      paste0("🌐 Countries: ", n_distinct(df$country_name)),
      tags$br(),
      paste0("📊 Records: ", format(nrow(df), big.mark = ","))
    )
  ),

  # ── Body ───────────────────────────────────────────────────────────────────
  dashboardBody(
    use_theme(my_theme),

    # Global CSS
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f0f4f1; }
      .box { border-radius: 10px; box-shadow: 0 2px 12px rgba(0,0,0,0.08); }
      .info-box { border-radius: 10px; }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #1a6b4a; }
      .selectize-input { border-radius: 6px !important; }
      h3.box-title { font-weight: 700; color: #0d3d26; }
      .section-title {
        font-size: 13px; font-weight: 700; color: #1a6b4a;
        text-transform: uppercase; letter-spacing: 1px;
        margin: 4px 0 10px; padding-bottom: 6px;
        border-bottom: 2px solid #c8e6c9;
      }
    "))),

    tabItems(

      # ════════════════════════════════════════════════════════════════════════
      # TAB 1 — OVERVIEW
      # ════════════════════════════════════════════════════════════════════════
      tabItem(
        tabName = "overview",

        fluidRow(
          infoBoxOutput("kpi_countries",  width = 3),
          infoBoxOutput("kpi_crisis",     width = 3),
          infoBoxOutput("kpi_hunger",     width = 3),
          infoBoxOutput("kpi_stunting",   width = 3)
        ),

        fluidRow(
          box(
            title = "Global Hunger Severity — Latest Available Year",
            width = 8, solidHeader = TRUE, status = "success",
            plotlyOutput("overview_bar", height = "350px")
          ),
          box(
            title = "Food Crisis Countries by Region",
            width = 4, solidHeader = TRUE, status = "warning",
            plotlyOutput("overview_pie", height = "350px")
          )
        ),

        fluidRow(
          box(
            title = "Undernourishment Trend — Global Average",
            width = 12, solidHeader = TRUE, status = "success",
            plotlyOutput("overview_trend", height = "280px")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 2 — SDG TRACKING
      # ════════════════════════════════════════════════════════════════════════
      tabItem(
        tabName = "sdg",

        fluidRow(
          box(
            width = 3, solidHeader = FALSE,
            tags$div(class = "section-title", "SDG-2 Controls"),

            pickerInput("sdg_indicator", "Indicator",
              choices  = sdg_vars,
              selected = "undernourishment",
              options  = list(`live-search` = TRUE)
            ),
            pickerInput("sdg_regions", "Regions",
              choices  = all_regions,
              selected = all_regions,
              multiple = TRUE,
              options  = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 2"
              )
            ),
            sliderInput("sdg_years", "Year Range",
              min = year_range[1], max = year_range[2],
              value = c(year_range[1], year_range[2]),
              step = 1, sep = ""
            ),
            radioGroupButtons("sdg_agg", "Aggregation",
              choices  = c("Mean" = "mean", "Median" = "median"),
              selected = "mean",
              justified = TRUE, size = "sm"
            ),
            checkboxInput("sdg_ribbon", "Show uncertainty band", TRUE)
          ),

          box(
            width = 9, solidHeader = TRUE, status = "success",
            title = "SDG-2 Progress by Region",
            plotlyOutput("sdg_trend_plot", height = "420px")
          )
        ),

        fluidRow(
          box(
            title = "Income Group Comparison — Latest Year",
            width = 6, solidHeader = TRUE, status = "success",
            plotlyOutput("sdg_income_plot", height = "320px")
          ),
          box(
            title = "Top 15 Most Affected Countries — Latest Year",
            width = 6, solidHeader = TRUE, status = "warning",
            plotlyOutput("sdg_top_countries", height = "320px")
          )
        ),

        fluidRow(
          box(
            title = "SDG-2 Data Table",
            width = 12, solidHeader = FALSE,
            DTOutput("sdg_table")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 3 — FORECASTING
      # ════════════════════════════════════════════════════════════════════════
      tabItem(
        tabName = "forecast",

        fluidRow(
          box(
            width = 3, solidHeader = FALSE,
            tags$div(class = "section-title", "Forecast Controls"),

            pickerInput("fc_country", "Country",
              choices  = all_countries,
              selected = "Ethiopia",
              options  = list(`live-search` = TRUE)
            ),
            pickerInput("fc_indicator", "Indicator",
              choices  = ts_vars,
              selected = "undernourishment"
            ),
            sliderInput("fc_horizon", "Forecast Horizon (years)",
              min = 1, max = 10, value = 5, step = 1
            ),
            radioGroupButtons("fc_model", "Model",
              choices = c("ETS" = "ets", "ARIMA" = "arima", "Holt" = "holt"),
              selected = "ets", justified = TRUE, size = "sm"
            ),
            tags$hr(),
            tags$div(
              class = "section-title", "Compare Country"
            ),
            pickerInput("fc_compare", "Add Comparison Country (optional)",
              choices  = c("None", all_countries),
              selected = "None",
              options  = list(`live-search` = TRUE)
            )
          ),

          box(
            width = 9, solidHeader = TRUE, status = "success",
            title = "Time-Series Forecast",
            plotlyOutput("fc_plot", height = "440px")
          )
        ),

        fluidRow(
          box(
            title = "Model Accuracy Metrics",
            width = 4, solidHeader = TRUE, status = "success",
            tableOutput("fc_accuracy")
          ),
          box(
            title = "Regional Average Forecast",
            width = 8, solidHeader = TRUE, status = "success",
            plotlyOutput("fc_region_plot", height = "280px")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 4 — GEOSPATIAL
      # ════════════════════════════════════════════════════════════════════════
      tabItem(
        tabName = "geo",

        fluidRow(
          column(3,
            box(
              width = 12, solidHeader = FALSE,
              tags$div(class = "section-title", "Map Controls"),

              pickerInput("geo_indicator", "Indicator",
                choices  = geo_vars,
                selected = "hunger_severity_index"
              ),
              sliderInput("geo_year", "Year",
                min = year_range[1], max = year_range[2],
                value = year_range[2],
                step = 1, sep = "",
                animate = animationOptions(interval = 800, loop = FALSE)
              ),
              pickerInput("geo_regions_filter", "Filter Regions",
                choices  = all_regions,
                selected = all_regions,
                multiple = TRUE,
                options  = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = "count > 2"
                )
              ),
              radioGroupButtons("geo_type", "Map Style",
                choices = c("Bubbles" = "bubble", "Heatmap" = "heat"),
                selected = "bubble", justified = TRUE, size = "sm"
              ),
              checkboxInput("geo_crisis", "Highlight Food Crisis Countries", TRUE),
              tags$hr(),
              tags$div(class = "section-title", "Summary"),
              uiOutput("geo_summary_box")
            )
          ),
          column(9,
            box(
              width = 12, solidHeader = TRUE, status = "success",
              title = "Interactive World Map",
              leafletOutput("geo_map", height = "520px")
            )
          )
        ),

        fluidRow(
          box(
            title = "Regional Distribution — Selected Year",
            width = 6, solidHeader = TRUE, status = "success",
            plotlyOutput("geo_violin", height = "300px")
          ),
          box(
            title = "Top 20 Countries Table",
            width = 6, solidHeader = TRUE, status = "warning",
            DTOutput("geo_table", height = "300px")
          )
        )
      )
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ── Colour palette helper ──────────────────────────────────────────────────
  region_colors <- setNames(
    RColorBrewer::brewer.pal(max(3, length(all_regions)), "Set2")[seq_along(all_regions)],
    all_regions
  )

  # ============================================================================
  # KPI BOXES
  # ============================================================================

  latest_year <- reactive({
    max(df$year[!is.na(df$undernourishment)], na.rm = TRUE)
  })

  output$kpi_countries <- renderInfoBox({
    infoBox("Countries Monitored", n_distinct(df$country_name),
            icon = icon("flag"), color = "green", fill = TRUE)
  })

  output$kpi_crisis <- renderInfoBox({
    n <- df %>%
      filter(year == latest_year(), !is.na(food_crisis_flag) & food_crisis_flag == 1) %>%
      pull(country_name) %>% n_distinct()
    infoBox("Food Crisis Countries", n,
            icon = icon("exclamation-triangle"), color = "orange", fill = TRUE)
  })

  output$kpi_hunger <- renderInfoBox({
    val <- df %>%
      filter(year == latest_year()) %>%
      summarise(m = round(mean(undernourishment, na.rm = TRUE), 1)) %>%
      pull(m)
    infoBox("Avg Undernourishment", paste0(val, "%"),
            icon = icon("utensils"), color = "red", fill = TRUE)
  })

  output$kpi_stunting <- renderInfoBox({
    val <- df %>%
      filter(year == latest_year()) %>%
      summarise(m = round(mean(stunting, na.rm = TRUE), 1)) %>%
      pull(m)
    infoBox("Avg Child Stunting", paste0(val, "%"),
            icon = icon("child"), color = "yellow", fill = TRUE)
  })

  # ============================================================================
  # OVERVIEW PLOTS
  # ============================================================================

  output$overview_bar <- renderPlotly({
    d <- df %>%
      filter(year == latest_year()) %>%
      group_by(region) %>%
      summarise(hunger = mean(hunger_severity_index, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(region)) %>%
      arrange(desc(hunger))

    plot_ly(d, x = ~reorder(region, hunger), y = ~round(hunger, 3),
            type = "bar",
            marker = list(
              color = ~hunger,
              colorscale = list(c(0,"#c8e6c9"), c(1,"#b71c1c")),
              showscale = TRUE,
              colorbar = list(title = "Index")
            ),
            text = ~round(hunger, 3), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Hunger Index: %{y:.3f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", tickangle = -30),
        yaxis = list(title = "Hunger Severity Index"),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(b = 100)
      )
  })

  output$overview_pie <- renderPlotly({
    d <- df %>%
      filter(year == latest_year(), !is.na(food_crisis_flag) & food_crisis_flag == 1, !is.na(region)) %>%
      count(region)

    plot_ly(d, labels = ~region, values = ~n, type = "pie",
            marker = list(colors = RColorBrewer::brewer.pal(max(3,nrow(d)), "Set3")),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>Countries: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })

  output$overview_trend <- renderPlotly({
    d <- df %>%
      group_by(year) %>%
      summarise(
        mean_hunger = mean(undernourishment, na.rm = TRUE),
        sd_hunger   = sd(undernourishment,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(mean_hunger))

    plot_ly(d) %>%
      add_ribbons(x = ~year,
                  ymin = ~pmax(0, mean_hunger - sd_hunger),
                  ymax = ~mean_hunger + sd_hunger,
                  fillcolor = "rgba(46,204,113,0.15)",
                  line = list(color = "transparent"),
                  name = "±1 SD") %>%
      add_lines(x = ~year, y = ~round(mean_hunger, 2),
                line = list(color = "#1a6b4a", width = 3),
                name = "Global Mean",
                hovertemplate = "Year: %{x}<br>Undernourishment: %{y:.1f}%<extra></extra>") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Undernourishment (%)"),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        legend = list(orientation = "h", x = 0, y = 1.1)
      )
  })

  # ============================================================================
  # SDG TRACKING
  # ============================================================================

  sdg_data <- reactive({
    req(input$sdg_years, input$sdg_regions, input$sdg_indicator)
    agg_fn <- if (input$sdg_agg == "mean") mean else median

    df %>%
      filter(
        year    >= input$sdg_years[1],
        year    <= input$sdg_years[2],
        region  %in% input$sdg_regions
      ) %>%
      group_by(year, region) %>%
      summarise(
        value = agg_fn(.data[[input$sdg_indicator]], na.rm = TRUE),
        sd    = sd(.data[[input$sdg_indicator]],    na.rm = TRUE),
        n     = sum(!is.na(.data[[input$sdg_indicator]])),
        .groups = "drop"
      ) %>%
      filter(!is.na(value))
  })

  output$sdg_trend_plot <- renderPlotly({
    d <- sdg_data()
    req(nrow(d) > 0)

    ind_label <- names(sdg_vars)[sdg_vars == input$sdg_indicator]
    p <- plot_ly()

    for (reg in unique(d$region)) {
      sub <- filter(d, region == reg)
      col <- region_colors[reg]
      if (isTRUE(input$sdg_ribbon) && any(!is.na(sub$sd))) {
        p <- p %>%
          add_ribbons(data = sub, x = ~year,
                      ymin = ~pmax(0, value - sd),
                      ymax = ~value + sd,
                      fillcolor = paste0(col, "22"),
                      line = list(color = "transparent"),
                      showlegend = FALSE, hoverinfo = "skip")
      }
      p <- p %>%
        add_lines(data = sub, x = ~year, y = ~round(value, 2),
                  name = reg,
                  line = list(color = col, width = 2.5),
                  hovertemplate = paste0("<b>", reg, "</b><br>Year: %{x}<br>",
                                         ind_label, ": %{y:.2f}<extra></extra>"))
    }
    p %>% layout(
      xaxis  = list(title = "Year"),
      yaxis  = list(title = ind_label),
      legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 11)),
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    )
  })

  output$sdg_income_plot <- renderPlotly({
    d <- df %>%
      filter(year == latest_year(), !is.na(income_group)) %>%
      group_by(income_group) %>%
      summarise(value = mean(.data[[input$sdg_indicator]], na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(value))

    ind_label <- names(sdg_vars)[sdg_vars == input$sdg_indicator]

    plot_ly(d, x = ~reorder(income_group, value), y = ~round(value, 2),
            type = "bar",
            marker = list(color = c("#81c784","#388e3c","#1b5e20","#ff8f00")[seq_len(nrow(d))]),
            text = ~round(value, 2), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{y:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", tickangle = -20),
        yaxis = list(title = ind_label),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })

  output$sdg_top_countries <- renderPlotly({
    d <- df %>%
      filter(year == latest_year()) %>%
      select(country_name, region, val = all_of(input$sdg_indicator)) %>%
      filter(!is.na(val)) %>%
      slice_max(val, n = 15)

    ind_label <- names(sdg_vars)[sdg_vars == input$sdg_indicator]

    plot_ly(d, x = ~round(val, 2), y = ~reorder(country_name, val),
            type = "bar", orientation = "h",
            marker = list(
              color = ~val,
              colorscale = list(c(0,"#ffeb3b"), c(0.5,"#ff5722"), c(1,"#b71c1c"))
            ),
            hovertemplate = "<b>%{y}</b><br>%{x:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(title = ind_label),
        yaxis = list(title = ""),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })

  output$sdg_table <- renderDT({
    d <- sdg_data() %>%
      mutate(value = round(value, 3), sd = round(sd, 3)) %>%
      rename(Year = year, Region = region, Value = value, SD = sd, N = n)
    datatable(d,
              options  = list(pageLength = 8, scrollX = TRUE),
              rownames = FALSE,
              filter   = "top") %>%
      formatStyle("Value",
                  background = styleColorBar(range(d$Value, na.rm = TRUE), "#c8e6c9"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ============================================================================
  # TIME-SERIES FORECASTING
  # ============================================================================

  fc_ts_data <- reactive({
    req(input$fc_country, input$fc_indicator)
    df %>%
      filter(country_name == input$fc_country, !is.na(.data[[input$fc_indicator]])) %>%
      arrange(year) %>%
      select(year, value = all_of(input$fc_indicator))
  })

  fc_model_fit <- reactive({
    d <- fc_ts_data()
    req(nrow(d) >= 5)
    ts_obj <- ts(d$value, start = min(d$year), frequency = 1)
    model <- switch(input$fc_model,
      "ets"   = ets(ts_obj),
      "arima" = auto.arima(ts_obj, seasonal = FALSE),
      "holt"  = holt(ts_obj, h = input$fc_horizon)$model
    )
    list(ts = ts_obj, model = model, data = d)
  })

  output$fc_plot <- renderPlotly({
    fit <- fc_model_fit()
    req(!is.null(fit))

    h   <- input$fc_horizon
    fct <- switch(input$fc_model,
      "ets"   = forecast(fit$model, h = h),
      "arima" = forecast(fit$model, h = h),
      "holt"  = holt(fit$ts, h = h)
    )

    hist_df <- fit$data
    fc_years <- seq(max(hist_df$year) + 1, max(hist_df$year) + h)

    fc_df <- tibble(
      year  = fc_years,
      mean  = as.numeric(fct$mean),
      lo80  = as.numeric(fct$lower[,1]),
      hi80  = as.numeric(fct$upper[,1]),
      lo95  = as.numeric(fct$lower[,2]),
      hi95  = as.numeric(fct$upper[,2])
    )

    ind_label <- names(ts_vars)[ts_vars == input$fc_indicator]

    p <- plot_ly() %>%
      add_ribbons(data = fc_df, x = ~year, ymin = ~lo95, ymax = ~hi95,
                  fillcolor = "rgba(46,204,113,0.12)",
                  line = list(color = "transparent"), name = "95% CI") %>%
      add_ribbons(data = fc_df, x = ~year, ymin = ~lo80, ymax = ~hi80,
                  fillcolor = "rgba(46,204,113,0.25)",
                  line = list(color = "transparent"), name = "80% CI") %>%
      add_lines(data = hist_df, x = ~year, y = ~value,
                line = list(color = "#1a6b4a", width = 2.5),
                name = paste(input$fc_country, "(historical)"),
                hovertemplate = "Year: %{x}<br>Actual: %{y:.2f}<extra></extra>") %>%
      add_lines(data = fc_df, x = ~year, y = ~round(mean, 2),
                line = list(color = "#e74c3c", width = 2.5, dash = "dash"),
                name = paste0("Forecast (", toupper(input$fc_model), ")"),
                hovertemplate = "Year: %{x}<br>Forecast: %{y:.2f}<extra></extra>")

    # Optional comparison country
    if (!is.null(input$fc_compare) && input$fc_compare != "None") {
      d2 <- df %>%
        filter(country_name == input$fc_compare, !is.na(.data[[input$fc_indicator]])) %>%
        arrange(year) %>%
        select(year, value = all_of(input$fc_indicator))
      if (nrow(d2) > 0) {
        p <- p %>%
          add_lines(data = d2, x = ~year, y = ~value,
                    line = list(color = "#9b59b6", width = 2, dash = "dot"),
                    name = paste(input$fc_compare, "(historical)"),
                    hovertemplate = "Year: %{x}<br>Value: %{y:.2f}<extra></extra>")
      }
    }

    p %>% layout(
      xaxis  = list(title = "Year"),
      yaxis  = list(title = ind_label),
      shapes = list(list(type="line", x0=max(hist_df$year)+0.5,
                         x1=max(hist_df$year)+0.5,
                         y0=0, y1=1, yref="paper",
                         line=list(color="#bbb", dash="dash"))),
      legend = list(orientation = "h", x = 0, y = -0.25),
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    )
  })

  output$fc_accuracy <- renderTable({
    fit <- fc_model_fit()
    req(!is.null(fit))
    acc <- accuracy(fit$model)
    as_tibble(acc, rownames = "Set") %>%
      select(Set, RMSE, MAE, MAPE) %>%
      mutate(across(where(is.numeric), ~round(.x, 3)))
  }, striped = TRUE, hover = TRUE)

  output$fc_region_plot <- renderPlotly({
    req(input$fc_country)
    country_region <- df %>%
      filter(country_name == input$fc_country) %>%
      pull(region) %>% first()

    if (is.na(country_region)) return(NULL)

    d <- df %>%
      filter(region == country_region) %>%
      group_by(year) %>%
      summarise(value = mean(.data[[input$fc_indicator]], na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(value))

    ind_label <- names(ts_vars)[ts_vars == input$fc_indicator]

    plot_ly(d, x = ~year, y = ~round(value, 2), type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 2),
            marker = list(color = "#3498db", size = 5),
            hovertemplate = "Year: %{x}<br>Regional Avg: %{y:.2f}<extra></extra>",
            name = paste(country_region, "avg")) %>%
      layout(
        title  = list(text = paste("Regional Average —", country_region), font = list(size = 13)),
        xaxis  = list(title = "Year"),
        yaxis  = list(title = ind_label),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })

  # ============================================================================
  # GEOSPATIAL
  # ============================================================================

  geo_data <- reactive({
    req(input$geo_year, input$geo_indicator)
    d <- df %>%
      filter(
        year   == input$geo_year,
        region %in% input$geo_regions_filter,
        !is.na(latitude), !is.na(longitude)
      ) %>%
      select(country_name, region, income_group,
             latitude, longitude, food_crisis_flag,
             value = all_of(input$geo_indicator)) %>%
      filter(!is.na(value))
    d
  })

  output$geo_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Terrain") %>%
      addLayersControl(
        baseGroups = c("Light", "Terrain"),
        options    = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = 10, lat = 15, zoom = 2)
  })

  observe({
    d   <- geo_data()
    req(nrow(d) > 0)
    ind <- input$geo_indicator

    pal <- colorNumeric(
      palette = c("#c8e6c9","#66bb6a","#f9a825","#e65100","#b71c1c"),
      domain  = d$value, na.color = "grey80"
    )

    proxy <- leafletProxy("geo_map", data = d) %>%
      clearMarkers() %>%
      clearHeatmap() %>%
      clearControls()

    if (input$geo_type == "bubble") {
      radius_scale <- function(x) {
        rng <- range(x, na.rm = TRUE)
        if (diff(rng) == 0) return(rep(8, length(x)))
        scales::rescale(x, to = c(4, 22), from = rng)
      }

      proxy <- proxy %>%
        addCircleMarkers(
          lng     = ~longitude, lat = ~latitude,
          radius  = ~radius_scale(value),
          color   = ~pal(value), fillOpacity = 0.75, weight = 1,
          stroke  = TRUE, fillColor = ~pal(value),
          popup   = ~paste0(
            "<b>", country_name, "</b><br>",
            "Region: ", region, "<br>",
            names(geo_vars)[geo_vars == ind], ": <b>", round(value, 2), "</b>",
            ifelse(!is.na(food_crisis_flag) & food_crisis_flag == 1,
              "<br><span style='color:red;font-weight:bold;'>&#9888; Food Crisis</span>", "")
          )
        )
    } else {
      proxy <- proxy %>%
        addHeatmap(
          lng = ~longitude, lat = ~latitude,
          intensity = ~value, blur = 30, max = max(d$value, na.rm = TRUE),
          radius = 20,
          gradient = c("0" = "#c8e6c9", "0.5" = "#f9a825", "1" = "#b71c1c")
        )
    }

    if (isTRUE(input$geo_crisis)) {
      crisis_pts <- d %>% filter(!is.na(food_crisis_flag) & food_crisis_flag == 1)
      if (nrow(crisis_pts) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data       = crisis_pts,
            lng        = ~longitude, lat = ~latitude,
            radius     = 10, color = "#c0392b",
            fillColor  = "transparent",
            fillOpacity = 0, weight = 2, dashArray = "4",
            group      = "Crisis Border"
          )
      }
    }

    proxy %>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = names(geo_vars)[geo_vars == ind],
                opacity = 0.85)
  })

  output$geo_summary_box <- renderUI({
    d <- geo_data()
    req(nrow(d) > 0)
    n_crisis <- sum(!is.na(d$food_crisis_flag) & d$food_crisis_flag == 1)
    tags$div(
      tags$p(style = "font-size:12px; margin:2px 0;",
             tags$b("Countries shown: "), nrow(d)),
      tags$p(style = "font-size:12px; margin:2px 0;",
             tags$b("Avg value: "), round(mean(d$value, na.rm = TRUE), 2)),
      tags$p(style = "font-size:12px; margin:2px 0;",
             tags$b("Max value: "), round(max(d$value, na.rm = TRUE), 2)),
      tags$p(style = "font-size:12px; margin:2px 0; color:#c0392b;",
             tags$b("Crisis countries: "), n_crisis)
    )
  })

  output$geo_violin <- renderPlotly({
    d <- geo_data()
    req(nrow(d) > 0)
    ind_label <- names(geo_vars)[geo_vars == input$geo_indicator]

    plot_ly(d, x = ~region, y = ~value, type = "violin",
            split = ~region,
            box      = list(visible = TRUE),
            meanline = list(visible = TRUE),
            colors   = "Set2",
            hovertemplate = "<b>%{x}</b><br>%{y:.2f}<extra></extra>") %>%
      layout(
        xaxis      = list(title = "", tickangle = -25),
        yaxis      = list(title = ind_label),
        showlegend = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(b = 90)
      )
  })

  output$geo_table <- renderDT({
    d <- geo_data() %>%
      arrange(desc(value)) %>%
      slice_head(n = 20) %>%
      select(Country = country_name, Region = region,
             "Income Group" = income_group,
             Value = value,
             Crisis = food_crisis_flag) %>%
      mutate(Value = round(Value, 3),
             Crisis = ifelse(Crisis == 1, "⚠ Yes", "No"))

    datatable(d,
              options  = list(pageLength = 8, dom = "tp"),
              rownames = FALSE) %>%
      formatStyle("Crisis",
                  color = styleEqual("⚠ Yes", "red"),
                  fontWeight = styleEqual("⚠ Yes", "bold"))
  })

}

# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)