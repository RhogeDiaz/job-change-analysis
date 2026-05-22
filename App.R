library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(tidyverse)
library(bsicons)
library(workflows)

# ==============================================================
# ★  COLOR THEME  — edit these hex values to restyle the app  ★
# ==============================================================
CLR_PRIMARY       <- "#0D9488"   # teal-600  — sidebar bg, primary buttons, card headers
CLR_PRIMARY_DK    <- "#0F766E"   # teal-700  — hover / pressed state
CLR_PRIMARY_LT    <- "#CCFBF1"   # teal-100  — light tints
CLR_SECONDARY     <- "#0EA5E9"   # sky-500   — 2nd accent color
CLR_ACCENT_AMBER  <- "#F59E0B"   # amber-500 — KPI card 3 color
CLR_ACCENT_VIOLET <- "#8B5CF6"   # violet-500— KPI card 4 color
CLR_SIDEBAR_TEXT  <- "#F0FDFA"   # near-white text on teal sidebar
CLR_CHART_LOOK    <- "#0D9488"   # chart bar / line: "Looking"
CLR_CHART_NOT     <- "#94A3B8"   # chart bar / line: "Not Looking"
CLR_HEAT_LO       <- "#CCFBF1"   # heatmap: low-density cell
CLR_HEAT_HI       <- "#0F766E"   # heatmap: high-density cell
# ==============================================================

dataset <- "https://raw.githubusercontent.com/RhogeDiaz/job-change-analysis/refs/heads/main/clean_hr_dataset.csv"
model   <- readRDS("../job-change-analysis/job_change_predictor.rds")

df       <- read_csv(dataset, show_col_types = FALSE)
df_train <- df %>% filter(split == "train")

# ── KPI values (computed once at startup) ──────────────────────────────────
n_total      <- nrow(df_train)
n_looking    <- sum(df_train$job_change_label == "Looking",    na.rm = TRUE)
n_not        <- sum(df_train$job_change_label == "Not Looking", na.rm = TRUE)
pct_looking  <- paste0(round(n_looking / n_total * 100, 1), "%")
avg_training <- paste0(round(mean(df_train$training_hours, na.rm = TRUE), 1), " hrs")

# ── helper mappers ─────────────────────────────────────────────────────────
map_experience <- function(val) {
  if (is.null(val) || is.na(val) || !is.numeric(val)) return("Unknown")
  if (val < 1)  return("<1")
  if (val > 20) return(">20")
  as.character(as.integer(val))
}
map_lnj <- function(val) {
  if (is.null(val) || is.na(val) || !is.numeric(val)) return(0)
  if (val < 1) return(0)
  if (val > 5) return(5)
  val
}

# ── custom CSS ─────────────────────────────────────────────────────────────
custom_css <- sprintf("
/* ── sidebar nav styling ───────────────────────────── */
.bslib-sidebar-layout > .sidebar {
  background-color: %s !important;
  border-right: none !important;
}
.sidebar .nav-link {
  color: %s !important;
  border-radius: 8px;
  margin-bottom: 4px;
  font-weight: 500;
  transition: background 0.18s, padding-left 0.18s;
}
.sidebar .nav-link:hover,
.sidebar .nav-link.active {
  background-color: rgba(255,255,255,0.18) !important;
  color: #ffffff !important;
  padding-left: 1.3rem !important;
}
.sidebar .nav-link.active {
  background-color: rgba(255,255,255,0.25) !important;
  font-weight: 700;
}
/* sidebar brand */
.sidebar-brand {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 14px 16px 20px;
  font-size: 1.15rem;
  font-weight: 700;
  color: #ffffff;
  letter-spacing: 0.3px;
  border-bottom: 1px solid rgba(255,255,255,0.2);
  margin-bottom: 12px;
}
/* dark-mode toggle row in sidebar */
.sidebar-bottom {
  margin-top: auto;
  padding-top: 16px;
  border-top: 1px solid rgba(255,255,255,0.18);
  display: flex;
  align-items: center;
  gap: 10px;
  color: %s;
  font-size: 0.85rem;
}

/* ── value / KPI boxes ─────────────────────────────── */
.value-box .value-box-showcase .bi {
  font-size: 2rem;
}

/* ── chart card headers ────────────────────────────── */
.card > .card-header {
  font-weight: 600;
  font-size: 0.92rem;
  letter-spacing: 0.2px;
  border-bottom: 2px solid %s;
  padding: 10px 16px;
}

/* ── filter card ───────────────────────────────────── */
.filter-card {
  border-left: 4px solid %s;
  border-radius: 8px;
  margin-bottom: 16px;
}

/* ── dark-mode overrides ───────────────────────────── */
[data-bs-theme='dark'] .card-header {
  border-bottom-color: %s !important;
}
[data-bs-theme='dark'] .sidebar-brand { color: #f0fdfa; }

/* ── predictive form polish ────────────────────────── */
.predict-sidebar {
  background: var(--bs-body-bg);
  border: 1px solid var(--bs-border-color);
  border-radius: 12px;
  padding: 20px;
}
.predict-result {
  border-left: 5px solid %s;
  border-radius: 8px;
  padding: 20px 24px;
  background: var(--bs-body-bg);
  border: 1px solid var(--bs-border-color);
  border-left-color: %s !important;
}

/* ── about tab ────────────────────────────────────── */
.about-hero {
  background: linear-gradient(135deg, %s 0%%, %s 100%%);
  color: white;
  border-radius: 16px;
  padding: 36px;
  margin-bottom: 24px;
}
", CLR_PRIMARY, CLR_SIDEBAR_TEXT, CLR_SIDEBAR_TEXT,
   CLR_SECONDARY, CLR_SECONDARY,
   CLR_PRIMARY_DK,
   CLR_PRIMARY, CLR_PRIMARY,
   CLR_PRIMARY, CLR_SECONDARY)

# ── bslib theme ────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = CLR_PRIMARY,
  secondary  = CLR_SECONDARY
)

# ── sidebar nav items ──────────────────────────────────────────────────────
sidebar_nav <- sidebar(
  width = 230,
  open  = "always",
  padding = "12px",

  # brand / logo row
  div(
    class = "sidebar-brand",
    bs_icon("diagram-3-fill", size = "1.3em"),
    "HR Analytics"
  ),

  navset_pill_list(
    id   = "main_tab",
    well = FALSE,
    widths = c(11, 1),

    nav_panel(
      tagList(bs_icon("bar-chart-fill"), " Dashboard"),
      value = "desc"
    ),
    nav_panel(
      tagList(bs_icon("graph-up-arrow"), " Diagnostic"),
      value = "diag"
    ),
    nav_panel(
      tagList(bs_icon("cpu-fill"), " Predictive"),
      value = "pred"
    ),
    nav_panel(
      tagList(bs_icon("info-circle-fill"), " About"),
      value = "about"
    )
  ),

  # dark-mode toggle pinned to bottom
  div(
    style = "position: absolute; bottom: 20px; left: 16px; right: 16px;",
    div(
      class = "sidebar-bottom",
      bs_icon("moon-stars-fill"),
      "Dark Mode",
      div(style = "margin-left: auto;",
          input_dark_mode(id = "dark_mode", mode = "light"))
    )
  )
)

# ==============================================================
# ── UI ────────────────────────────────────────────────────────
# ==============================================================
ui <- page_sidebar(
  theme = app_theme,
  sidebar = sidebar_nav,
  fillable = FALSE,
  title = NULL,

  tags$head(tags$style(HTML(custom_css))),

  # ── DESCRIPTIVE / DASHBOARD TAB ──────────────────────────────
  conditionalPanel(
    condition = "input.main_tab == 'desc'",

    div(style = "padding: 8px 0 16px;",
      h4(style = "font-weight:700; margin-bottom:4px;",
         bs_icon("bar-chart-fill"), " Descriptive Analytics"),
      p(class = "text-muted", style = "font-size:0.88rem; margin:0;",
        "Distribution and spread of employee attributes in the training set.")
    ),

    # KPI value boxes
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      gap = "12px",

      value_box(
        title    = "Total Employees",
        value    = format(n_total, big.mark = ","),
        showcase = bs_icon("people-fill"),
        theme    = value_box_theme(bg = CLR_PRIMARY, fg = "white"),
        p(class = "mb-0 opacity-75", "in training set")
      ),
      value_box(
        title    = "Seeking Job Change",
        value    = format(n_looking, big.mark = ","),
        showcase = bs_icon("briefcase-fill"),
        theme    = value_box_theme(bg = CLR_SECONDARY, fg = "white"),
        p(class = "mb-0 opacity-75", paste0(pct_looking, " of total"))
      ),
      value_box(
        title    = "Avg Training Hours",
        value    = avg_training,
        showcase = bs_icon("mortarboard-fill"),
        theme    = value_box_theme(bg = CLR_ACCENT_AMBER, fg = "white"),
        p(class = "mb-0 opacity-75", "per employee")
      ),
      value_box(
        title    = "Not Seeking Change",
        value    = format(n_not, big.mark = ","),
        showcase = bs_icon("shield-check"),
        theme    = value_box_theme(bg = CLR_ACCENT_VIOLET, fg = "white"),
        p(class = "mb-0 opacity-75", paste0(round(n_not/n_total*100,1), "% retained"))
      )
    ),

    # filter row
    card(
      class = "filter-card",
      card_body(
        layout_columns(
          col_widths = c(4, 8),
          radioButtons(
            inputId  = "desc_yaxis",
            label    = tagList(bs_icon("toggles"), " Show Values As"),
            choices  = c("Count", "Percentage"),
            selected = "Count",
            inline   = TRUE
          ),
          checkboxGroupInput(
            inputId  = "desc_gender",
            label    = tagList(bs_icon("gender-ambiguous"), " Filter by Gender"),
            choices  = c("Male", "Female", "Other", "Unknown"),
            selected = c("Male", "Female", "Other", "Unknown"),
            inline   = TRUE
          )
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      gap = "16px",

      card(
        card_header(tagList(bs_icon("pie-chart-fill"), " Job Change Distribution")),
        card_body(plotlyOutput("plot_job_change", height = "380px"))
      ),
      card(
        card_header(tagList(bs_icon("mortarboard-fill"), " Education Level Spread")),
        card_body(plotlyOutput("plot_education", height = "380px"))
      ),
      card(
        card_header(tagList(bs_icon("clock-history"), " Training Hours Spread")),
        card_body(plotlyOutput("plot_training", height = "380px"))
      ),
      card(
        card_header(tagList(bs_icon("building"), " University Enrollment")),
        card_body(plotlyOutput("plot_university", height = "380px"))
      )
    )
  ),

  # ── DIAGNOSTIC TAB ───────────────────────────────────────────
  conditionalPanel(
    condition = "input.main_tab == 'diag'",

    div(style = "padding: 8px 0 16px;",
      h4(style = "font-weight:700; margin-bottom:4px;",
         bs_icon("graph-up-arrow"), " Diagnostic Analytics"),
      p(class = "text-muted", style = "font-size:0.88rem; margin:0;",
        "Explore relationships between employee attributes and job-change behavior.")
    ),

    card(
      class = "filter-card",
      card_body(
        div(
          style = "display:flex; align-items:flex-end; gap:40px; flex-wrap:wrap;",
          div(
            style = "min-width:260px;",
            radioButtons(
              inputId  = "diag_education",
              label    = tagList(bs_icon("mortarboard"), " Education Level"),
              choices  = c("All", "Primary School", "High School",
                           "Graduate", "Masters", "Phd", "Unknown"),
              selected = "All", inline = TRUE
            )
          ),
          div(
            style = "min-width:180px;",
            radioButtons(
              inputId  = "diag_rel_exp",
              label    = tagList(bs_icon("stars"), " Relevant Experience"),
              choices  = c("All", "Yes", "No"),
              selected = "All", inline = TRUE
            )
          ),
          div(
            style = "min-width:260px;",
            checkboxGroupInput(
              inputId  = "diag_gender",
              label    = tagList(bs_icon("gender-ambiguous"), " Gender"),
              choices  = c("Male", "Female", "Other", "Unknown"),
              selected = c("Male", "Female", "Other", "Unknown"),
              inline   = TRUE
            )
          )
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      gap = "16px",

      card(
        card_header(tagList(bs_icon("mortarboard-fill"), " Job Change by Education Level")),
        card_body(plotlyOutput("plot_diag_education", height = "380px"))
      ),
      card(
        card_header(tagList(bs_icon("graph-up"), " Experience vs Job Change Rate")),
        card_body(plotlyOutput("plot_diag_experience", height = "380px"))
      ),
      card(
        card_header(tagList(bs_icon("grid-3x3-gap-fill"), " Major Discipline × Education Heatmap")),
        card_body(plotlyOutput("plot_diag_heatmap", height = "380px"))
      ),
      card(
        card_header(tagList(bs_icon("calendar-check-fill"), " Last New Job vs Job Change Rate")),
        card_body(plotlyOutput("plot_diag_last_job", height = "380px"))
      )
    )
  ),

  # ── PREDICTIVE TAB ───────────────────────────────────────────
  conditionalPanel(
    condition = "input.main_tab == 'pred'",

    div(style = "padding: 8px 0 16px;",
      h4(style = "font-weight:700; margin-bottom:4px;",
         bs_icon("cpu-fill"), " Predictive Analytics"),
      p(class = "text-muted", style = "font-size:0.88rem; margin:0;",
        "Use the trained model to predict whether an employee is likely to seek a job change.")
    ),

    layout_columns(
      col_widths = c(4, 8),
      gap = "20px",

      # input form card
      card(
        card_header(tagList(bs_icon("person-lines-fill"), " Employee Information")),
        card_body(
          selectInput("company_size", tagList(bs_icon("building"), " Company Size"),
                      c("<10","50-99","100-500","500-999","1000-4999","10000+","5000-9999","Unknown")),
          selectInput("company_type", tagList(bs_icon("briefcase"), " Company Type"),
                      c("Early Stage Startup","Funded Startup","NGO","Public Sector","Pvt Ltd","Unknown","Other")),
          numericInput("experience",      tagList(bs_icon("clock"),          " Years of Experience"),  1),
          selectInput("enrolled",         tagList(bs_icon("building"),        " University Enrollment"),
                      c("Full time course","Part time course","No enrollment","Unknown")),
          radioButtons("relevant_exp",    tagList(bs_icon("stars"),           " Relevant Experience"),
                      c("Yes","No"), inline = TRUE),
          selectInput("educ_level",       tagList(bs_icon("mortarboard"),     " Education Level"),
                      c("Graduate","High School","Masters","Phd","Primary School","Unknown")),
          numericInput("last_job_years",  tagList(bs_icon("calendar2-check"), " Years in Previous Job"), 0),
          radioButtons("gender",          tagList(bs_icon("gender-ambiguous")," Gender"),
                      c("Male","Female","Other","Unknown"), inline = TRUE),
          selectInput("discipline",       tagList(bs_icon("book"),            " Major Discipline"),
                      c("STEM","Business Degree","Arts","Humanities","Other","No Major")),
          numericInput("training_hours",  tagList(bs_icon("clock-history"),   " Training Hours"), 0),
          br(),
          actionButton("predict_button", tagList(bs_icon("lightning-fill"), " Predict"),
                       class = "btn-primary w-100", style = "font-weight:600;")
        )
      ),

      # results card
      card(
        card_header(tagList(bs_icon("clipboard2-data-fill"), " Prediction Results")),
        card_body(
          div(class = "predict-result",
            h4(style = "font-weight:700; margin-bottom:8px;",
               textOutput("prediction_decision")),
            p(class = "text-muted mb-0",
              textOutput("prediction_prob"))
          ),
          br(),
          p(class = "text-muted", style = "font-size:0.83rem;",
            bs_icon("info-circle"),
            " Enter employee details on the left and click Predict to generate a result.")
        )
      )
    )
  ),

  # ── ABOUT TAB ───────────────────────────────────────────────
  conditionalPanel(
    condition = "input.main_tab == 'about'",

    div(
      class = "about-hero",
      h3(style = "font-weight:800; margin-bottom:6px;",
         bs_icon("diagram-3-fill"), " HR Analytics Dashboard"),
      p(style = "opacity:0.9; margin:0; font-size:1.05rem;",
        "A data-driven approach to understanding and predicting employee behavior.")
    ),

    layout_columns(
      col_widths = c(6, 6),
      gap = "16px",

      card(
        card_header(tagList(bs_icon("bar-chart-fill"), " About This Dashboard")),
        card_body(
          p("This dashboard provides three layers of HR analytics:"),
          tags$ul(
            tags$li(tags$b("Descriptive Analytics — "),
                    "Summarizes the distribution and spread of employee attributes."),
            tags$li(tags$b("Diagnostic Analytics — "),
                    "Explores correlations between attributes and job-change likelihood."),
            tags$li(tags$b("Predictive Analytics — "),
                    "Uses a trained machine learning model to predict whether an employee is likely to seek a job change.")
          )
        )
      ),

      card(
        card_header(tagList(bs_icon("database-fill"), " Dataset")),
        card_body(
          p("The dataset used is a cleaned HR dataset sourced from a job change prediction study."),
          p("Features include education level, company size/type, experience, training hours, gender, and enrollment status.")
        )
      )
    )
  )
)

# ==============================================================
# ── SERVER ────────────────────────────────────────────────────
# ==============================================================
server <- function(input, output, session) {

  # ── shared chart colors (reflect theme above) ──────────────
  # ============================================================
  # ★  CHART COLOR PALETTE  — modify to restyle all charts  ★
  # ============================================================
  job_colors <- c(
    "Looking"     = CLR_CHART_LOOK,   # teal
    "Not Looking" = CLR_CHART_NOT    # slate gray
  )
  edu_palette  <- "GnBu"   # RColorBrewer palette for education bars
  # ============================================================

  df_diag_base <- df_train %>% filter(job_change_label != "No Data")

  # ── reactive filtered data ────────────────────────────────
  desc_data <- reactive({
    df_train %>% filter(gender %in% input$desc_gender)
  })

  diag_data <- reactive({
    d <- df_diag_base %>% filter(gender %in% input$diag_gender)
    if (input$diag_rel_exp  != "All") d <- d %>% filter(relevant_experience == input$diag_rel_exp)
    if (input$diag_education != "All") d <- d %>% filter(as.character(education_level) == input$diag_education)
    d
  })

  # helper: count or pct
  get_y <- function(data, group_col) {
    d <- data %>%
      count({{ group_col }}) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    if (input$desc_yaxis == "Percentage") {
      d %>% mutate(y_val = pct, y_label = paste0(pct, "%"))
    } else {
      d %>% mutate(y_val = n,   y_label = as.character(n))
    }
  }

  # ── DESCRIPTIVE CHARTS ──────────────────────────────────────

  output$plot_job_change <- renderPlotly({
    d <- get_y(desc_data(), job_change_label)
    p <- ggplot(d, aes(x = job_change_label, y = y_val, fill = job_change_label,
                       text = paste0(job_change_label, ": ", y_label))) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = y_label), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = job_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Status", y = input$desc_yaxis) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$plot_education <- renderPlotly({
    d <- get_y(desc_data(), education_level)
    p <- ggplot(d, aes(x = education_level, y = y_val, fill = education_level,
                       text = paste0(education_level, ": ", y_label))) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = edu_palette) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Education Level", y = input$desc_yaxis) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$plot_training <- renderPlotly({
    p <- ggplot(desc_data(), aes(y = training_hours,
                                 text = paste0("Hours: ", training_hours))) +
      geom_boxplot(fill = CLR_CHART_LOOK, color = CLR_PRIMARY_DK,
                   outlier.alpha = 0.3, width = 0.4) +
      labs(y = "Training Hours", x = "") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$plot_university <- renderPlotly({
    d <- get_y(desc_data(), enrolled_university)
    p <- ggplot(d, aes(x = enrolled_university, y = y_val, fill = enrolled_university,
                       text = paste0(enrolled_university, ": ", y_label))) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = edu_palette) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Enrollment Status", y = input$desc_yaxis) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  # ── DIAGNOSTIC CHARTS ────────────────────────────────────────

  output$plot_diag_education <- renderPlotly({
    edu_job <- diag_data() %>% count(education_level, job_change_label)
    p <- ggplot(edu_job, aes(x = education_level, y = n, fill = job_change_label,
                             text = paste0(education_level, " — ", job_change_label, ": ", n))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = job_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Education Level", y = "Count", fill = "Status") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 15, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$plot_diag_experience <- renderPlotly({
    exp_rate <- diag_data() %>%
      group_by(experience_numeric) %>%
      summarise(total = n(), looking = sum(job_change_label == "Looking"),
                rate = round(looking / total * 100, 1))
    p <- ggplot(exp_rate, aes(x = experience_numeric, y = rate,
                              text = paste0("Experience: ", experience_numeric,
                                            " yrs\nLooking rate: ", rate, "%"))) +
      geom_line(color = CLR_CHART_LOOK, linewidth = 1) +
      geom_point(color = CLR_PRIMARY_DK, size = 2.5) +
      scale_x_continuous(breaks = seq(0, 21, by = 3)) +
      labs(x = "Years of Experience", y = "% Looking for Job Change") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$plot_diag_heatmap <- renderPlotly({
    heat_data <- diag_data() %>% count(major_discipline, education_level)
    p <- ggplot(heat_data, aes(x = education_level, y = major_discipline, fill = n,
                               text = paste0(major_discipline, " + ", education_level, ": ", n))) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient(low = CLR_HEAT_LO, high = CLR_HEAT_HI) +
      labs(x = "Education Level", y = "Major Discipline", fill = "Count") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$plot_diag_last_job <- renderPlotly({
    last_rate <- diag_data() %>%
      group_by(last_new_job_numeric) %>%
      summarise(total = n(), looking = sum(job_change_label == "Looking"),
                rate = round(looking / total * 100, 1))
    p <- ggplot(last_rate, aes(x = last_new_job_numeric, y = rate,
                               text = paste0("Years since last job: ", last_new_job_numeric,
                                             "\nLooking rate: ", rate, "%"))) +
      geom_col(fill = CLR_CHART_LOOK) +
      scale_x_continuous(breaks = 0:5) +
      labs(x = "Years Since Last Job Change", y = "% Looking for Job Change") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  # ── PREDICTIVE ───────────────────────────────────────────────
  prediction_results <- reactiveValues(decision = "No prediction made yet.", prob = "")

  observeEvent(input$predict_button, {
    exp_num      <- as.numeric(input$experience)
    lnj_num      <- as.numeric(input$last_job_years)

    new_employee <- tibble(
      company_size         = factor(input$company_size),
      company_type         = factor(input$company_type),
      experience_numeric   = exp_num,
      experience           = factor(map_experience(exp_num)),
      enrolled_university  = factor(input$enrolled),
      relevant_experience  = factor(input$relevant_exp),
      education_level      = factor(input$educ_level),
      last_new_job_numeric = as.numeric(map_lnj(lnj_num)),
      gender               = factor(input$gender),
      major_discipline     = factor(input$discipline),
      training_hours       = as.numeric(input$training_hours)
    )

    result <- tryCatch({
      pred_class  <- predict(model, new_data = new_employee, type = "class")[[1]]
      pred_prob   <- predict(model, new_data = new_employee, type = "prob")
      prob_looking <- round(pred_prob$.pred_Looking * 100, 1)
      list(
        decision = paste("Decision:", as.character(pred_class)),
        prob     = paste0("Probability of seeking job change: ", prob_looking, "%")
      )
    }, error = function(e) {
      list(decision = "Prediction error.", prob = paste("Details:", e$message))
    })

    prediction_results$decision <- result$decision
    prediction_results$prob     <- result$prob
  })

  output$prediction_decision <- renderText({ prediction_results$decision })
  output$prediction_prob     <- renderText({ prediction_results$prob })
}

shinyApp(ui = ui, server = server)
