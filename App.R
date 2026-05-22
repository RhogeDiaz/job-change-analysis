library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(tidyverse)

model = readRDS("../job-change-analysis/job_change_predictor.rds")
dataset = "https://raw.githubusercontent.com/RhogeDiaz/job-change-analysis/refs/heads/main/clean_hr_dataset.csv"

# gi load ang cleaned dataset
df <- read_csv(dataset, show_col_types = FALSE)

# train rows ra ang gamiton para sa tanan nga charts
df_train <- df %>% filter(split == "train")

map_experience <- function(val) {
  if (is.null(val) || is.na(val) || !is.numeric(val)) {
    return("Unknown")
  }
  if (val < 1) {
    return("<1")
  } else if (val > 20) {
    return(">20")
  } else {
    return(as.character(as.integer(val)))
  }
}

map_lnj <- function(val) {
  if (is.null(val) || is.na(val) || !is.numeric(val)) {
    return(0)
  }
  if (val < 1) {
    return(0)
  } else if (val > 5) {
    return(5)
  } else {
    return(val)
  }
}


ui <- page_navbar(
  title = "HR Analytics Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,


  # DESCRIPTIVE ANALYTICS TAB

  nav_panel(
    title = "Descriptive Analytics",

    div(
      style = "overflow-y: auto; padding: 16px;",

      # filter row para sa descriptive tab
      card(
        card_body(
          layout_columns(
            col_widths = c(6, 6),

            # filter 1 - para ma pilion kung count o percentage ang ipakita sa y-axis
            radioButtons(
              inputId  = "desc_yaxis",
              label    = "Show Values As",
              choices  = c("Count", "Percentage"),
              selected = "Count",
              inline   = TRUE
            ),

            # filter 2 - para ma filter tanan descriptive charts base sa gender
            checkboxGroupInput(
              inputId  = "desc_gender",
              label    = "Filter by Gender",
              choices  = c("Male", "Female", "Other", "Unknown"),
              selected = c("Male", "Female", "Other", "Unknown"),
              inline   = TRUE
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Job Change Distribution"),
          plotlyOutput("plot_job_change", height = "450px")
        ),

        card(
          card_header("Education Level Spread"),
          plotlyOutput("plot_education", height = "450px")
        ),

        card(
          card_header("Training Hours Spread"),
          plotlyOutput("plot_training", height = "450px")
        ),

        card(
          card_header("University Enrollment"),
          plotlyOutput("plot_university", height = "450px")
        )
      )
    )
  ),


  # DIAGNOSTIC ANALYTICS TAB

  nav_panel(
    title = "Diagnostic Analytics",

    div(
      style = "overflow-y: auto; padding: 16px;",

      # filter row para sa diagnostic tab — gi flex para pareho ang height sa tanan inputs
      card(
        style = "min-height: 120px;",
        card_body(
          padding = "16px",
          div(
            style = "display: flex; align-items: flex-end; gap: 48px; flex-wrap: wrap;",

            # filter 1 - radio buttons para ma drill down sa specific education level
            # gi change gikan selectInput para dili mag expand paubos ang dropdown
            div(
              style = "min-width: 200px;",
              radioButtons(
                inputId  = "diag_education",
                label    = "Filter by Education Level",
                choices  = c("All", "Primary School", "High School",
                             "Graduate", "Masters", "Phd", "Unknown"),
                selected = "All",
                inline   = TRUE
              )
            ),

            # filter 2 - radio para ma filter base sa relevant experience
            div(
              style = "min-width: 180px;",
              radioButtons(
                inputId  = "diag_rel_exp",
                label    = "Relevant Experience",
                choices  = c("All", "Yes", "No"),
                selected = "All",
                inline   = TRUE
              )
            ),

            # filter 3 - checkboxes para ma filter base sa gender
            div(
              style = "min-width: 260px;",
              checkboxGroupInput(
                inputId  = "diag_gender",
                label    = "Filter by Gender",
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

        card(
          card_header("Job Change by Education Level"),
          plotlyOutput("plot_diag_education", height = "450px")
        ),

        card(
          card_header("Experience vs Job Change Rate"),
          plotlyOutput("plot_diag_experience", height = "450px")
        ),

        card(
          card_header("Major Discipline x Education Heatmap"),
          plotlyOutput("plot_diag_heatmap", height = "450px")
        ),

        card(
          card_header("Last New Job vs Job Change Rate"),
          plotlyOutput("plot_diag_last_job", height = "450px")
        )
      )
    )
  ),


  # PREDICTIVE ANALYTICS TAB — wala gi usab, gi keep as-is
  nav_panel(
    title = "Predictive Analytics",
    fluidRow(
      sidebarPanel(
        h3('Enter Employee Information'),
        selectInput('company_size', "Company Size", c('<10', '50-99', '100-500', '500-999', '1000-4999', '10000+', '5000-9999', 'Unknown')),
        selectInput('company_type', 'Company Type', c('Early Stage Startup', 'Funded Startup', 'NGO', 'Public Sector', 'Pvt Ltd', 'Unknown', 'Other')),
        numericInput('experience', 'Years of Experience', 1),
        selectInput('enrolled', 'Enrolled University', c('Full time course', 'Part time course', 'No enrollment', 'Unknown')),
        radioButtons('relevant_exp', 'Has experience in the current field?', c('Yes', 'No')),
        selectInput('educ_level', 'Educational Attainment', c('Graduate', 'High School', 'Masters', 'Phd', 'Primary School', 'Unknown')),
        numericInput('last_job_years', 'Years in previous job', 0),
        radioButtons('gender', 'Gender', c('Male', 'Female', 'Other', 'Unknown')),
        selectInput('discipline', 'Major Discipline', c('STEM', 'Business Degree', 'Arts', 'Humanities', 'Other', 'No Major')),
        numericInput('training_hours', 'Training Hours', 0),
        actionButton('predict_button', 'Predict Employee Job Change')
      ),
      mainPanel(
        h3('Prediction Results'),
        hr(),
        h3(textOutput('prediction_decision')),
        p(textOutput('prediction_prob')),
        br()
      )
    )
  ),


  # ABOUT TAB — gi keep as-is
  nav_panel(
    title = "About"
  )
)


server <- function(input, output) {

  # common colors para consistent tanan charts
  job_colors <- c("Looking" = "#4C9BE8", "Not Looking" = "#B0BEC5")

  # gi filter ang test rows para sa diagnostic
  df_diag_base <- df_train %>% filter(job_change_label != "No Data")


  # DESCRIPTIVE — gi reactive ang data base sa gender filter
  desc_data <- reactive({
    df_train %>% filter(gender %in% input$desc_gender)
  })

  # helper function — mo return count or pct depende sa radio
  # para dili mag duplicate og code sa matag chart
  get_y <- function(data, group_col) {
    d <- data %>%
      count({{ group_col }}) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    if (input$desc_yaxis == "Percentage") {
      d <- d %>% mutate(y_val = pct, y_label = paste0(pct, "%"))
    } else {
      d <- d %>% mutate(y_val = n, y_label = as.character(n))
    }
    d
  }


  # chart 1 - job change distribution, mo react sa gender filter + count/pct toggle
  output$plot_job_change <- renderPlotly({

    d <- get_y(desc_data(), job_change_label)

    p <- ggplot(d,
                aes(x = job_change_label, y = y_val, fill = job_change_label,
                    text = paste0(job_change_label, ": ", y_label))) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = y_label), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = job_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Status", y = input$desc_yaxis) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 2 - education level spread, mo react sa gender filter + count/pct toggle
  output$plot_education <- renderPlotly({

    d <- get_y(desc_data(), education_level)

    p <- ggplot(d,
                aes(x = education_level, y = y_val, fill = education_level,
                    text = paste0(education_level, ": ", y_label))) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Education Level", y = input$desc_yaxis) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 3 - training hours boxplot, mo react sa gender filter
  output$plot_training <- renderPlotly({

    p <- ggplot(desc_data(),
                aes(y = training_hours,
                    text = paste0("Hours: ", training_hours))) +
      geom_boxplot(fill = "#4C9BE8", color = "#1565C0",
                   outlier.alpha = 0.3, width = 0.4) +
      labs(y = "Training Hours", x = "") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text.x        = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 4 - university enrollment, mo react sa gender filter + count/pct toggle
  output$plot_university <- renderPlotly({

    d <- get_y(desc_data(), enrolled_university)

    p <- ggplot(d,
                aes(x = enrolled_university, y = y_val, fill = enrolled_university,
                    text = paste0(enrolled_university, ": ", y_label))) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Enrollment Status", y = input$desc_yaxis) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })


  # DIAGNOSTIC — gi reactive ang data base sa tanan diagnostic filters
  diag_data <- reactive({
    d <- df_diag_base %>% filter(gender %in% input$diag_gender)

    if (input$diag_rel_exp != "All") {
      d <- d %>% filter(relevant_experience == input$diag_rel_exp)
    }

    if (input$diag_education != "All") {
      d <- d %>% filter(as.character(education_level) == input$diag_education)
    }

    d
  })

  # chart 1 - job change by education, mo react sa tanan diagnostic filters
  output$plot_diag_education <- renderPlotly({

    edu_job <- diag_data() %>%
      count(education_level, job_change_label)

    p <- ggplot(edu_job,
                aes(x = education_level, y = n, fill = job_change_label,
                    text = paste0(education_level, " — ", job_change_label, ": ", n))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = job_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Education Level", y = "Count", fill = "Status") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text.x        = element_text(angle = 15, hjust = 1))

    ggplotly(p, tooltip = "text")
  })

  # chart 2 - experience vs job change rate, mo react sa tanan diagnostic filters
  output$plot_diag_experience <- renderPlotly({

    exp_rate <- diag_data() %>%
      group_by(experience_numeric) %>%
      summarise(
        total   = n(),
        looking = sum(job_change_label == "Looking"),
        rate    = round(looking / total * 100, 1)
      )

    p <- ggplot(exp_rate,
                aes(x = experience_numeric, y = rate,
                    text = paste0("Experience: ", experience_numeric,
                                  " yrs<br>Looking rate: ", rate, "%"))) +
      geom_line(color = "#4C9BE8", linewidth = 1) +
      geom_point(color = "#1565C0", size = 2) +
      scale_x_continuous(breaks = seq(0, 21, by = 3)) +
      labs(x = "Years of Experience", y = "% Looking for Job Change") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 3 - heatmap, mo react sa tanan diagnostic filters
  output$plot_diag_heatmap <- renderPlotly({

    heat_data <- diag_data() %>%
      count(major_discipline, education_level)

    p <- ggplot(heat_data,
                aes(x = education_level, y = major_discipline, fill = n,
                    text = paste0(major_discipline, " + ", education_level, ": ", n))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#E3F2FD", high = "#1565C0") +
      labs(x = "Education Level", y = "Major Discipline", fill = "Count") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))

    ggplotly(p, tooltip = "text")
  })

  # chart 4 - last job gap vs job change rate, mo react sa tanan diagnostic filters
  output$plot_diag_last_job <- renderPlotly({

    last_rate <- diag_data() %>%
      group_by(last_new_job_numeric) %>%
      summarise(
        total   = n(),
        looking = sum(job_change_label == "Looking"),
        rate    = round(looking / total * 100, 1)
      )

    p <- ggplot(last_rate,
                aes(x = last_new_job_numeric, y = rate,
                    text = paste0("Years since last job: ", last_new_job_numeric,
                                  "<br>Looking rate: ", rate, "%"))) +
      geom_col(fill = "#4C9BE8") +
      scale_x_continuous(breaks = 0:5) +
      labs(x = "Years Since Last Job Change", y = "% Looking for Job Change") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })


  # PREDICTIVE PART — wala gi usab, gi keep as-is
  prediction_results <- reactiveValues(decision = "No prediction made yet", prob = "")

  observeEvent(input$predict_button, {

    exp_num = as.numeric(input$experience)
    exp_cat = map_experience(exp_num)

    lnj_num = as.numeric(input$last_job_years)
    lnj_mapped_num = map_lnj(lnj_num)

    new_employee = tibble(
      company_size         = factor(input$company_size),
      company_type         = factor(input$company_type),
      experience_numeric   = exp_num,
      experience           = factor(exp_cat),
      enrolled_university  = factor(input$enrolled),
      relevant_experience  = factor(input$relevant_exp),
      education_level      = factor(input$educ_level),
      last_new_job_numeric = as.numeric(lnj_mapped_num),
      gender               = factor(input$gender),
      major_discipline     = factor(input$discipline),
      training_hours       = as.numeric(input$training_hours)
    )

    result <- tryCatch({
      pred_class <- predict(wf_fit, new_data = new_employee, type = "class")[[1]]
      pred_prob  <- predict(wf_fit, new_data = new_employee, type = "prob")
      prob_looking <- round(pred_prob$.pred_Looking * 100, 1)

      list(
        decision = paste("Employee Status Decision:", as.character(pred_class)),
        prob     = paste0("Probability of looking for a job change: ", prob_looking, "%")
      )
    }, error = function(e) {
      list(
        decision = "Error in Prediction",
        prob     = paste("Verify factor alignment:", e$message)
      )
    })

    prediction_results$decision <- result$decision
    prediction_results$prob     <- result$prob
  })

  output$prediction_decision <- renderText({ prediction_results$decision })
  output$prediction_prob     <- renderText({ prediction_results$prob })
}

shinyApp(ui = ui, server = server)