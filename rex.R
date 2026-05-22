library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(tidyverse)

# gi load ang cleaned dataset
df <- read_csv("clean_hr_dataset.csv", show_col_types = FALSE)

# train rows ra ang gamiton
df_train <- df %>% filter(split == "train")
df_diag  <- df_train %>% filter(job_change_label != "No Data")

# common colors para consistent tanan charts
job_colors <- c("Looking" = "#4C9BE8", "Not Looking" = "#B0BEC5")


ui <- page_navbar(
  title = "HR Analytics Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,

  nav_panel(
    title = "Dashboard",

    div(
      style = "padding: 16px;",

      # FILTER ROW — naa sa taas para makita dayon sa user
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),

            # filter 1 - para ma filter tanan charts base sa job change status
            radioButtons(
              inputId  = "filter_status",
              label    = "Job Change Status",
              choices  = c("All", "Looking", "Not Looking"),
              selected = "All",
              inline   = TRUE
            ),

            # filter 2 - para ma filter base sa gender
            checkboxGroupInput(
              inputId  = "filter_gender",
              label    = "Gender",
              choices  = c("Male", "Female", "Other", "Unknown"),
              selected = c("Male", "Female", "Other", "Unknown"),
              inline   = TRUE
            ),

            # filter 3 - para ma filter base sa relevant experience
            radioButtons(
              inputId  = "filter_exp",
              label    = "Relevant Experience",
              choices  = c("All", "Yes", "No"),
              selected = "All",
              inline   = TRUE
            )
          )
        )
      ),

      # DESCRIPTIVE CHARTS
      h5("Descriptive Analytics", style = "margin-top: 16px; font-weight: bold;"),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Job Change Distribution"),
          plotlyOutput("plot_job_change", height = "420px")
        ),
        card(
          card_header("Education Level Spread"),
          plotlyOutput("plot_education", height = "420px")
        ),
        card(
          card_header("Training Hours Spread"),
          plotlyOutput("plot_training", height = "420px")
        ),
        card(
          card_header("University Enrollment"),
          plotlyOutput("plot_university", height = "420px")
        )
      ),

      # DIAGNOSTIC CHARTS
      h5("Diagnostic Analytics", style = "margin-top: 24px; font-weight: bold;"),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Job Change by Education Level"),
          plotlyOutput("plot_diag_education", height = "420px")
        ),
        card(
          card_header("Experience vs Job Change Rate"),
          plotlyOutput("plot_diag_experience", height = "420px")
        ),
        card(
          card_header("Major Discipline x Education Heatmap"),
          plotlyOutput("plot_diag_heatmap", height = "420px")
        ),
        card(
          card_header("Relevant Experience vs Job Change"),
          plotlyOutput("plot_diag_rel_exp", height = "420px")
        ),
        card(
          card_header("Last New Job vs Job Change Rate"),
          plotlyOutput("plot_diag_last_job", height = "420px")
        )
      )
    )
  )
)


server <- function(input, output) {

  # gi reactive ang data para mo update tanan charts kung nag filter ang user
  filtered <- reactive({
    d <- df_train

    # gi apply ang gender filter
    if (!is.null(input$filter_gender)) {
      d <- d %>% filter(gender %in% input$filter_gender)
    }

    # gi apply ang relevant experience filter
    if (input$filter_exp != "All") {
      d <- d %>% filter(relevant_experience == input$filter_exp)
    }

    # gi apply ang job change status filter
    if (input$filter_status != "All") {
      d <- d %>% filter(job_change_label == input$filter_status)
    }

    d
  })

  # sama ra pero para sa diagnostic charts — walay "No Data" rows
  filtered_diag <- reactive({
    filtered() %>% filter(job_change_label != "No Data")
  })


  # DESCRIPTIVE CHARTS

  # chart 1 - pila ka tawo ang looking/not looking base sa mga filter
  output$plot_job_change <- renderPlotly({

    job_dist <- filtered() %>%
      count(job_change_label) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    p <- ggplot(job_dist,
                aes(x = job_change_label, y = n, fill = job_change_label,
                    text = paste0(job_change_label, ": ", n, " (", pct, "%)"))) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = paste0(n, "\n(", pct, "%)")), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = job_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Status", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 2 - education level spread base sa filters
  output$plot_education <- renderPlotly({

    edu_dist <- filtered() %>%
      count(education_level) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    p <- ggplot(edu_dist,
                aes(x = education_level, y = n, fill = education_level,
                    text = paste0(education_level, ": ", n, " (", pct, "%)"))) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Education Level", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 3 - training hours boxplot base sa filters
  output$plot_training <- renderPlotly({

    p <- ggplot(filtered(),
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

  # chart 4 - university enrollment base sa filters
  output$plot_university <- renderPlotly({

    uni_dist <- filtered() %>%
      count(enrolled_university) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    p <- ggplot(uni_dist,
                aes(x = enrolled_university, y = n, fill = enrolled_university,
                    text = paste0(enrolled_university, ": ", n, " (", pct, "%)"))) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Enrollment Status", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })


  # DIAGNOSTIC CHARTS

  # chart 1 - job change by education, mo react sa tanan filters
  output$plot_diag_education <- renderPlotly({

    edu_job <- filtered_diag() %>%
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

  # chart 2 - experience vs job change rate, mo react sa tanan filters
  output$plot_diag_experience <- renderPlotly({

    exp_rate <- filtered_diag() %>%
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

  # chart 3 - heatmap, mo react sa tanan filters
  output$plot_diag_heatmap <- renderPlotly({

    heat_data <- filtered_diag() %>%
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

  # chart 4 - relevant experience vs job change, mo react sa tanan filters
  output$plot_diag_rel_exp <- renderPlotly({

    rel_job <- filtered_diag() %>%
      count(relevant_experience, job_change_label)

    p <- ggplot(rel_job,
                aes(x = relevant_experience, y = n, fill = job_change_label,
                    text = paste0(relevant_experience, " — ", job_change_label, ": ", n))) +
      geom_col(position = "dodge", width = 0.5) +
      scale_fill_manual(values = job_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Relevant Experience", y = "Count", fill = "Status") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 5 - last job gap vs job change rate, mo react sa tanan filters
  output$plot_diag_last_job <- renderPlotly({

    last_rate <- filtered_diag() %>%
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
}

shinyApp(ui = ui, server = server)