library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(tidyverse)

# gi load ang cleaned dataset
df <- read_csv("clean_hr_dataset.csv", show_col_types = FALSE)

# train rows ra ang gamiton para sa tanan nga charts
df_train <- df %>% filter(split == "train")


ui <- page_navbar(
  title = "HR Analytics Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,


  # DESCRIPTIVE ANALYTICS TAB

  nav_panel(
    title = "Descriptive Analytics",

    # gi wrap sa div para scrollable ang page, dili mag squeeze ang charts
    div(
      style = "overflow-y: auto; padding: 16px;",

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
          card_header("Gender Breakdown"),
          plotlyOutput("plot_gender", height = "450px")
        ),

        card(
          card_header("Training Hours Spread"),
          plotlyOutput("plot_training", height = "450px")
        ),

        card(
          card_header("Relevant Experience"),
          plotlyOutput("plot_rel_exp", height = "450px")
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

    # same ra, gi wrap para scrollable pud
    div(
      style = "overflow-y: auto; padding: 16px;",

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
          card_header("Relevant Experience vs Job Change"),
          plotlyOutput("plot_diag_rel_exp", height = "450px")
        ),

        card(
          card_header("Last New Job vs Job Change Rate"),
          plotlyOutput("plot_diag_last_job", height = "450px")
        )
      )
    )
  )
)


server <- function(input, output) {

  # gi define ang common colors para consistent tanan charts
  job_colors <- c("Looking" = "#4C9BE8", "Not Looking" = "#B0BEC5")


  # DESCRIPTIVE CHARTS

  # chart 1 - pila ka tawo ang looking/not looking mag change ug job
  output$plot_job_change <- renderPlotly({

    job_dist <- df_train %>%
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

  # chart 2 - unsa ang education level sa kadaghanan sa mga enrollees
  output$plot_education <- renderPlotly({

    edu_dist <- df_train %>%
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

  # chart 3 - unsa ang gender breakdown sa mga enrollees
  output$plot_gender <- renderPlotly({

    gender_dist <- df_train %>%
      count(gender) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    plot_ly(gender_dist,
            labels = ~gender, values = ~n, type = "pie",
            textinfo = "label+percent",
            hovertemplate = paste0("%{label}: %{value} (%{percent})<extra></extra>"),
            marker = list(colors = c("#4C9BE8", "#81C784", "#B0BEC5", "#FFB74D")))
  })

  # chart 4 - pila ka training hours ang natapos sa mga enrollees
  output$plot_training <- renderPlotly({

    p <- ggplot(df_train,
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

  # chart 5 - pila ang may relevant experience ug wala
  output$plot_rel_exp <- renderPlotly({

    rel_dist <- df_train %>%
      count(relevant_experience) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    p <- ggplot(rel_dist,
                aes(x = relevant_experience, y = n, fill = relevant_experience,
                    text = paste0(relevant_experience, ": ", n, " (", pct, "%)"))) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = paste0(n, "\n(", pct, "%)")), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = c("Yes" = "#4C9BE8", "No" = "#B0BEC5")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Relevant Experience", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank())

    ggplotly(p, tooltip = "text")
  })

  # chart 6 - enrolled ba sila sa university or wala
  output$plot_university <- renderPlotly({

    uni_dist <- df_train %>%
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

  # gi filter ang test rows para dili masama ang diagnostic charts
  df_diag <- df_train %>% filter(job_change_label != "No Data")

  # chart 1 - kinsa ang mas gusto mag change ug job base sa ilang education
  output$plot_diag_education <- renderPlotly({

    edu_job <- df_diag %>%
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

  # chart 2 - mas gusto bang mag change ug job ang mas experienced o dili
  output$plot_diag_experience <- renderPlotly({

    exp_rate <- df_diag %>%
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

  # chart 3 - heatmap sa major discipline ug education level
  output$plot_diag_heatmap <- renderPlotly({

    heat_data <- df_diag %>%
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

  # chart 4 - mas gusto bang mag change ug job ang naa ug relevant experience
  output$plot_diag_rel_exp <- renderPlotly({

    rel_job <- df_diag %>%
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

  # chart 5 - mas layo ba ang last job change, mas gusto mag change pag-usab
  output$plot_diag_last_job <- renderPlotly({

    last_rate <- df_diag %>%
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