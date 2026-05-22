# gi keep ni nga snippet para reference lang, dili na ni included sa dashboard

#  gender breakdown pie (gi remove kay redundant sa gender checkbox filter) ---
output$plot_gender <- renderPlotly({

  gender_dist <- desc_data() %>%
    count(gender) %>%
    mutate(pct = round(n / sum(n) * 100, 1))

  plot_ly(gender_dist,
          labels = ~gender, values = ~n, type = "pie",
          textinfo = "label+percent",
          hovertemplate = paste0("%{label}: %{value} (%{percent})<extra></extra>"),
          marker = list(colors = c("#4C9BE8", "#81C784", "#B0BEC5", "#FFB74D")))
})

#  relevant experience bar (gi remove kay redundant sa relevant experience radio filter) ---
output$plot_rel_exp <- renderPlotly({

  d <- get_y(desc_data(), relevant_experience)

  p <- ggplot(d,
              aes(x = relevant_experience, y = y_val, fill = relevant_experience,
                  text = paste0(relevant_experience, ": ", y_label))) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_text(aes(label = y_label), vjust = -0.4, size = 3.5) +
    scale_fill_manual(values = c("Yes" = "#4C9BE8", "No" = "#B0BEC5")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = "Relevant Experience", y = input$desc_yaxis) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank())

  ggplotly(p, tooltip = "text")
})

#  relevant experience vs job change grouped bar (gi remove kay redundant sa diagnostic radio filter) ---
output$plot_diag_rel_exp <- renderPlotly({

  rel_job <- diag_data() %>%
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