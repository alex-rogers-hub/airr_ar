# ============================================
# Report Download Handlers
# ============================================

output$download_brand_report <- downloadHandler(
  filename = function() {
    paste0("AiRR_Brand_Report_", rv$brand_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    
    notif <- showNotification("Generating PDF report...", type = "message", duration = NULL)
    
    tryCatch({
      ts_data <- dash_timeseries()
      trend_plot <- if (!is.null(ts_data) && nrow(ts_data) > 0) {
        create_dash_chart(ts_data, "airr_score", "AIRR Score", rv$brand_name)
      } else {
        NULL
      }
      
      scores <- dash_latest_scores()
      spider_plot <- if (!is.null(scores) && nrow(scores) > 0) {
        p <- plot_ly(type = 'scatterpolar', fill = 'toself')
        
        comp_data <- scores %>% filter(main_brand_flag == FALSE)
        for (i in seq_len(nrow(comp_data))) {
          p <- p %>% add_trace(
            r = c(comp_data$presence_score[i], comp_data$perception_score[i],
                  comp_data$prestige_score[i], comp_data$persistence_score[i],
                  comp_data$presence_score[i]),
            theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
            name = comp_data$brand_name[i],
            line = list(width = 1.5, dash = "dot"),
            opacity = 0.5,
            fillcolor = 'rgba(200,200,200,0.1)'
          )
        }
        
        main_data <- scores %>% filter(main_brand_flag == TRUE)
        if (nrow(main_data) > 0) {
          p <- p %>% add_trace(
            r = c(main_data$presence_score[1], main_data$perception_score[1],
                  main_data$prestige_score[1], main_data$persistence_score[1],
                  main_data$presence_score[1]),
            theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
            name = main_data$brand_name[1],
            line = list(width = 4, color = '#667eea'),
            fillcolor = 'rgba(102,126,234,0.3)'
          )
        }
        
        p %>% layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 100)),
            angularaxis = list(showline = TRUE)
          ),
          showlegend = TRUE,
          plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF"
        )
      } else {
        NULL
      }
      
      generate_brand_report(
        brand_name = rv$brand_name,
        ai_summary = "",
        rankings_data = scores,
        trend_plot = trend_plot,
        spider_plot = spider_plot,
        output_path = file
      )
      
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), type = "error", duration = 10)
    }, finally = {
      removeNotification(notif)
    })
  }
)

output$download_prompt_report <- downloadHandler(
  filename = function() {
    prompt_short <- substr(gsub("[^A-Za-z0-9 ]", "", input$dash_query_select), 1, 30)
    prompt_short <- gsub(" ", "_", trimws(prompt_short))
    paste0("AiRR_Prompt_Report_", prompt_short, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    
    notif <- showNotification("Generating PDF report...", type = "message", duration = NULL)
    
    tryCatch({
      query_data <- dash_query_timeseries()
      
      trend_plot <- if (!is.null(query_data) && nrow(query_data) > 0) {
        create_dash_chart(query_data, "airr_score", "AIRR Score", rv$brand_name)
      } else {
        NULL
      }
      
      spider_plot <- if (!is.null(query_data) && nrow(query_data) > 0) {
        latest <- query_data %>%
          group_by(brand_name, brand_id, main_brand_flag) %>%
          filter(date == max(date)) %>%
          ungroup()
        
        p <- plot_ly(type = 'scatterpolar', fill = 'toself')
        
        comp <- latest %>% filter(main_brand_flag == FALSE)
        for (i in seq_len(nrow(comp))) {
          p <- p %>% add_trace(
            r = c(comp$presence_score[i], comp$perception_score[i],
                  comp$prestige_score[i], comp$persistence_score[i],
                  comp$presence_score[i]),
            theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
            name = comp$brand_name[i],
            line = list(width = 1.5, dash = "dot"),
            opacity = 0.5,
            fillcolor = 'rgba(200,200,200,0.1)'
          )
        }
        
        main <- latest %>% filter(main_brand_flag == TRUE)
        if (nrow(main) > 0) {
          p <- p %>% add_trace(
            r = c(main$presence_score[1], main$perception_score[1],
                  main$prestige_score[1], main$persistence_score[1],
                  main$presence_score[1]),
            theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
            name = main$brand_name[1],
            line = list(width = 4, color = '#667eea'),
            fillcolor = 'rgba(102,126,234,0.3)'
          )
        }
        
        p %>% layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 100)),
            angularaxis = list(showline = TRUE)
          ),
          showlegend = TRUE,
          plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF"
        )
      } else {
        NULL
      }
      
      rankings <- if (!is.null(query_data) && nrow(query_data) > 0) {
        query_data %>%
          group_by(brand_name, brand_id, main_brand_flag) %>%
          filter(date == max(date)) %>%
          ungroup() %>%
          arrange(desc(airr_score))
      } else {
        NULL
      }
      
      generate_prompt_report(
        brand_name = rv$brand_name,
        prompt_text = input$dash_query_select,
        ai_summary = "",
        rankings_data = rankings,
        trend_plot = trend_plot,
        spider_plot = spider_plot,
        output_path = file
      )
      
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), type = "error", duration = 10)
    }, finally = {
      removeNotification(notif)
    })
  }
)