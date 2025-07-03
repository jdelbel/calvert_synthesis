library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(lubridate)
library(tidyr)

# UI
ui <- fluidPage(
  titlePanel("Subsurface Chlorophyll Accumulation Analyzer"),
  
  fluidRow(
    column(3,
           wellPanel(
             h4("Data Selection"),
             dateInput("selected_date", "Select Date:", 
                       value = Sys.Date(), format = "yyyy-mm-dd"),
             
             selectInput("selected_station", "Select Station:",
                         choices = NULL),
             
             hr(),
             
             # File upload
             fileInput("data_file", "Upload Data (CSV)",
                       accept = c(".csv")),
             
             helpText("Expected columns: date, station, pres, f_npq"),
             
             hr(),
             
             # Analysis controls
             h4("Analysis Options"),
             numericInput("max_depth", "Maximum Depth (m):", 
                          value = 50, min = 10, max = 200),
             
             numericInput("surface_threshold", "Surface Threshold (m):", 
                          value = 2, min = 1, max = 5, step = 0.5),
             
             actionButton("analyze", "Run Analysis", 
                          class = "btn-primary")
           )
    ),
    
    column(9,
           tabsetPanel(
             tabPanel("Profile Visualization",
                      fluidRow(
                        column(12,
                               plotlyOutput("profile_plot", height = "500px")
                        )
                      ),
                      
                      fluidRow(
                        column(6,
                               h4("Key Metrics"),
                               DT::dataTableOutput("metrics_table")
                        ),
                        column(6,
                               h4("Detection Summary"),
                               verbatimTextOutput("detection_summary")
                        )
                      )
             ),
             
             tabPanel("All Results",
                      fluidRow(
                        column(12,
                               h4("Subsurface Accumulation Summary"),
                               DT::dataTableOutput("summary_table")
                        )
                      ),
                      
                      fluidRow(
                        column(6,
                               h4("Seasonal Distribution"),
                               plotlyOutput("seasonal_plot", height = "400px")
                        ),
                        column(6,
                               h4("Depth Distribution"),
                               plotlyOutput("depth_plot", height = "400px")
                        )
                      )
             ),
             
             tabPanel("Raw Data",
                      DT::dataTableOutput("raw_data_table")
             )
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    analysis_results = NULL,
    summary_results = NULL
  )
  
  # Your existing functions (included here for completeness)
  detect_subsurface_accumulation <- function(data, max_depth = 50, surface_threshold = 2) {
    data %>%
      group_by(date, station) %>%
      filter(pres >= 1, pres <= max_depth, !is.na(f_npq)) %>%
      arrange(pres) %>%
      filter(n() >= 6, max(pres) >= 8) %>%
      summarise(
        # Surface reference
        surface_chl = mean(f_npq[pres <= surface_threshold], na.rm = TRUE),
        near_surface_chl = mean(f_npq[pres <= 5], na.rm = TRUE),
        
        # Subsurface characteristics
        subsurface_max = max(f_npq[pres > surface_threshold], na.rm = TRUE),
        subsurface_max_depth = pres[pres > surface_threshold][which.max(f_npq[pres > surface_threshold])],
        
        # Integrated characteristics
        integrated_chl = sum(f_npq[pres <= max_depth] * c(diff(pres[pres <= max_depth])[1],
                                                          diff(pres[pres <= max_depth])),
                             na.rm = TRUE),
        surface_integrated = sum(f_npq[pres <= 5] * diff(pres)[1], na.rm = TRUE),
        subsurface_integrated = integrated_chl - surface_integrated,
        
        # Profile characteristics
        max_chl_overall = max(f_npq, na.rm = TRUE),
        max_depth_overall = pres[which.max(f_npq)],
        profile_mean = mean(f_npq[pres <= max_depth], na.rm = TRUE),
        
        # Biomass distribution metrics
        subsurface_fraction = subsurface_integrated / integrated_chl,
        surface_fraction = surface_integrated / integrated_chl,
        surface_representation = surface_chl / profile_mean,
        
        month = month(first(date)),
        .groups = 'drop'
      ) %>%
      mutate(
        # Calculate enhancement ratios
        subsurface_enhancement_2m = subsurface_max / surface_chl,
        subsurface_enhancement_5m = subsurface_max / near_surface_chl,
        
        # Key criteria for subsurface accumulation
        has_subsurface_accumulation = case_when(
          subsurface_max > 5.0 & 
            subsurface_enhancement_2m > 1.5 & 
            subsurface_fraction > 0.4 & 
            subsurface_max_depth > 5 ~ TRUE,
          
          subsurface_max > 2.0 &
            subsurface_enhancement_2m > 2.0 &
            subsurface_fraction > 0.5 &
            surface_representation < 0.7 ~ TRUE,
          
          subsurface_enhancement_2m > 3.0 &
            subsurface_fraction > 0.6 &
            subsurface_max_depth > 3 ~ TRUE,
          
          TRUE ~ FALSE
        ),
        
        # Classify type
        accumulation_type = case_when(
          !has_subsurface_accumulation ~ "none",
          subsurface_max > 10 ~ "high_biomass",
          subsurface_max > 5 ~ "moderate_biomass", 
          subsurface_enhancement_2m > 3 ~ "low_biomass",
          TRUE ~ "weak_subsurface"
        ),
        
        # Depth characteristics
        accumulation_depth = ifelse(has_subsurface_accumulation, subsurface_max_depth, NA)
      )
  }
  
  summarize_subsurface_events <- function(detection_results) {
    detection_results %>%
      mutate(
        season = case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring", 
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall"
        )
      ) %>%
      group_by(accumulation_type, season) %>%
      summarise(
        count = n(),
        mean_depth = round(mean(accumulation_depth, na.rm = TRUE), 1),
        mean_max_chl = round(mean(subsurface_max, na.rm = TRUE), 1),
        mean_surface_miss = round(mean(subsurface_enhancement_2m * 100, na.rm = TRUE), 1),
        mean_integrated = round(mean(integrated_chl, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(desc(mean_max_chl))
  }
  
  # Handle file upload
  observeEvent(input$data_file, {
    req(input$data_file)
    
    tryCatch({
      values$data <- read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
      values$data$date <- as.Date(values$data$date)
      
      # Update date and station choices
      available_dates <- sort(unique(values$data$date))
      available_stations <- sort(unique(values$data$station))
      
      updateDateInput(session, "selected_date", 
                      value = available_dates[1],
                      min = min(available_dates),
                      max = max(available_dates))
      
      updateSelectInput(session, "selected_station",
                        choices = available_stations,
                        selected = available_stations[1])
      
      showNotification("Data loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Run analysis
  observeEvent(input$analyze, {
    req(values$data)
    
    withProgress(message = 'Running analysis...', {
      values$analysis_results <- detect_subsurface_accumulation(
        values$data, 
        max_depth = input$max_depth,
        surface_threshold = input$surface_threshold
      )
      
      values$summary_results <- summarize_subsurface_events(values$analysis_results)
      
      showNotification("Analysis complete!", type = "message")
    })
  })
  
  # Profile plot
  output$profile_plot <- renderPlotly({
    req(values$data, input$selected_date, input$selected_station)
    
    profile_data <- values$data %>%
      filter(date == input$selected_date, station == input$selected_station) %>%
      arrange(pres)
    
    if(nrow(profile_data) == 0) {
      return(plotly_empty())
    }
    
    # Get analysis results for this profile if available
    analysis_data <- NULL
    if(!is.null(values$analysis_results)) {
      analysis_data <- values$analysis_results %>%
        filter(date == input$selected_date, station == input$selected_station)
    }
    
    p <- plot_ly(data = profile_data, x = ~f_npq, y = ~pres, 
                 type = 'scatter', mode = 'lines+markers',
                 line = list(color = 'green', width = 3),
                 marker = list(color = 'darkgreen', size = 6),
                 name = 'Chlorophyll Profile',
                 hovertemplate = paste('<b>Depth:</b> %{y:.1f} m<br>',
                                       '<b>Fluorescence:</b> %{x:.2f}<br>',
                                       '<extra></extra>')) %>%
      layout(
        title = paste("Chlorophyll Profile -", input$selected_station, "on", input$selected_date),
        xaxis = list(title = "Fluorescence (f_npq)"),
        yaxis = list(title = "Depth (m)", autorange = "reversed"),
        hovermode = 'closest'
      )
    
    # Add reference lines and annotations if analysis is available
    if(!is.null(analysis_data) && nrow(analysis_data) > 0) {
      # Add surface threshold line
      p <- p %>% add_trace(x = c(min(profile_data$f_npq, na.rm = TRUE), max(profile_data$f_npq, na.rm = TRUE)), 
                           y = c(input$surface_threshold, input$surface_threshold),
                           type = 'scatter', mode = 'lines',
                           line = list(color = "blue", dash = "dash", width = 2),
                           name = paste("Surface threshold (", input$surface_threshold, "m)"),
                           hoverinfo = 'name')
      
      # Add subsurface maximum point if present
      if(!is.na(analysis_data$subsurface_max_depth)) {
        p <- p %>% add_trace(x = analysis_data$subsurface_max, 
                             y = analysis_data$subsurface_max_depth,
                             type = 'scatter', mode = 'markers',
                             marker = list(color = "red", size = 12, symbol = "star"),
                             name = "Subsurface Maximum",
                             hovertemplate = paste('<b>Subsurface Max</b><br>',
                                                   'Depth: %{y:.1f} m<br>',
                                                   'Fluorescence: %{x:.2f}<br>',
                                                   '<extra></extra>'))
      }
    }
    
    p
  })
  
  # Metrics table
  output$metrics_table <- DT::renderDataTable({
    req(values$analysis_results, input$selected_date, input$selected_station)
    
    metrics <- values$analysis_results %>%
      filter(date == input$selected_date, station == input$selected_station) %>%
      select(surface_chl, near_surface_chl, subsurface_max, subsurface_max_depth,
             integrated_chl, subsurface_fraction, subsurface_enhancement_2m) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
      mutate(
        Metric = case_when(
          Metric == "surface_chl" ~ "Surface Chl (0-2m)",
          Metric == "near_surface_chl" ~ "Near-surface Chl (0-5m)",
          Metric == "subsurface_max" ~ "Subsurface Maximum",
          Metric == "subsurface_max_depth" ~ "Max Depth (m)",
          Metric == "integrated_chl" ~ "Integrated Chl",
          Metric == "subsurface_fraction" ~ "Subsurface Fraction",
          Metric == "subsurface_enhancement_2m" ~ "Enhancement Ratio",
          TRUE ~ Metric
        ),
        Value = round(Value, 3)
      )
    
    DT::datatable(metrics, options = list(dom = 't', pageLength = 20), rownames = FALSE)
  })
  
  # Detection summary
  output$detection_summary <- renderText({
    req(values$analysis_results, input$selected_date, input$selected_station)
    
    result <- values$analysis_results %>%
      filter(date == input$selected_date, station == input$selected_station)
    
    if(nrow(result) == 0) {
      return("No analysis results available for this date/station combination.")
    }
    
    paste(
      "=== SUBSURFACE ACCUMULATION ANALYSIS ===\n",
      "Station:", input$selected_station, "\n",
      "Date:", input$selected_date, "\n\n",
      
      "DETECTION RESULT:", result$has_subsurface_accumulation, "\n",
      "Type:", result$accumulation_type, "\n",
      "Depth:", ifelse(is.na(result$accumulation_depth), "N/A", paste(result$accumulation_depth, "m")), "\n\n",
      
      "BIOMASS METRICS:\n",
      "Surface chl (0-2m):", round(result$surface_chl, 2), "\n",
      "Subsurface maximum:", round(result$subsurface_max, 2), "\n",
      "Integrated chl:", round(result$integrated_chl, 2), "\n\n",
      
      "SURFACE MISS ANALYSIS:\n",
      "Enhancement ratio:", round(result$subsurface_enhancement_2m, 2), "x\n",
      "Subsurface biomass fraction:", round(result$subsurface_fraction * 100, 1), "%\n"
    )
  })
  
  # Summary table
  output$summary_table <- DT::renderDataTable({
    req(values$summary_results)
    
    DT::datatable(values$summary_results, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      formatRound(columns = c("mean_depth", "mean_max_chl", "mean_surface_miss", "mean_integrated"), digits = 2)
  })
  
  # Seasonal plot
  output$seasonal_plot <- renderPlotly({
    req(values$analysis_results)
    
    seasonal_data <- values$analysis_results %>%
      filter(has_subsurface_accumulation) %>%
      mutate(season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring", 
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall"
      )) %>%
      group_by(season, accumulation_type) %>%
      summarise(count = n(), .groups = 'drop')
    
    if(nrow(seasonal_data) == 0) {
      return(plotly_empty() %>% layout(title = "No subsurface accumulations detected"))
    }
    
    plot_ly(data = seasonal_data, x = ~season, y = ~count, color = ~accumulation_type,
            type = 'bar', 
            hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                  'Season: %{x}<br>',
                                  'Count: %{y}<br>',
                                  '<extra></extra>')) %>%
      layout(title = "Seasonal Distribution of Subsurface Accumulations",
             xaxis = list(title = "Season"),
             yaxis = list(title = "Count"),
             barmode = 'stack')
  })
  
  # Depth distribution plot
  output$depth_plot <- renderPlotly({
    req(values$analysis_results)
    
    depth_data <- values$analysis_results %>%
      filter(has_subsurface_accumulation, !is.na(accumulation_depth))
    
    if(nrow(depth_data) == 0) {
      return(plotly_empty() %>% layout(title = "No depth data available"))
    }
    
    plot_ly(data = depth_data, x = ~accumulation_depth, 
            type = 'histogram', nbinsx = 20,
            name = 'Accumulation Depth',
            hovertemplate = paste('<b>Depth Range:</b> %{x}<br>',
                                  '<b>Count:</b> %{y}<br>',
                                  '<extra></extra>')) %>%
      layout(title = "Distribution of Subsurface Accumulation Depths",
             xaxis = list(title = "Depth (m)"),
             yaxis = list(title = "Frequency"))
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    req(values$data)
    
    DT::datatable(values$data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  filter = 'top')
  })
}

# Run the app
shinyApp(ui = ui, server = server)