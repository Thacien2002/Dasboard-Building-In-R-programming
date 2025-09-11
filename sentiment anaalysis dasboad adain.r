library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(caret)
library(leaflet)
library(sf)
library(forecast)
library(ggcorrplot)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(viridis)
library(flexdashboard)
library(bslib)
library(waiter)
library(rmarkdown)

## ---------------------------
## UI COMPONENTS
## ---------------------------

# Custom CSS with interpretation panels
custom_css <- "
  /* Modern styling with interpretation panels */
  .interpretation-panel {
    background-color: #f8f9fa;
    border-radius: 8px;
    padding: 15px;
    margin-top: 20px;
    border-left: 4px solid #3498db;
  }
  
  .interpretation-title {
    font-weight: bold;
    color: #2c3e50;
    margin-bottom: 10px;
  }
  
  /* Conversation cards */
  .conversation-card {
    border-left: 4px solid #ddd;
    margin-bottom: 15px;
    padding: 12px;
    border-radius: 4px;
    background-color: white;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  }
  
  .conversation-card.negative { border-left-color: #e74c3c; }
  .conversation-card.positive { border-left-color: #2ecc71; }
  .conversation-card.neutral { border-left-color: #f39c12; }
  
  .conversation-time {
    font-size: 0.85em;
    color: #7f8c8d;
    margin-bottom: 5px;
  }
  
  .conversation-topic {
    font-size: 0.8em;
    color: #3498db;
    text-align: right;
    margin-top: 5px;
  }
"

header <- dashboardHeader(
  title = div(
    img(src = "https://via.placeholder.com/40x40?text=DA", height = "40px"),
    span("Advanced Data Analytics", style = "font-weight:300;")
  ),
  titleWidth = 300,
  dropdownMenu(
    type = "notifications", 
    icon = icon("cog"),
    badgeStatus = NULL,
    headerText = "Dashboard Controls",
    notificationItem(
      text = div(
        materialSwitch(
          inputId = "dark_mode",
          label = "Dark Mode", 
          status = "primary",
          value = FALSE
        )
      ),
      icon = icon("circle")
    )
  )
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("tachometer-alt")),
    menuItem("Data Explorer", tabName = "explore", icon = icon("search")),
    menuItem("Visual Analytics", tabName = "visualize", icon = icon("chart-line")),
    menuItem("Geospatial", tabName = "geo", icon = icon("map")),
    menuItem("Time Series", tabName = "timeseries", icon = icon("clock")),
    menuItem("Predictive Models", tabName = "ml", icon = icon("robot")),
    menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
    hr(),
    div(
      style = "padding:15px;",
      fileInput("file_input", "Upload Data", 
                accept = c(".csv", ".xlsx", ".json", ".geojson"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      actionButton("sample_data", "Use Sample Data", 
                   icon = icon("database"),
                   class = "btn-primary",
                   style = "width:100%;")
    )
  )
)

body <- dashboardBody(
  useShinyjs(),
  useWaiter(),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  tabItems(
    ## Dashboard Overview Tab
    tabItem(
      tabName = "dashboard",
      fluidRow(
        valueBoxOutput("data_summary_box", width = 4),
        valueBoxOutput("variables_box", width = 4),
        valueBoxOutput("completeness_box", width = 4)
      ),
      fluidRow(
        box(
          title = "Quick Insights", 
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          div(
            style = "margin-bottom:15px;",
            selectInput("quick_insight_var", "Select Variable:", choices = NULL)
          ),
          fluidRow(
            column(
              width = 6,
              plotlyOutput("quick_plot") %>% withSpinner()
            ),
            column(
              width = 6,
              div(
                class = "interpretation-panel",
                div(class = "interpretation-title", "Insights"),
                textOutput("quick_insight_text")
              ),
              verbatimTextOutput("quick_summary")
            )
          )
        )
      )
    ),
    
    ## Data Explorer Tab
    tabItem(
      tabName = "explore",
      fluidRow(
        box(
          title = "Variable Explorer", 
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          selectInput("explore_var", "Select Variable:", choices = NULL),
          radioButtons("explore_type", "Visualization Type:",
                      choices = c("Distribution" = "hist", 
                                 "Density" = "density",
                                 "Boxplot" = "box")),
          colourInput("explore_color", "Select Color:", "#3498db")
        ),
        box(
          title = "Visualization", 
          width = 8,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("explore_plot", height = "400px") %>% withSpinner(),
          div(
            class = "interpretation-panel",
            div(class = "interpretation-title", "Interpretation"),
            textOutput("explore_interpretation")
          )
        )
      ),
      fluidRow(
        box(
          title = "Data Quality Analysis", 
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("missing_plot") %>% withSpinner(),
          div(
            class = "interpretation-panel",
            div(class = "interpretation-title", "Data Quality Assessment"),
            textOutput("data_quality_text")
          )
        )
      )
    ),
    
    ## Visualization Tab
    tabItem(
      tabName = "visualize",
      fluidRow(
        box(
          title = "Chart Controls", 
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectInput("chart_x", "X Variable:", choices = NULL),
          selectInput("chart_y", "Y Variable:", choices = NULL),
          selectInput("chart_type", "Chart Type:",
                     choices = c("Scatter", "Line", "Bar", "Boxplot")),
          colourInput("chart_color", "Color:", "#3498db"),
          checkboxInput("chart_trend", "Add Trend Line", FALSE)
        ),
        box(
          title = "Interactive Visualization", 
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("main_chart", height = "500px") %>% withSpinner(),
          div(
            class = "interpretation-panel",
            div(class = "interpretation-title", "Visual Analysis"),
            textOutput("chart_interpretation")
          )
        )
      )
    ),
    
    ## Geospatial Tab
    tabItem(
      tabName = "geo",
      fluidRow(
        box(
          title = "Map Controls", 
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectInput("map_var", "Value Variable:", choices = NULL),
          selectInput("map_type", "Map Type:",
                     choices = c("Point" = "point", 
                                "Choropleth" = "choropleth",
                                "Heatmap" = "heat")),
          selectInput("map_palette", "Color Palette:",
                     choices = c("Viridis", "Plasma", "Magma", "Inferno"))
        ),
        box(
          title = "Interactive Map", 
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          leafletOutput("geo_map", height = "600px") %>% withSpinner(),
          div(
            class = "interpretation-panel",
            div(class = "interpretation-title", "Geospatial Insights"),
            textOutput("map_interpretation")
          )
        )
      )
    ),
    
    ## Time Series Tab
    tabItem(
      tabName = "timeseries",
      fluidRow(
        box(
          title = "Time Series Controls", 
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectInput("ts_var", "Value Variable:", choices = NULL),
          selectInput("ts_date", "Date Variable:", choices = NULL),
          selectInput("ts_model", "Forecast Model:",
                     choices = c("Auto ARIMA", "ETS", "Neural Network")),
          numericInput("forecast_periods", "Forecast Periods:", 12, min = 1, max = 36)
        ),
        box(
          title = "Time Series Analysis", 
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              "Forecast",
              plotlyOutput("ts_plot", height = "400px") %>% withSpinner(),
              div(
                class = "interpretation-panel",
                div(class = "interpretation-title", "Forecast Interpretation"),
                textOutput("forecast_interpretation")
              )
            ),
            tabPanel(
              "Decomposition",
              plotlyOutput("ts_decomposition") %>% withSpinner()
            )
          )
        )
      )
    ),
    
    ## Machine Learning Tab
    tabItem(
      tabName = "ml",
      fluidRow(
        box(
          title = "Model Configuration", 
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectInput("ml_target", "Target Variable:", choices = NULL),
          selectizeInput("ml_features", "Features:", choices = NULL, multiple = TRUE),
          selectInput("ml_algorithm", "Algorithm:",
                     choices = c("Random Forest" = "rf",
                               "Linear Regression" = "lm",
                               "Gradient Boosting" = "gbm")),
          sliderInput("train_split", "Train/Test Split:", 70, 90, 80)
        ),
        box(
          title = "Model Performance", 
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              "Results",
              plotlyOutput("ml_plot", height = "300px") %>% withSpinner(),
              verbatimTextOutput("ml_summary"),
              div(
                class = "interpretation-panel",
                div(class = "interpretation-title", "Model Assessment"),
                textOutput("model_interpretation")
              )
            ),
            tabPanel(
              "Feature Importance",
              plotlyOutput("feature_importance") %>% withSpinner()
            )
          )
        )
      )
    ),
    
    ## Reports Tab
    tabItem(
      tabName = "reports",
      fluidRow(
        box(
          title = "Report Configuration", 
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          selectInput("report_type", "Report Type:",
                     choices = c("Comprehensive Analysis" = "full",
                               "Executive Summary" = "summary",
                               "Technical Deep Dive" = "technical")),
          textInput("report_title", "Report Title:", "Data Analysis Report"),
          selectInput("report_format", "Format:",
                     choices = c("HTML", "PDF", "Word")),
          downloadButton("download_report", "Generate Report",
                         class = "btn-success")
        ),
        box(
          title = "Report Preview", 
          width = 8,
          status = "primary",
          solidHeader = TRUE,
          htmlOutput("report_preview") %>% withSpinner()
        )
      )
    )
  )
)

## ---------------------------
## SERVER LOGIC
## ---------------------------

server <- function(input, output, session) {
  
  # Initialize waiter
  waiter <- Waiter$new()
  
  # Toggle dark mode
  observeEvent(input$dark_mode, {
    if (input$dark_mode) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  # Reactive data storage
  rv <- reactiveValues(
    data = NULL,
    geo_data = NULL,
    model = NULL,
    forecast = NULL
  )
  
  # Load sample data
  observeEvent(input$sample_data, {
    waiter$show()
    on.exit(waiter$hide())
    
    rv$data <- diamonds
    showNotification("Sample diamonds data loaded successfully", type = "message")
  })
  
  # Handle file uploads
  observeEvent(input$file_input, {
    waiter$show()
    on.exit(waiter$hide())
    
    req(input$file_input)
    ext <- tools::file_ext(input$file_input$name)
    
    tryCatch({
      if (ext == "csv") {
        rv$data <- read_csv(input$file_input$datapath)
      } else if (ext == "xlsx") {
        rv$data <- read_excel(input$file_input$datapath)
      } else if (ext == "json") {
        rv$data <- jsonlite::fromJSON(input$file_input$datapath)
      } else if (ext == "geojson") {
        rv$geo_data <- st_read(input$file_input$datapath)
        rv$data <- st_drop_geometry(rv$geo_data)
      }
      showNotification("Data uploaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Update select inputs when data changes
  observe({
    req(rv$data)
    df <- rv$data
    
    # Update all select inputs
    updateSelectInput(session, "quick_insight_var", choices = names(df))
    updateSelectInput(session, "explore_var", choices = names(df))
    updateSelectInput(session, "chart_x", choices = names(df))
    updateSelectInput(session, "chart_y", choices = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, "ts_var", choices = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, "ts_date", choices = names(df)[sapply(df, function(x) inherits(x, "Date"))])
    updateSelectInput(session, "ml_target", choices = names(df)[sapply(df, is.numeric)])
    updateSelectizeInput(session, "ml_features", choices = names(df)[names(df) != input$ml_target])
    
    if (!is.null(rv$geo_data)) {
      updateSelectInput(session, "map_var", choices = names(rv$geo_data)[sapply(rv$geo_data, is.numeric)])
    }
  })
  
  ## Dashboard Overview Outputs
  output$data_summary_box <- renderValueBox({
    req(rv$data)
    valueBox(
      paste(nrow(rv$data), "Records"), 
      "Total Observations",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$variables_box <- renderValueBox({
    req(rv$data)
    valueBox(
      ncol(rv$data), 
      "Variables",
      icon = icon("columns"),
      color = "green"
    )
  })
  
  output$completeness_box <- renderValueBox({
    req(rv$data)
    missing <- sum(is.na(rv$data))
    pct_missing <- round(missing / (nrow(rv$data) * ncol(rv$data)) * 100, 1)
    
    valueBox(
      paste0(pct_missing, "% Complete"), 
      "Data Quality",
      icon = icon(ifelse(pct_missing > 95, "check-circle", "exclamation-triangle")),
      color = ifelse(pct_missing > 95, "green", "orange")
    )
  })
  
  output$quick_plot <- renderPlotly({
    req(rv$data, input$quick_insight_var)
    var <- rv$data[[input$quick_insight_var]]
    
    if (is.numeric(var)) {
      p <- ggplot(rv$data, aes(x = !!sym(input$quick_insight_var))) +
        geom_histogram(fill = "#3498db", bins = 30) +
        labs(title = paste("Distribution of", input$quick_insight_var)) +
        theme_minimal()
    } else {
      p <- rv$data %>%
        count(!!sym(input$quick_insight_var)) %>%
        ggplot(aes(x = reorder(!!sym(input$quick_insight_var), n), y = n)) +
        geom_bar(stat = "identity", fill = "#3498db") +
        coord_flip() +
        labs(title = paste("Frequency of", input$quick_insight_var), x = "", y = "Count")
    }
    
    ggplotly(p)
  })
  
  output$quick_insight_text <- renderText({
    req(rv$data, input$quick_insight_var)
    var <- rv$data[[input$quick_insight_var]]
    
    if (is.numeric(var)) {
      stats <- summary(var)
      skew <- moments::skewness(var, na.rm = TRUE)
      
      paste("The variable", input$quick_insight_var, "has an average value of", 
            round(mean(var, na.rm = TRUE), 2), "with values ranging from", 
            round(min(var, na.rm = TRUE), 2), "to", round(max(var, na.rm = TRUE), 2),
            ifelse(abs(skew) > 1, "The distribution is highly skewed.", 
                   ifelse(abs(skew) > 0.5, "The distribution is moderately skewed.",
                          "The distribution is approximately symmetric.")))
    } else {
      top_val <- names(sort(table(var), decreasing = TRUE))[1]
      top_pct <- round(100 * max(table(var)) / length(var), 1)
      
      paste("The most frequent category in", input$quick_insight_var, "is", 
            top_val, "which appears in", top_pct, "% of records. There are",
            n_distinct(var), "unique categories in this variable.")
    }
  })
  
  output$quick_summary <- renderPrint({
    req(rv$data, input$quick_insight_var)
    summary(rv$data[[input$quick_insight_var]])
  })
  
  ## Data Explorer Outputs
  output$explore_plot <- renderPlotly({
    req(rv$data, input$explore_var)
    var <- rv$data[[input$explore_var]]
    
    if (input$explore_type == "hist" && is.numeric(var)) {
      p <- ggplot(rv$data, aes(x = !!sym(input$explore_var))) +
        geom_histogram(fill = input$explore_color, bins = 30) +
        labs(title = paste("Distribution of", input$explore_var))
    } else if (input$explore_type == "density" && is.numeric(var)) {
      p <- ggplot(rv$data, aes(x = !!sym(input$explore_var))) +
        geom_density(fill = input$explore_color, alpha = 0.7) +
        labs(title = paste("Density of", input$explore_var))
    } else if (input$explore_type == "box" && is.numeric(var)) {
      p <- ggplot(rv$data, aes(y = !!sym(input$explore_var))) +
        geom_boxplot(fill = input$explore_color) +
        labs(title = paste("Boxplot of", input$explore_var))
    } else {
      # Default for categorical variables
      p <- rv$data %>%
        count(!!sym(input$explore_var)) %>%
        ggplot(aes(x = reorder(!!sym(input$explore_var), n), y = n)) +
        geom_bar(stat = "identity", fill = input$explore_color) +
        coord_flip() +
        labs(title = paste("Frequency of", input$explore_var), x = "", y = "Count")
    }
    
    p <- p + theme_minimal()
    ggplotly(p)
  })
  
  output$explore_interpretation <- renderText({
    req(rv$data, input$explore_var)
    var <- rv$data[[input$explore_var]]
    
    if (is.numeric(var)) {
      stats <- summary(var)
      outliers <- boxplot.stats(var)$out
      
      paste("The variable", input$explore_var, "shows a", 
            ifelse(mean(var, na.rm = TRUE) > median(var, na.rm = TRUE), 
                   "right-skewed distribution", 
                   ifelse(mean(var, na.rm = TRUE) < median(var, na.rm = TRUE),
                          "left-skewed distribution",
                          "symmetric distribution")),
            "with", length(outliers), "potential outliers. The interquartile range (IQR) is",
            round(stats["3rd Qu."] - stats["1st Qu."], 2), "units.")
    } else {
      top_3 <- names(sort(table(var), decreasing = TRUE))[1:3]
      top_pct <- round(100 * table(var)[top_3] / length(var), 1)
      
      paste("The top 3 categories in", input$explore_var, "are:", 
            paste(top_3, "(", top_pct, "%)", collapse = ", "),
            "accounting for", sum(top_pct), "% of all values.")
    }
  })
  
  output$missing_plot <- renderPlotly({
    req(rv$data)
    missing_data <- rv$data %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "missing") %>%
      arrange(desc(missing))
    
    p <- ggplot(missing_data, aes(x = reorder(variable, missing), y = missing)) +
      geom_bar(stat = "identity", fill = "#e74c3c") +
      coord_flip() +
      labs(x = "", y = "Missing Values") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$data_quality_text <- renderText({
    req(rv$data)
    missing <- sum(is.na(rv$data))
    total_cells <- nrow(rv$data) * ncol(rv$data)
    pct_missing <- round(100 * missing / total_cells, 1)
    
    most_missing <- names(sort(sapply(rv$data, function(x) sum(is.na(x))), decreasing = TRUE))[1]
    missing_in_var <- sum(is.na(rv$data[[most_missing]]))
    
    paste("The dataset contains", missing, "missing values (", pct_missing, "% of all cells).",
          "The variable with the most missing values is", most_missing, "with",
          missing_in_var, "NA values. Consider examining this variable closely",
          "for potential data quality issues.")
  })
  
  ## Visualization Outputs
  output$main_chart <- renderPlotly({
    req(rv$data, input$chart_x, input$chart_y)
    
    if (input$chart_type == "Scatter") {
      p <- ggplot(rv$data, aes_string(x = input$chart_x, y = input$chart_y)) +
        geom_point(color = input$chart_color, alpha = 0.7)
    } else if (input$chart_type == "Line") {
      p <- ggplot(rv$data, aes_string(x = input$chart_x, y = input$chart_y)) +
        geom_line(color = input$chart_color, size = 1)
    } else if (input$chart_type == "Bar") {
      p <- ggplot(rv$data, aes_string(x = input$chart_x, y = input$chart_y)) +
        geom_bar(stat = "identity", fill = input$chart_color)
    } else if (input$chart_type == "Boxplot") {
      p <- ggplot(rv$data, aes_string(x = input$chart_x, y = input$chart_y)) +
        geom_boxplot(fill = input$chart_color)
    }
    
    if (input$chart_trend && input$chart_type %in% c("Scatter", "Line")) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    p <- p + labs(title = paste(input$chart_type, "of", input$chart_y, "by", input$chart_x)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$chart_interpretation <- renderText({
    req(rv$data, input$chart_x, input$chart_y)
    
    x_var <- rv$data[[input$chart_x]]
    y_var <- rv$data[[input$chart_y]]
    
    if (input$chart_type == "Scatter") {
      corr <- cor(x_var, y_var, use = "complete.obs")
      
      paste("The scatter plot shows", 
            ifelse(abs(corr) > 0.7, "a strong",
                   ifelse(abs(corr) > 0.3, "a moderate", "a weak")),
            ifelse(corr > 0, "positive", "negative"),
            "relationship between", input$chart_x, "and", input$chart_y,
            "(r =", round(corr, 2), ").",
            ifelse(input$chart_trend, "The trend line confirms this relationship.", ""))
    } else if (input$chart_type == "Line") {
      paste("The line chart displays the trend of", input$chart_y, 
            "across different values of", input$chart_x, ".",
            ifelse(input$chart_trend, "The trend line shows the overall direction of change.", ""))
    } else if (input$chart_type == "Bar") {
      paste("The bar chart compares values of", input$chart_y, 
            "across different categories of", input$chart_x)
    } else if (input$chart_type == "Boxplot") {
      paste("The boxplot shows the distribution of", input$chart_y, 
            "for different categories of", input$chart_x,
            "including median, quartiles, and potential outliers.")
    }
  })
  
  ## Geospatial Outputs
  output$geo_map <- renderLeaflet({
    req(rv$geo_data, input$map_var)
    
    geo_df <- rv$geo_data
    var <- geo_df[[input$map_var]]
    
    if (input$map_type == "point") {
      leaflet(geo_df) %>%
        addTiles() %>%
        addCircleMarkers(
          radius = 6,
          color = "#3498db",
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = ~paste(input$map_var, ":", round(var, 2))
        )
    } else if (input$map_type == "choropleth") {
      pal <- colorNumeric(
        palette = tolower(input$map_palette),
        domain = var
      )
      
      leaflet(geo_df) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(var),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = ~paste(input$map_var, ":", round(var, 2))
        ) %>%
        addLegend(
          pal = pal,
          values = ~var,
          opacity = 0.7,
          title = input$map_var,
          position = "bottomright"
        )
    } else if (input$map_type == "heat") {
      leaflet(geo_df) %>%
        addTiles() %>%
        addHeatmap(
          intensity = ~var,
          radius = 20,
          blur = 15,
          max = 0.05
        )
    }
  })
  
  output$map_interpretation <- renderText({
    req(rv$geo_data, input$map_var)
    
    var <- rv$geo_data[[input$map_var]]
    stats <- summary(var)
    
    paste("The map displays spatial variation in", input$map_var, "with values ranging from",
          round(stats["Min."], 2), "to", round(stats["Max."], 2), "and a median of",
          round(stats["Median"], 2), ".", 
          ifelse(input$map_type == "choropleth", 
                 "The color gradient highlights areas with higher and lower values.",
                 ifelse(input$map_type == "heat",
                        "The heatmap shows concentrations of high values.",
                        "Point markers indicate specific locations with their values.")))
  })
  
  ## Time Series Outputs
  output$ts_plot <- renderPlotly({
    req(rv$data, input$ts_var, input$ts_date)
    
    df <- rv$data
    date_var <- df[[input$ts_date]]
    value_var <- df[[input$ts_var]]
    
    # Create time series object
    ts_data <- ts(value_var, frequency = 12) # Monthly frequency as default
    
    # Fit model
    model <- switch(input$ts_model,
                   "Auto ARIMA" = auto.arima(ts_data),
                   "ETS" = ets(ts_data),
                   "Neural Network" = nnetar(ts_data))
    
    # Forecast
    forecast_values <- forecast(model, h = input$forecast_periods)
    
    # Store model
    rv$forecast <- forecast_values
    
    # Prepare data for plotting
    plot_data <- data.frame(
      Date = c(date_var, max(date_var) + days(1:input$forecast_periods)),
      Value = c(as.numeric(ts_data), as.numeric(forecast_values$mean)),
      Type = c(rep("Actual", length(ts_data)), rep("Forecast", input$forecast_periods)),
      Lower = c(rep(NA, length(ts_data)), as.numeric(forecast_values$lower[,2])),
      Upper = c(rep(NA, length(ts_data)), as.numeric(forecast_values$upper[,2]))
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Date, y = Value, color = Type)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "#3498db") +
      labs(title = paste(input$ts_var, "Time Series Forecast"),
           x = "Date", y = input$ts_var) +
      scale_color_manual(values = c("Actual" = "#3498db", "Forecast" = "#e74c3c")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$forecast_interpretation <- renderText({
    req(rv$forecast)
    
    fc <- rv$forecast
    mean_fc <- round(mean(fc$mean), 2)
    range_fc <- paste(round(range(fc$mean), 2), collapse = " to ")
    
    paste("The forecast predicts an average value of", mean_fc, 
          "over the next", input$forecast_periods, "periods, with values ranging from",
          range_fc, ". The shaded area represents the 95% confidence interval.",
          "The model used was", input$ts_model, "which",
          ifelse(input$ts_model == "Auto ARIMA", "automatically selects the best ARIMA parameters.",
                 ifelse(input$ts_model == "ETS", "uses exponential smoothing state space model.",
                        "employs a neural network approach.")))
  })
  
  output$ts_decomposition <- renderPlotly({
    req(rv$data, input$ts_var, input$ts_date)
    
    df <- rv$data
    value_var <- df[[input$ts_var]]
    
    # Create time series object
    ts_data <- ts(value_var, frequency = 12)
    
    # Decompose
    decomp <- stl(ts_data, s.window = "periodic")
    
    autoplot(decomp) +
      theme_minimal()
  })
  
  ## Machine Learning Outputs
  observeEvent(input$run_model, {
    req(rv$data, input$ml_target, input$ml_features)
    
    df <- rv$data
    target <- input$ml_target
    features <- input$ml_features
    
    # Split data
    set.seed(123)
    train_index <- createDataPartition(df[[target]], p = input$train_split/100, list = FALSE)
    train_data <- df[train_index, c(features, target)]
    test_data <- df[-train_index, c(features, target)]
    
    # Train control
    train_control <- trainControl(
      method = "cv",
      number = 5,
      verboseIter = TRUE
    )
    
    # Train model
    rv$model <- train(
      as.formula(paste(target, "~ .")),
      data = train_data,
      method = input$ml_algorithm,
      trControl = train_control
    )
    
    # Store predictions
    predictions <- predict(rv$model, newdata = test_data)
    rv$predictions <- data.frame(
      Actual = test_data[[target]],
      Predicted = predictions,
      Residual = test_data[[target]] - predictions
    )
  })
  
  output$ml_plot <- renderPlotly({
    req(rv$predictions)
    
    p <- ggplot(rv$predictions, aes(x = Actual, y = Predicted)) +
      geom_point(color = "#3498db", alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed") +
      labs(title = "Actual vs Predicted Values") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$ml_summary <- renderPrint({
    req(rv$model)
    print(rv$model)
  })
  
  output$model_interpretation <- renderText({
    req(rv$model, rv$predictions)
    
    if (is.numeric(rv$predictions$Actual)) {
      metrics <- postResample(pred = rv$predictions$Predicted, obs = rv$predictions$Actual)
      
      paste("The", input$ml_algorithm, "model achieved an R-squared value of", 
            round(metrics["Rsquared"], 2), "and a root mean squared error (RMSE) of",
            round(metrics["RMSE"], 2), "on the test data.",
            ifelse(metrics["Rsquared"] > 0.7, 
                   "This indicates a strong predictive performance.",
                   ifelse(metrics["Rsquared"] > 0.4,
                          "This indicates moderate predictive performance.",
                          "This indicates weak predictive performance.")))
    } else {
      cm <- confusionMatrix(rv$predictions$Predicted, rv$predictions$Actual)
      accuracy <- round(cm$overall["Accuracy"], 2)
      
      paste("The classification model achieved an accuracy of", accuracy, 
            "on the test data.",
            ifelse(accuracy > 0.8, 
                   "This indicates excellent classification performance.",
                   ifelse(accuracy > 0.6,
                          "This indicates reasonable classification performance.",
                          "This indicates poor classification performance.")))
    }
  })
  
  output$feature_importance <- renderPlotly({
    req(rv$model)
    
    if (input$ml_algorithm %in% c("rf", "gbm")) {
      imp <- varImp(rv$model)$importance
      imp$Variable <- rownames(imp)
      
      p <- ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall)) +
        geom_bar(stat = "identity", fill = "#3498db") +
        coord_flip() +
        labs(x = "", y = "Importance") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      # Return empty plot for models without feature importance
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, 
                     label = "Feature importance not available\nfor this model type")) +
        theme_void()
      
      ggplotly(p)
    }
  })
  
  ## Reports Outputs
  output$report_preview <- renderUI({
    req(rv$data)
    
    HTML(paste(
      h3(input$report_title),
      hr(),
      h4("Dataset Overview"),
      p("Number of observations:", nrow(rv$data)),
      p("Number of variables:", ncol(rv$data)),
      p("Date generated:", format(Sys.Date(), "%B %d, %Y")),
      hr(),
      h4("Key Insights"),
      p("This preview shows the structure of your report. The full report will include:"),
      tags$ul(
        tags$li("Detailed data summary statistics"),
        tags$li("Visualizations with interpretations"),
        tags$li("Analysis results"),
        tags$li("Conclusions and recommendations")
      ),
      hr(),
      p("Click the 'Generate Report' button to create the full", input$report_type, "report.")
    ))
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("data-report-", Sys.Date(), 
            switch(input$report_format,
                  "HTML" = ".html",
                  "PDF" = ".pdf",
                  "Word" = ".docx"), 
            sep = "")
    },
    content = function(file) {
      # Create a temporary RMarkdown document
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Copy appropriate template based on report type
      if (input$report_type == "full") {
        file.copy("www/report_templates/full_template.Rmd", tempReport, overwrite = TRUE)
      } else if (input$report_type == "summary") {
        file.copy("www/report_templates/summary_template.Rmd", tempReport, overwrite = TRUE)
      } else {
        file.copy("www/report_templates/technical_template.Rmd", tempReport, overwrite = TRUE)
      }
      
      # Set up parameters to pass to Rmd document
      params <- list(
        report_title = input$report_title,
        dataset = rv$data,
        geo_data = rv$geo_data,
        model = rv$model,
        forecast = rv$forecast
      )
      
      # Knit the document
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        output_format = switch(input$report_format,
                             "HTML" = "html_document",
                             "PDF" = "pdf_document",
                             "Word" = "word_document")
      )
    }
  )
}

## ---------------------------
## RUN THE APPLICATION
## ---------------------------

shinyApp(ui = dashboardPage(header, sidebar, body), server = server)

