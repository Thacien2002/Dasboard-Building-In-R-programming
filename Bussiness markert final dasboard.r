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
library(colourpicker)
library(moments)
library(jsonlite)
library(readxl)

## ---------------------------
## UI COMPONENTS
## ---------------------------

# [Rest of your original code follows...]

## ---------------------------
## UI COMPONENTS
## ---------------------------

# Custom CSS for modern styling
custom_css <- "
  /* Modern styling */
  body { font-family: 'Segoe UI', Roboto, sans-serif; }
  
  /* Dashboard header */
  .skin-blue .main-header .logo {
    background-color: #2c3e50 !important;
    font-weight: 300;
    font-size: 1.2em;
  }
  
  /* Sidebar styling */
  .skin-blue .main-sidebar {
    background-color: #34495e !important;
  }
  
  /* Box styling */
  .box {
    border-radius: 8px;
    box-shadow: 0 4px 8px rgba(0,0,0,0.1);
    border-top: none;
  }
  
  .box-header {
    border-radius: 8px 8px 0 0;
    background-color: #f8f9fa !important;
    color: #333 !important;
    border-bottom: 1px solid #eee;
  }
  
  /* Value boxes */
  .value-box {
    border-radius: 8px;
  }
  
  /* Custom colors */
  .bg-blue { background-color: #3498db !important; }
  .bg-green { background-color: #2ecc71 !important; }
  .bg-purple { background-color: #9b59b6 !important; }
  .bg-orange { background-color: #e67e22 !important; }
  
  /* Hover effects */
  .hover-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 10px 20px rgba(0,0,0,0.1);
    transition: all 0.3s ease;
  }
  
  /* Button styling */
  .btn {
    border-radius: 20px;
    padding: 6px 12px;
  }
  
  /* Dark mode */
  .dark-mode {
    background-color: #222d32;
    color: #f4f4f4;
  }
  
  .dark-mode .box {
    background-color: #2d3b44;
    color: #f4f4f4;
  }
  
  .dark-mode .box-header {
    background-color: #34495e !important;
    color: #f4f4f4 !important;
  }
"

# Header with logo and controls
header <- dashboardHeader(
  title = div(
    img(src = "https://via.placeholder.com/40x40?text=DA", 
        height = "40px",
        style = "margin-right:10px;"),
    span("Data Analytics Dashboard", style = "font-weight:300;")
  ),
  titleWidth = 300,
  tags$li(
    div(
      materialSwitch(
        inputId = "dark_mode",
        label = NULL, 
        status = "primary",
        value = FALSE
      ),
      style = "padding:15px;"
    ),
    class = "dropdown"
  )
)

# Sidebar menu
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
    menuItem("Data", tabName = "data", icon = icon("database")),
    menuItem("Exploration", tabName = "explore", icon = icon("search")),
    menuItem("Visualization", tabName = "visualize", icon = icon("chart-bar")),
    menuItem("Geospatial", tabName = "geo", icon = icon("globe-americas")),
    menuItem("Time Series", tabName = "timeseries", icon = icon("clock")),
    menuItem("Machine Learning", tabName = "ml", icon = icon("robot")),
    menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
    hr(),
    div(
      style = "padding:15px;",
      h4("Data Controls"),
      fileInput("file_input", "Upload Data", 
                accept = c(".csv", ".xlsx", ".json", ".geojson"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      actionButton("sample_data", "Use Sample Data", 
                   icon = icon("cube"),
                   class = "btn-primary",
                   style = "width:100%;")
    )
  )
)

# Dashboard body
body <- dashboardBody(
  useShinyjs(),
  useWaiter(),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  tabItems(
    ## Dashboard Overview Tab
    tabItem(
      tabName = "dashboard",
      fluidRow(
        valueBoxOutput("vbox_rows", width = 3),
        valueBoxOutput("vbox_cols", width = 3),
        valueBoxOutput("vbox_numeric", width = 3),
        valueBoxOutput("vbox_missing", width = 3)
      ),
      fluidRow(
        box(
          title = "Quick Exploration", 
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              "Data Summary",
              verbatimTextOutput("data_summary")
            ),
            tabPanel(
              "Structure",
              verbatimTextOutput("data_structure")
            ),
            tabPanel(
              "Missing Data",
              plotlyOutput("missing_plot") %>% withSpinner()
            )
          )
        )
      )
    ),
    
    ## Data Upload/View Tab
    tabItem(
      tabName = "data",
      box(
        title = "Data Preview", 
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        DTOutput("data_table") %>% withSpinner()
      )
    ),
    
    ## Data Exploration Tab
    tabItem(
      tabName = "explore",
      fluidRow(
        box(
          title = "Variable Selector",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectInput("explore_var", "Select Variable:", choices = NULL),
          radioButtons("explore_type", "Plot Type:",
                       choices = c("Histogram", "Density", "Boxplot", "Barplot"),
                       selected = "Histogram"),
          colourInput("plot_color", "Select Color:", "#3498db"),
          sliderInput("plot_alpha", "Transparency:", 0.1, 1, 0.7)
        ),
        box(
          title = "Variable Exploration",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("explore_plot", height = "500px") %>% withSpinner(),
          verbatimTextOutput("explore_summary")
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
          selectInput("chart_group", "Group By:", choices = c("None")),
          selectInput("chart_type", "Chart Type:",
                       choices = c("Scatter", "Line", "Bar", "Boxplot", "Violin", "Density")),
          colourInput("chart_color", "Color:", "#3498db"),
          checkboxInput("chart_trend", "Add Trend Line", FALSE),
          actionButton("update_chart", "Update Chart", 
                       icon = icon("sync"),
                       class = "btn-primary")
        ),
        box(
          title = "Interactive Chart",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("main_chart", height = "600px") %>% withSpinner(),
          downloadButton("download_chart", "Download Chart")
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
                       choices = c("Point", "Choropleth", "Heatmap")),
          selectInput("map_palette", "Color Palette:",
                       choices = c("Viridis", "Plasma", "Magma", "Inferno")),
          checkboxInput("map_legend", "Show Legend", TRUE),
          actionButton("update_map", "Update Map", 
                       icon = icon("sync"),
                       class = "btn-primary")
        ),
        box(
          title = "Interactive Map",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          leafletOutput("geo_map", height = "600px") %>% withSpinner(),
          downloadButton("download_map", "Download Map")
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
          selectInput("ts_freq", "Frequency:",
                       choices = c("Auto", "Daily", "Weekly", "Monthly", "Yearly")),
          selectInput("ts_model", "Forecast Model:",
                       choices = c("Auto ARIMA", "ETS", "Neural Network")),
          numericInput("ts_forecast", "Forecast Periods:", 12, min = 1, max = 36),
          actionButton("run_forecast", "Run Forecast", 
                       icon = icon("play"),
                       class = "btn-primary")
        ),
        box(
          title = "Time Series Plot",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("ts_plot", height = "500px") %>% withSpinner()
        )
      ),
      fluidRow(
        box(
          title = "Model Summary",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          verbatimTextOutput("ts_summary")
        ),
        box(
          title = "Model Accuracy",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          verbatimTextOutput("ts_accuracy")
        )
      )
    ),
    
    ## Machine Learning Tab
    tabItem(
      tabName = "ml",
      fluidRow(
        box(
          title = "Model Controls",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectInput("ml_target", "Target Variable:", choices = NULL),
          selectizeInput("ml_features", "Features:", choices = NULL, multiple = TRUE),
          selectInput("ml_algorithm", "Algorithm:",
                       choices = c("Random Forest" = "rf",
                                 "Linear Regression" = "lm",
                                 "Gradient Boosting" = "gbm",
                                 "XGBoost" = "xgbTree")),
          sliderInput("ml_split", "Train/Test Split:", 70, 90, 80),
          actionButton("run_model", "Train Model", 
                       icon = icon("cogs"),
                       class = "btn-primary")
        ),
        box(
          title = "Model Performance",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              "Actual vs Predicted",
              plotlyOutput("ml_plot") %>% withSpinner()
            ),
            tabPanel(
              "Feature Importance",
              plotOutput("ml_importance") %>% withSpinner()
            ),
            tabPanel(
              "Residuals",
              plotOutput("ml_residuals") %>% withSpinner()
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Model Summary",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          verbatimTextOutput("ml_summary")
        )
      )
    ),
    
    ## Reports Tab
    tabItem(
      tabName = "reports",
      fluidRow(
        box(
          title = "Export Options",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          selectInput("export_type", "Export Type:",
                       choices = c("HTML Report", "PDF Report", "PowerPoint")),
          textInput("report_title", "Report Title:", "Data Analysis Report"),
          downloadButton("download_report", "Generate Report",
                         class = "btn-success")
        ),
        box(
          title = "Dashboard Settings",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          colourInput("primary_color", "Primary Color:", "#3498db"),
          selectInput("font_family", "Font Family:",
                       choices = c("Segoe UI", "Roboto", "Arial", "Helvetica")),
          actionButton("apply_settings", "Apply Settings",
                       class = "btn-primary")
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
    showNotification("Sample data loaded successfully", type = "message")
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
        rv$data <- fromJSON(input$file_input$datapath)
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
    
    # Update variable selectors
    updateSelectInput(session, "explore_var", choices = names(df))
    updateSelectInput(session, "chart_x", choices = names(df))
    updateSelectInput(session, "chart_y", choices = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, "chart_group", 
                      choices = c("None", names(df)[!sapply(df, is.numeric)]))
    updateSelectInput(session, "ts_var", 
                      choices = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, "ts_date", 
                      choices = names(df)[sapply(df, function(x) inherits(x, "Date"))])
    updateSelectInput(session, "ml_target", 
                      choices = names(df)[sapply(df, is.numeric)])
    updateSelectizeInput(session, "ml_features", 
                         choices = names(df)[names(df) != input$ml_target])
    
    # Update geospatial inputs if available
    if (!is.null(rv$geo_data)) {
      updateSelectInput(session, "map_var", 
                        choices = names(rv$geo_data)[sapply(rv$geo_data, is.numeric)])
    }
  })
  
  ## Dashboard Overview Outputs
  output$vbox_rows <- renderValueBox({
    req(rv$data)
    valueBox(
      nrow(rv$data), "Rows", 
      icon = icon("list-ol"),
      color = "blue"
    )
  })
  
  output$vbox_cols <- renderValueBox({
    req(rv$data)
    valueBox(
      ncol(rv$data), "Columns", 
      icon = icon("columns"),
      color = "green"
    )
  })
  
  output$vbox_numeric <- renderValueBox({
    req(rv$data)
    valueBox(
      sum(sapply(rv$data, is.numeric)), "Numeric Vars", 
      icon = icon("calculator"),
      color = "purple"
    )
  })
  
  output$vbox_missing <- renderValueBox({
    req(rv$data)
    missing <- sum(is.na(rv$data))
    valueBox(
      missing, "Missing Values", 
      icon = icon("question-circle"),
      color = ifelse(missing > 0, "orange", "green")
    )
  })
  
  output$data_summary <- renderPrint({
    req(rv$data)
    summary(rv$data)
  })
  
  output$data_structure <- renderPrint({
    req(rv$data)
    str(rv$data)
  })
  
  output$missing_plot <- renderPlotly({
    req(rv$data)
    missing_data <- rv$data %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "missing") %>%
      arrange(desc(missing))
    
    ggplot(missing_data, aes(x = reorder(variable, missing), y = missing)) +
      geom_bar(stat = "identity", fill = "#e74c3c") +
      coord_flip() +
      labs(x = "", y = "Missing Values") +
      theme_minimal()
  })
  
  ## Data Table Output
  output$data_table <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 500,
        scroller = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10
      )
    )
  })
  
  ## Exploration Outputs
  output$explore_plot <- renderPlotly({
    req(rv$data, input$explore_var)
    var <- rv$data[[input$explore_var]]
    
    if (is.numeric(var)) {
      if (input$explore_type == "Histogram") {
        p <- ggplot(rv$data, aes(x = !!sym(input$explore_var))) +
          geom_histogram(fill = input$plot_color, alpha = input$plot_alpha, bins = 30)
      } else if (input$explore_type == "Density") {
        p <- ggplot(rv$data, aes(x = !!sym(input$explore_var))) +
          geom_density(fill = input$plot_color, alpha = input$plot_alpha)
      } else if (input$explore_type == "Boxplot") {
        p <- ggplot(rv$data, aes(y = !!sym(input$explore_var))) +
          geom_boxplot(fill = input$plot_color, alpha = input$plot_alpha)
      }
    } else {
      p <- rv$data %>%
        count(!!sym(input$explore_var)) %>%
        ggplot(aes(x = reorder(!!sym(input$explore_var), n), y = n)) +
        geom_bar(stat = "identity", fill = input$plot_color, alpha = input$plot_alpha) +
        coord_flip()
    }
    
    p <- p + labs(title = paste(input$explore_type, "of", input$explore_var)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$explore_summary <- renderPrint({
    req(rv$data, input$explore_var)
    var <- rv$data[[input$explore_var]]
    
    if (is.numeric(var)) {
      cat("Numeric Variable Summary:\n")
      print(summary(var))
      cat("\nSkewness:", moments::skewness(var, na.rm = TRUE))
      cat("\nKurtosis:", moments::kurtosis(var, na.rm = TRUE))
    } else {
      cat("Categorical Variable Summary:\n")
      print(table(var))
      cat("\nUnique values:", n_distinct(var))
    }
    cat("\nMissing values:", sum(is.na(var)), 
        "(", round(100*sum(is.na(var))/length(var), 1), "%)")
  })
  
  ## Visualization Outputs
  output$main_chart <- renderPlotly({
    req(rv$data, input$chart_x, input$chart_y)
    
    p <- ggplot(rv$data, aes_string(x = input$chart_x, y = input$chart_y))
    
    if (input$chart_type == "Scatter") {
      if (input$chart_group != "None") {
        p <- p + geom_point(aes_string(color = input$chart_group), alpha = 0.7)
      } else {
        p <- p + geom_point(color = input$chart_color, alpha = 0.7)
      }
    } else if (input$chart_type == "Line") {
      if (input$chart_group != "None") {
        p <- p + geom_line(aes_string(color = input$chart_group), size = 1)
      } else {
        p <- p + geom_line(color = input$chart_color, size = 1)
      }
    } else if (input$chart_type == "Bar") {
      if (input$chart_group != "None") {
        p <- p + geom_bar(aes_string(fill = input$chart_group), stat = "identity", position = "dodge")
      } else {
        p <- p + geom_bar(fill = input$chart_color, stat = "identity")
      }
    } else if (input$chart_type == "Boxplot") {
      if (input$chart_group != "None") {
        p <- p + geom_boxplot(aes_string(fill = input$chart_group))
      } else {
        p <- p + geom_boxplot(fill = input$chart_color)
      }
    } else if (input$chart_type == "Violin") {
      if (input$chart_group != "None") {
        p <- p + geom_violin(aes_string(fill = input$chart_group))
      } else {
        p <- p + geom_violin(fill = input$chart_color)
      }
    } else if (input$chart_type == "Density") {
      if (input$chart_group != "None") {
        p <- ggplot(rv$data, aes_string(x = input$chart_y, fill = input$chart_group)) +
          geom_density(alpha = 0.5)
      } else {
        p <- ggplot(rv$data, aes_string(x = input$chart_y)) +
          geom_density(fill = input$chart_color, alpha = 0.7)
      }
    }
    
    if (input$chart_trend && input$chart_type %in% c("Scatter", "Line")) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    p <- p + labs(title = paste(input$chart_type, "Chart of", input$chart_y, "vs", input$chart_x)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  ## Geospatial Outputs
  output$geo_map <- renderLeaflet({
    req(rv$geo_data, input$map_var, input$update_map)
    
    geo_df <- rv$geo_data
    var <- geo_df[[input$map_var]]
    
    if (input$map_type == "Point") {
      leaflet(geo_df) %>%
        addTiles() %>%
        addCircleMarkers(
          radius = 6,
          color = input$primary_color,
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = ~paste(input$map_var, ":", var)
        )
    } else if (input$map_type == "Choropleth") {
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
          label = ~paste(input$map_var, ":", var)
        ) %>%
        addLegend(
          pal = pal,
          values = ~var,
          opacity = 0.7,
          title = input$map_var,
          position = "bottomright"
        )
    } else if (input$map_type == "Heatmap") {
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
  
  ## Time Series Outputs
  output$ts_plot <- renderPlotly({
    req(rv$data, input$ts_var, input$ts_date, input$run_forecast)
    
    df <- rv$data
    date_var <- df[[input$ts_date]]
    value_var <- df[[input$ts_var]]
    
    # Create time series object
    ts_data <- ts(value_var, frequency = switch(input$ts_freq,
                                              "Daily" = 365,
                                              "Weekly" = 52,
                                              "Monthly" = 12,
                                              "Yearly" = 1,
                                              1)) # Default to 1 if auto
    
    # Fit model
    model <- switch(input$ts_model,
                   "Auto ARIMA" = auto.arima(ts_data),
                   "ETS" = ets(ts_data),
                   "Neural Network" = nnetar(ts_data))
    
    # Forecast
    forecast_values <- forecast(model, h = input$ts_forecast)
    
    # Prepare data for plotting
    time_points <- time(ts_data)
    forecast_time <- seq(max(time_points), 
                        by = switch(input$ts_freq,
                                  "Daily" = 1/365,
                                  "Weekly" = 1/52,
                                  "Monthly" = 1/12,
                                  "Yearly" = 1,
                                  1), # Default to 1 if auto
                        length.out = input$ts_forecast + 1)[-1]
    
    plot_data <- data.frame(
      Time = c(time_points, forecast_time),
      Value = c(as.numeric(ts_data), as.numeric(forecast_values$mean)),
      Type = c(rep("Actual", length(ts_data)), rep("Forecast", input$ts_forecast)),
      Lower = c(rep(NA, length(ts_data)), as.numeric(forecast_values$lower[,2])),
      Upper = c(rep(NA, length(ts_data)), as.numeric(forecast_values$upper[,2]))
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Time, y = Value, color = Type)) +
      geom_line(data = subset(plot_data, Type == "Actual"), size = 1) +
      geom_line(data = subset(plot_data, Type == "Forecast"), size = 1, linetype = "dashed") +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = input$primary_color) +
      labs(title = paste(input$ts_var, "Time Series Forecast"),
           x = "Time", y = input$ts_var) +
      scale_color_manual(values = c("Actual" = input$primary_color, "Forecast" = "red")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$ts_summary <- renderPrint({
    req(rv$model)
    print(rv$model)
  })
  
  output$ts_accuracy <- renderPrint({
    req(rv$model)
    accuracy(rv$model)
  })
  
  ## Machine Learning Outputs
  observeEvent(input$run_model, {
    req(rv$data, input$ml_target, input$ml_features)
    
    df <- rv$data
    target <- input$ml_target
    features <- input$ml_features
    
    # Split data
    set.seed(123)
    train_index <- createDataPartition(df[[target]], p = input$ml_split/100, list = FALSE)
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
      geom_point(color = input$primary_color, alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Actual vs Predicted Values") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$ml_importance <- renderPlot({
    req(rv$model)
    
    if (input$ml_algorithm %in% c("rf", "gbm", "xgbTree")) {
      imp <- varImp(rv$model)
      ggplot(imp, top = 10) +
        theme_minimal()
    } else {
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
      text(0.5, 0.5, "Feature importance not available for this model type", cex = 1.2)
    }
  })
  
  output$ml_residuals <- renderPlot({
    req(rv$predictions)
    
    ggplot(rv$predictions, aes(x = Predicted, y = Residual)) +
      geom_point(color = input$primary_color, alpha = 0.6) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residuals Plot") +
      theme_minimal()
  })
  
  output$ml_summary <- renderPrint({
    req(rv$model)
    print(rv$model)
    cat("\n\nPerformance on Test Data:\n")
    if (is.numeric(rv$predictions$Actual)) {
      print(postResample(pred = rv$predictions$Predicted, obs = rv$predictions$Actual))
    } else {
      print(confusionMatrix(rv$predictions$Predicted, rv$predictions$Actual))
    }
  })
  
  ## Download Handlers
  output$download_chart <- downloadHandler(
    filename = function() {
      paste("chart-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      export(plotly::last_plot(), file = file)
    }
  )
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste("map-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      mapshot(leafletProxy("geo_map"), file = file)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), 
            switch(input$export_type,
                  "HTML Report" = ".html",
                  "PDF Report" = ".pdf",
                  "PowerPoint" = ".pptx"), 
            sep = "")
    },
    content = function(file) {
      # Create a temporary RMarkdown document
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        data = rv$data,
        geo_data = rv$geo_data,
        report_title = input$report_title,
        primary_color = input$primary_color
      )
      
      # Knit the document
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

## ---------------------------
## RUN THE APPLICATION
## ---------------------------

shinyApp(ui = dashboardPage(header, sidebar, body), server = server)

