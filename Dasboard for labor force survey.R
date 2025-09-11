getwd()
setwd("C:\\Users\\harag\\OneDrive\\Desktop\\Project")
getwd()
# Interactive R Shiny Dashboard for Youth Employment Survival Analysis

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(readstata13)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Youth Employment Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file", "Upload Stata Dataset (.dta)", accept = ".dta"),
      actionButton("loadLocal", "Load Local Dataset"),
      menuItem("Summary Plots", tabName = "summary", icon = icon("bar-chart")),
      menuItem("Employment Analysis", tabName = "employment", icon = icon("chart-pie")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                box(plotOutput("genderPlot"), width = 6),
                box(plotOutput("eduPlot"), width = 6),
                box(plotOutput("tvetPlot"), width = 6),
                box(plotOutput("residencePlot"), width = 6)
              )
      ),
      tabItem(tabName = "employment",
              fluidRow(
                box(plotOutput("empGender"), width = 6),
                box(plotOutput("empResidence"), width = 6),
                box(plotOutput("empEdu"), width = 6),
                box(plotOutput("empTvet"), width = 6)
              )
      ),
      tabItem(tabName = "survival",
              fluidRow(
                box(plotOutput("kmPlot"), width = 12),
                verbatimTextOutput("coxModel")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  df_global <- reactiveVal()
  
  observeEvent(input$file, {
    df <- read.dta13(input$file$datapath)
    df_global(df)
  })
  
  observeEvent(input$loadLocal, {
    path <- "C:/Users/harag/OneDrive/Documents/Dissenation group 5 veno thacien/university graduated 2023 dissetation.dta"
    df <- read.dta13(path)
    df_global(df)
  })
  
  dataInput <- reactive({
    df <- df_global()
    req(df)
    
    mode_replace <- function(x) {
      ux <- na.omit(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    replace_cols <- c("A01", "C01", "B08", "B13", "B14", "B02A", "B03", "Code_UR")
    for (col in replace_cols) {
      df[[col]][is.na(df[[col]])] <- mode_replace(df[[col]])
    }
    
    df <- df %>%
      mutate(
        employed = ifelse(C01 == 1, 1, 0),
        university_grad = ifelse(B02A == 6, 1, 0),
        sex_female = ifelse(A01 == 2, 1, 0),
        tvet_training = ifelse(B08 == 1, 1, 0),
        training_cert = ifelse(B13 == 1, 1, 0),
        outcome_job = ifelse(B14 == 3, 1, 0),
        degree_A0 = ifelse(B03 == 6, 1, 0),
        urban = ifelse(Code_UR == 1, 1, 0),
        time_to_getjob_aftergraduation = year_starting_job - B13A
      )
    
    df
  })
  
  output$genderPlot <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(A01))) +
      geom_bar(fill = "#1f77b4") +
      labs(title = "Gender Distribution", x = "Sex", y = "Count") +
      theme_minimal()
  })
  
  output$eduPlot <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(B02A))) +
      geom_bar(fill = "#2ca02c") +
      labs(title = "Education Level", x = "Level", y = "Count") +
      theme_minimal()
  })
  
  output$tvetPlot <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(B08))) +
      geom_bar(fill = "#d62728") +
      labs(title = "TVET Participation", x = "TVET", y = "Count") +
      theme_minimal()
  })
  
  output$residencePlot <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(Code_UR))) +
      geom_bar(fill = "#bcbd22") +
      labs(title = "Residence", x = "Location", y = "Count") +
      theme_minimal()
  })
  
  output$empGender <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(A01), fill = factor(C01))) +
      geom_bar(position = "fill") +
      labs(title = "Employment by Gender", x = "Sex", y = "Proportion") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()
  })
  
  output$empResidence <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(Code_UR), fill = factor(C01))) +
      geom_bar(position = "fill") +
      labs(title = "Employment by Residence", x = "Residence", y = "Proportion") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()
  })
  
  output$empEdu <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(B02A), fill = factor(C01))) +
      geom_bar(position = "fill") +
      labs(title = "Employment by Education Level", x = "Education", y = "Proportion") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()
  })
  
  output$empTvet <- renderPlot({
    df <- dataInput()
    ggplot(df, aes(x = factor(B08), fill = factor(C01))) +
      geom_bar(position = "fill") +
      labs(title = "Employment by TVET Participation", x = "TVET", y = "Proportion") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()
  })
  
  output$kmPlot <- renderPlot({
    df <- dataInput()
    surv_object <- Surv(df$time_to_getjob_aftergraduation, df$employed)
    km_fit <- survfit(surv_object ~ university_grad, data = df)
    ggsurvplot(km_fit, data = df, pval = TRUE, conf.int = TRUE,
               title = "Kaplan-Meier Curve by University Graduate")
  })
  
  output$coxModel <- renderPrint({
    df <- dataInput()
    surv_object <- Surv(df$time_to_getjob_aftergraduation, df$employed)
    model <- coxph(surv_object ~ A01 + A04 + A05 + disable + university_grad +
                     tvet_training + training_cert + outcome_job + urban, data = df)
    summary(model)
  })
}

shinyApp(ui, server)
