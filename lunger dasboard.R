library(shiny)
library(tidyverse)
library(plotly)
library(readxl)
library(janitor)

# Load and clean the dataset
data <- read_excel("C:\\Users\\harag\\OneDrive\\Desktop\\Project\\R DOCUMENT MATRIALS\\data plus R TRAINING\\R Training exercise.xlsx") %>%
  clean_names()

# Function to compute mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute missing values and label factors
data <- data %>%
  mutate(
    age = ifelse(is.na(age), median(age, na.rm = TRUE), age),
    weight = ifelse(is.na(weight), median(weight, na.rm = TRUE), weight),
    education_level = ifelse(is.na(education_level), get_mode(education_level), education_level),
    gender = ifelse(is.na(gender), get_mode(gender), gender),
    drink_alcohol = ifelse(is.na(drink_alcohol), get_mode(drink_alcohol), drink_alcohol),
    smoking = ifelse(is.na(smoking), get_mode(smoking), smoking),
    physical_exercises = ifelse(is.na(physical_exercises), get_mode(physical_exercises), physical_exercises)
  ) %>%
  mutate(
    marital_status = factor(marital_status, levels = 1:4, labels = c("Single", "Married", "Divorced", "Widowed")),
    education_level = factor(education_level, levels = 1:3, labels = c("Primary", "Secondary", "Tertiary")),
    lung_cancer = factor(lung_cancer, levels = 0:1, labels = c("No", "Yes")),
    gender = factor(gender, levels = 1:2, labels = c("Male", "Female")),
    drink_alcohol = factor(drink_alcohol, levels = 2:3, labels = c("No", "Yes")),
    smoking = factor(smoking, levels = 0:1, labels = c("No", "Yes")),
    physical_exercises = factor(physical_exercises, levels = 1:2, labels = c("No", "Yes"))
  )

# Logistic regression model
model <- glm(lung_cancer ~ age + gender + smoking + drink_alcohol + physical_exercises +
               education_level + marital_status + weight,
             data = data, family = binomial)

# UI Layout
ui <- fluidPage(
  titlePanel("Lung Cancer Prediction Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Risk Prediction Inputs"),
      numericInput("age", "Age:", value = 40, min = 15, max = 65),
      selectInput("gender", "Gender:", choices = levels(data$gender)),
      selectInput("smoking", "Smoking:", choices = levels(data$smoking)),
      selectInput("drink", "Drink Alcohol:", choices = levels(data$drink_alcohol)),
      selectInput("exercise", "Physical Exercise:", choices = levels(data$physical_exercises)),
      selectInput("edu", "Education Level:", choices = levels(data$education_level)),
      selectInput("marital", "Marital Status:", choices = levels(data$marital_status)),
      numericInput("weight", "Weight (kg):", value = 50, min = 1, max = 100),
      actionButton("predict", "Predict Lung Cancer Risk")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction Result",
                 verbatimTextOutput("predictionText"),
                 plotlyOutput("predictionPlot")
        ),
        tabPanel("Visualizations",
                 fluidRow(
                   column(6, plotlyOutput("ageBox")),
                   column(6, plotlyOutput("weightBox"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("smokeBar")),
                   column(6, plotlyOutput("genderBar"))
                 )
        ),
        tabPanel("Model Info",
                 verbatimTextOutput("modelSummary"),
                 tableOutput("oddsRatios")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  newdata <- reactive({
    tibble(
      age = input$age,
      gender = factor(input$gender, levels = levels(data$gender)),
      smoking = factor(input$smoking, levels = levels(data$smoking)),
      drink_alcohol = factor(input$drink, levels = levels(data$drink_alcohol)),
      physical_exercises = factor(input$exercise, levels = levels(data$physical_exercises)),
      education_level = factor(input$edu, levels = levels(data$education_level)),
      marital_status = factor(input$marital, levels = levels(data$marital_status)),
      weight = input$weight
    )
  })
  
  prediction <- eventReactive(input$predict, {
    predict(model, newdata = newdata(), type = "response")
  })
  
  output$predictionText <- renderPrint({
    req(prediction())
    cat("Predicted Risk of Lung Cancer:", round(prediction() * 100, 2), "%")
  })
  
  output$predictionPlot <- renderPlotly({
    req(prediction())
    prob <- prediction()
    df <- data.frame(
      Outcome = c("No", "Yes"),
      Probability = c(1 - prob, prob)
    )
    p <- ggplot(df, aes(x = Outcome, y = Probability, fill = Outcome)) +
      geom_col() + ylim(0, 1) + theme_minimal() +
      labs(title = "Lung Cancer Probability", y = "Probability")
    ggplotly(p)
  })
  
  output$ageBox <- renderPlotly({
    ggplotly(
      ggplot(data, aes(x = lung_cancer, y = age, fill = lung_cancer)) +
        geom_boxplot() + theme_minimal() + labs(title = "Age vs Lung Cancer")
    )
  })
  
  output$weightBox <- renderPlotly({
    ggplotly(
      ggplot(data, aes(x = lung_cancer, y = weight, fill = lung_cancer)) +
        geom_boxplot() + theme_minimal() + labs(title = "Weight vs Lung Cancer")
    )
  })
  
  output$smokeBar <- renderPlotly({
    df <- data %>% group_by(smoking, lung_cancer) %>% summarise(n = n())
    ggplotly(
      ggplot(df, aes(x = smoking, y = n, fill = lung_cancer)) +
        geom_col(position = "dodge") + theme_minimal() +
        labs(title = "Smoking vs Lung Cancer")
    )
  })
  
  output$genderBar <- renderPlotly({
    df <- data %>% group_by(gender, lung_cancer) %>% summarise(n = n())
    ggplotly(
      ggplot(df, aes(x = gender, y = n, fill = lung_cancer)) +
        geom_col(position = "dodge") + theme_minimal() +
        labs(title = "Gender vs Lung Cancer")
    )
  })
  
  output$modelSummary <- renderPrint({
    summary(model)
  })
  
  output$oddsRatios <- renderTable({
    exp(cbind(OddsRatio = coef(model), confint(model)))
  }, rownames = TRUE)
}

# Run the app
shinyApp(ui, server)
