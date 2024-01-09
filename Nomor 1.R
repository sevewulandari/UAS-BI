
library(shiny)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(PerformanceAnalytics)

# Data
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)
datafix = data[,c(2:7)]

model1 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
model <- lm(y ~ x2 + x5, data = data)

# Define UI
ui <- navbarPage(
  title = "Dashboard Data e-commerce company with operations across several Southeast Asian countries",
  theme = shinytheme("yeti"),
  
  # Multiple Linear Regresion (moved to the first position)
  tabPanel("Multiple Linear Regresion",
           fluidPage(
             titlePanel(title = div("Multiple Linear Regresion", style = "color: #ffff; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
             mainPanel(
               style = "margin: 0 auto; text-align: center; margin-left: 300px;",
               verbatimTextOutput("Regresion_output"),
             )
           )
  ),
  # corplot
  tabPanel("Correlation Plot",
           fluidPage(
             titlePanel(title = div("Correlation Plot", style = "color: #ffff; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
             mainPanel(
               style = "margin: 0 auto; text-align: center; margin-left: 300px;",
               plotOutput("correlation_plot" ,height = "800px", width = "100%") 
             )
           )
  ),
  
  
  # Variable Relationship
  tabPanel("Variable Relationship",
           fluidPage(
             titlePanel(title = div("Variable Relationship Analysis Dashboard", style = "color: #ffff; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Variable:", choices = colnames(data)),
                 actionButton("plot", "Generate Plot", class = "btn-primary")
               ),
               mainPanel(
                 plotOutput("scatter_boxplot")
               )
             )
           )
  ),
  
  # Prediction Dashboard
  tabPanel("Prediction Dashboard",
           fluidPage(
             titlePanel(title = div("Prediction Dashboard Monthly Sales", style = "color: #ffff; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
             sidebarLayout(
               sidebarPanel(
                 numericInput("x2_input", "x2 Value:", value = 10000),
                 numericInput("x5_input", "x5 Value:", value = 20000),
                 actionButton("predict_button", "Generate Predict", class = "btn-primary")
               ),
               
               mainPanel(
                 plotOutput("regression_plot"),
                 h4("Predicted Sales (Y):"),
                 verbatimTextOutput("predicted_sales")
               )
             )
           )
  )
)

# Define Server
server <- function(input, output) {
  
  # Descriptive Statistics
  output$Regresion_output <- renderPrint({
    summary(model1)
  })
  
  
  #corplot
  output$correlation_plot<- renderPlot({
    chart.Correlation(datafix)
  })
  
  # Variable Relationship
  observeEvent(input$plot, {
    variable1 <- input$variable
    
    output$scatter_boxplot <- renderPlot({
      
      scatter_plot <- ggplot(data, aes_string(x = variable1, y = "y")) +
        geom_point(color = "#3498DB", size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "#FF5722") +
        labs(x = variable1, y = "y", title = paste("Scatter Plot of", variable1, "vs. y")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.position = "none")
      
      scatter_plot
    })
  })
  
  # Prediction Dashboard
  # Predict function
  predict_sales <- function(x2, x5) {
    new_data <- data.frame(x2 = x2, x5 = x5)
    predicted_sales <- predict(model, newdata = new_data)
    return(predicted_sales)
  }
  
  # Render combined plot for x2 and x5
  output$regression_plot <- renderPlot({
    ggplot(data, aes(x = x2, y = y)) +
      geom_point(aes(color = "x2"), size = 3) +
      geom_smooth(method = "lm", formula = y ~ x2 + x5, se = FALSE) +
      geom_point(aes(x = x5, color = "x5"), size = 3) +
      labs(title = "Multiple Linear Regression",
           x = "x2 and x5",
           y = "Sales (Y)",
           color = "Variable") +
      theme_minimal()
  })
  
  # Render coefficients
  output$coefficients_text <- renderText({
    paste("Intercept:", round(coef(model)[1], 4),
          "\nx2 Coefficient:", round(coef(model)[2], 4),
          "\nx5 Coefficient:", round(coef(model)[3], 4))
  })
  
  # Event handler for prediction button
  observeEvent(input$predict_button, {
    x2_input <- input$x2_input
    x5_input <- input$x5_input
    predicted_sales <- predict_sales(x2_input, x5_input)
    output$predicted_sales <- renderText({
      paste("Predicted Sales (Y):", round(predicted_sales, 2))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
