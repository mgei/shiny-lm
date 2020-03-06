library(shiny)
library(shinyWidgets)
library(DT)

# change this for your data
feature_matrix <- mtcars

ui <- fluidPage(
  sidebarPanel(
    pickerInput(
      inputId = "model_response", 
      label = "Model response", 
      choices = names(feature_matrix), 
      multiple = F
    ),
    pickerInput(
      inputId = "model_features", 
      label = "Model features", 
      choices = names(feature_matrix), 
      multiple = T
    ),
    checkboxInput(
      inputId = "intercept",
      label = "use an intercept", 
      value = T),
    
    DTOutput("rawdata")),
  
  
  mainPanel(
    verbatimTextOutput("summary"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4"),
    plotOutput("plot5"),
    plotOutput("plot6")
  )
)

server <- function(input, output, session) {
  
  output$rawdata <- renderDT({
    datatable(feature_matrix,
              options = list(scrollX = TRUE))
  })
  
  linearmodel <- reactive({
    
    req(input$model_response)
    req(input$model_features)
    
    formula <- reformulate(termlabels = input$model_features, response = input$model_response, intercept = input$intercept)
    
    lm(formula = formula, data = feature_matrix)
    
  })
  
  output$summary <- renderPrint({
    summary(linearmodel())
  })
  
  output$plot1 <- renderPlot({
    plot(linearmodel(), which = 1)
  })
  
  output$plot2 <- renderPlot({
    plot(linearmodel(), which = 2)
  })
  
  output$plot3 <- renderPlot({
    plot(linearmodel(), which = 3)
  })
  
  output$plot4 <- renderPlot({
    plot(linearmodel(), which = 4)
  })
  
  output$plot5 <- renderPlot({
    plot(linearmodel(), which = 5)
  })
  
  output$plot6 <- renderPlot({
    plot(linearmodel(), which = 6)
  })
  
}

shinyApp(ui, server)