library(shiny)

ui <- fluidPage(
  titlePanel("Histogram of Random Normal Values"),
  
  sliderInput(
    inputId = "num",
    label = "Choose a number:",
    min = 1,
    max = 100,
    value = 50
  ),
  
  textInput(
    inputId = "text",
    label = "Change title?",
    value = "Histogram of Random Normal Values"
  ),
  
  actionButton(inputId = "norm",
               label = "Normal"),
  
  actionButton(inputId = "unif",
               label = "Uniform"),
  
  plotOutput("hist"),
  verbatimTextOutput("stats")
  
)

server <- function (input, output) {
  data <-
    rv <- reactiveValues(data = rnorm(100))
    eventReactive(input$clicks, {
      rnorm(input$num)
    })
  
  reactive ({
    rnorm(input$num)
  })
  
  
  output$hist <- renderPlot({
    hist(rv$data,
  
    main = isolate({input$text})
    )
    
  })
  
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
  
  observeEvent(input$norm, {
    rv$data <-rnorm(100)
  })
  
  observeEvent(input$unif, {
    rv$data <-runif(100)
  })
}

shinyApp(ui = ui, server = server)
