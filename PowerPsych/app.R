library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    selectInput("alys", 
                label = h3("Analysis"), 
                choices = list("One-Way ANOVA" = "ANV1", 
                               "Two-Way ANOVA" = "ANV2", 
                               "Multiple Regression" = "MR",
                               "Multiple Regression With Interaction" = "MRWI"), 
                selected = "None"),
    
    numericInput("numIV", 
                 label = h3("Number Of Independent Variables"),
                 value = 1),
    
    
    
    
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
