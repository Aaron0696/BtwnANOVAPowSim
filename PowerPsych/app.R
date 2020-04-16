library(shiny)
library(semTools)

model <- "
y ~ c(0.4)*1
"

temp <- simulateData(model, group.label = c("A","B"))

# a function that generates data based on the model selected by the user from the user-interface
# and conducts the analysis
genAnalyze <- function(model){
    
    
    
    
}


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
    numericInput("Total Sample Size", 
                 label = h3("Number Of Independent Variables"),
                 value = 1)
)

server <- function(input, output) {
    
    
    data <- reactive({
        
        
                   
    })

    output$distPlot <- renderPlot({
        data <-  data.frame(DV = c(rnorm(100,
                                         mean = 0.5,
                                         sd = 1),
                                   rnorm(100,
                                         mean = 0,
                                         sd = 1)),
                            Grp = c(rep(0,100),
                                    rep(1,100)))
        pvalue <- summary(lm(DV ~ Grp, 
                             data = data))[["coefficients"]][,4]
        
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
