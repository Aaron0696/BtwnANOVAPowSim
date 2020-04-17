library(shiny)
library(semTools)
library(car)
library(DT)

anova1way <- function(n, means, sds, num.grp) {
    for (r in 1:num.grp) {
        assign(paste0("grp",
                      r,
                      ".model"),
               paste0("y ~ ",
                      means[r],
                      "*1",
                      "\n y ~~ ",
                      sds[r] ^ 2,
                      "*y"))
        
        assign(paste0("data.g",
                      r),
               simulateData(eval(parse(
                   text = paste0("grp",
                                 r,
                                 ".model")
               )),
               sample.nobs = n))
    }
    
    # retrieve all objects in environment with the suffix data.g
    alldata.obs <- ls(pattern = "^data.g")
    # create formula to create the overall dataframe
    alldat.formula <- paste0(alldata.obs, "[,1]", collapse = ",")
    alldat.formula <- paste0("c(", alldat.formula, ")")
    
    X <- c()
    for (r in 1:num.grp) {
        X <- append(X, rep(paste(r), n))
    }
    
    alldat <- data.frame(Y = eval(parse(text = alldat.formula)),
                         X1 = X)
    
    anova.results <- Anova(lm(Y ~ X1,
                              data = alldat))[1, 4]
    
    if (anova.results < 0.05) {
        sig <- 1
    } else {
        sig <- 0
    }
    
    return(sig)
    counter <- counter + 1
    print(counter)
}

# progress "bar"
progress <- 0

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            actionButton(
                "update",
                label = "Calculate"
            ),
            selectInput(
                "alys",
                label = h3("Analysis"),
                choices = list(
                    "One-Way ANOVA" = "ANV1",
                    "Two-Way ANOVA" = "ANV2",
                    "Multiple Regression" = "MR",
                    "Multiple Regression With Interaction" = "MRWI"
                ),
                selected = "None"
            ),
            numericInput(
                "numIV",
                label = h3("Number Of Independent Variables"),
                value = 1
            ),
            numericInput(
                "sampsize",
                label = h3("Sample Size Per Group"),
                value = 100
            ),
            numericInput(
                "g1mean",
                label = h3("Mean Of Group 1"),
                value = 1
            ),
            numericInput(
                "g2mean",
                label = h3("Mean Of Group 2"),
                value = -1
            ),
            numericInput(
                "g1sd",
                label = h3("SD Of Group 1"),
                value = 1
            ),
            numericInput(
                "g2sd",
                label = h3("SD Of Group 2"),
                value = 1
            ),
            numericInput(
                "iter",
                label = h3("Number Of Iterations"),
                value = 100
            )
        ),
        mainPanel(
            dataTableOutput(outputId = "power")
        )
    ))

server <- function(input, output) {
    
    # tracker
    tracker <- data.frame(MainEffect = c("X1"),
                          Count = c(0))
    
    params <- reactiveValues(sampsize = 100,
                             g1mean = 1,
                             g2mean = 2,
                             g1sd = 1,
                             g2sd = 1,
                             iter = 100)
    
    observeEvent(input$update,{
        
        params$sampsize <- input$sampsize
        params$g1mean <- input$g1mean
        params$g2mean <- input$g2mean
        params$g1sd <- input$g1sd
        params$g2sd <- input$g2sd
        params$iter <- input$iter
    })
    
    output$power <- renderDataTable({
        
        # loop
        for (i in 1:params$iter){
            tracker$Count[1] <- tracker$Count[1] + anova1way(n = params$sampsize, 
                                                             means = c(params$g1mean, params$g2mean), 
                                                             sds = c(params$g1sd, params$g2sd), 
                                                             num.grp = 2)
            progress <- progress + 1
            # print(paste0(progress,
            #              " out of ",
            #              iterations,
            #              " iterations completed!"))
        }
        
        tracker
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

