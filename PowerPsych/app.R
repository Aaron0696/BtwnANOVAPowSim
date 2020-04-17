library(shiny)
library(semTools)
library(car)

n <- 1000
num.grp <- 2
mean.g1 <- 3
mean.g2 <- 4
iteration <- 100
means <- c(0,1)
sds <- c(2.5,2.5)

# counter
counter <- 0

# vector 
tracker <- data.frame(MainEffect = c("X1"),
                      Count = c(0))

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
# loop
for (i in 1:iterations){
    tracker$Count[1] <- tracker$Count[1] + anova1way(n = n, means = means, sds = sds, num.grp = num.grp)
    progress <- progress + 1
    print(paste0(progress,
                " out of ",
                iterations,
                " iterations completed!"))
}


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
