library(shiny)
library(semTools)
library(car)
library(DT)

anova1way <- function(n, means, sds, num.grp){
    
    # simulate data for each group
    for (r in 1:num.grp) {
        # create the model formula to be used in simulateData(model = ...)
        # assign the model formula to an object called grp*.model,
        # where * is 1 for group 1 and so on...
        assign(paste0("grp", r, ".model"),
               paste0("y ~ ", means[r], "*1", "\n y ~~ ", sds[r] ^ 2, "*y"))
        
        # run simulateData() and save the simulated dataset into an object called 
        # data.g*, where * is 1 for group 1 and so on...
        assign(paste0("data.g", r),
               simulateData(eval(parse(
                   text = paste0("grp", r, ".model")
               )),
               sample.nobs = n))
    }
    
    # retrieve the names of all objects in the environment with the suffix data.g
    alldata.obs <- ls(pattern = "^data.g")
    # create command needed to create the overall dataframe, consisting of data from each group
    # alldat.formula would take the form of "data.g1[,1], data.g2[,2]"
    alldat.formula <- paste0(alldata.obs, "[,1]", collapse = ",")
    # wrap alldat.formula in c( and ) to complete the command
    alldat.formula <- paste0("c(", alldat.formula, ")")
    
    # create a vector X to be used as a dummy variable to indicate the groups of
    # each row of the results in alldat, created later on...
    X <- c()
    for (r in 1:num.grp) {
        X <- append(X, rep(paste(r), n))
    }
    
    # alldat is the overall dataframe that contains the data of all the groups
    alldat <- data.frame(Y = eval(parse(text = alldat.formula)),
                         X1 = X)
    # save the p-value of the main effect from the 1-WAY ANOVA
    anova.results <- Anova(lm(Y ~ X1,
                              data = alldat))[1,4]
    
    # if the p value is statistically significant, let sig = 1,
    # else sig = 0
    if (anova.results < 0.05) {
        sig <- 1
    } else {
        sig <- 0
    }
    
    # the output of the function is either 0 or 1, 
    # 0 = main effect was not statistically significant,
    # 1 = main effect was statistically significant
    return(sig)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "One-Way ANOVA",
    fluidRow(
        column(2,
               numericInput("sampsize", label = h3("Sample Size Per Group"), value = 100),
               numericInput("iter",label = h3("Number Of Iterations"), value = 100)
               ),
        column(2,
               numericInput("g1mean", label = h3("Mean Of Group 1"), value = 1),
               numericInput("g1sd", label = h3("SD Of Group 1"), value = 1),
               ),
        column(2,
               numericInput("g2mean",label = h3("Mean Of Group 2"), value = -1),
               numericInput("g2sd", label = h3("SD Of Group 2"), value = 1),
               actionButton("update", label = "Calculate"))
    ),
    hr(),
    column(verbatimTextOutput(outputId = "power"), offset = 4, width = 4)
)


server <- function(input, output) {
    
    # tracker is the object which will keep track of how many time the
    # main effect of X1 was statistically significant
    tracker <- data.frame(MainEffect = c("X1"),
                          Count = c(0))
    # create params, a list of reactive values that will create the interactivity of
    # the app
    params <- reactiveValues(sampsize = 100,
                             g1mean = 1,
                             g2mean = 2,
                             g1sd = 1,
                             g2sd = 1,
                             iter = 100)
    
    # ensure that the results only update when the calculate
    # button is pressed and input$update is invalidated
    observeEvent(input$update,{
        
        params$sampsize <- input$sampsize
        params$g1mean <- input$g1mean
        params$g2mean <- input$g2mean
        params$g1sd <- input$g1sd
        params$g2sd <- input$g2sd
        params$iter <- input$iter
    })
    
    # the main output, which is a dataframe but looks nicer when printed
    output$power <- renderPrint({
        
        # a progress counter
        progress <- 0
        
        # loop repeating anova1way() with appropriate parameters for the number of
        # iterations specified by the user
        for (i in 1:params$iter){
            tracker$Count[1] <- tracker$Count[1] + anova1way(n = params$sampsize, 
                                                             means = c(params$g1mean, params$g2mean), 
                                                             sds = c(params$g1sd, params$g2sd), 
                                                             num.grp = 2)
            progress <- progress + 1
            # print(paste0(progress, " out of ", params$iter, " iterations completed!"))
        }
        tracker
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

