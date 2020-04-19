library(shiny)
library(semTools)
library(car)
library(DT)
library(shinythemes)

anova1way <- function(means, sds, grpsize, num.grp){
    
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
               sample.nobs = grpsize[r]
               ))
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
        X <- append(X, rep(paste(r), grpsize[r]))
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
    theme = shinytheme("flatly"),
    titlePanel(h1("One-Way ANOVA Power Simulator")),
    fluidRow(
        column(4,
               wellPanel(
                   h3("Please begin by selecting the general settings within this panel."),
                   numericInput("iter",label = h3("Number Of Iterations"), value = 100),
                   "The option above determines the number of iteration for the simulation, please note that selecting larger number of iterations will increase computational time.",
                   numericInput("seed", label = h3("Set Seed For Replicability"), value = 123456),
                   "The seed ensures that the simulation is replicable, it is ideal to record down the seed utilized. Any sufficiently-large number can be used for the seed.",
                   sliderInput("numgrps", label = h3("Number Of Groups"), min = 2, max = 9, value = 2),
                   "The number of levels your independent variable possesses. Or the number of groups you have in your design.",
                   hr(),
                   actionButton("update", label = "Run!"),
                   hr(),
                   "Click Run! when you are satisfied with your settings and are ready to begin the simulation. The output will appear below, the application may take a while so hang tight!"
               )),
        column(wellPanel(h3("Group Means"), uiOutput("grpmeans")), width = 2),
        column(wellPanel(h3("Group SDs"), uiOutput("grpsds")), width = 2),
        column(wellPanel(h3("Group Size"), uiOutput("grpsize")), width = 2)
    ),
    hr(),
    titlePanel("Output"),
    fluidRow(
                column(verbatimTextOutput(outputId = "power"), offset = 0, width = 6),
    )
)


server <- function(input, output) {
    
    output$grpmeans <- renderUI({
        numgrps <- as.integer(input$numgrps)
        lapply(1:numgrps, function(i) {
            numericInput(paste0("g",i,"mean"),
                        label = paste0("Mean Of Group ", i),
                        value = 1)
        })
    })
    
    output$grpsds <- renderUI({
        numgrps <- as.integer(input$numgrps)
        lapply(1:numgrps, function(i) {
            numericInput(paste0("g",i,"sd"),
                         label = paste0("SD Of Group ", i),
                         value = 1)
        })
    })
    
    output$grpsize <- renderUI({
        numgrps <- as.integer(input$numgrps)
        lapply(1:numgrps, function(i) {
            numericInput(paste0("g",i,"size"),
                         label = paste0("Size Of Group ", i),
                         value = 50)
        })
    })
    
    
    # tracker is the object which will keep track of how many time the
    # main effect of X1 was statistically significant
    tracker <- data.frame(NumIterations = 0,
                          NumSig = c(0)
                          )
    # create params, a list of reactive values that will create the interactivity of
    # the app
    params <- reactiveValues(numgrps = 2)
    
    # ensure that the results only update when the calculate
    # button is pressed and input$update is invalidated
    # browser()
    observeEvent(input$update,{
    
        params$iter <- input$iter
        params$numgrps <- input$numgrps
        params$seed <- input$seed
        
        if(input$numgrps == 2){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
        }
        
        if(input$numgrps == 3){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
        }
        
        if(input$numgrps == 4){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g4mean <- input$g4mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g4sd <- input$g4sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
            params$g4size <- input$g4size
        }
        
        if(input$numgrps == 5){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g4mean <- input$g4mean
            params$g5mean <- input$g5mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g4sd <- input$g4sd
            params$g5sd <- input$g5sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
            params$g4size <- input$g4size
            params$g5size <- input$g5size
        }
        
        if(input$numgrps == 6){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g4mean <- input$g4mean
            params$g5mean <- input$g5mean
            params$g6mean <- input$g6mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g4sd <- input$g4sd
            params$g5sd <- input$g5sd
            params$g6sd <- input$g6sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
            params$g4size <- input$g4size
            params$g5size <- input$g5size
            params$g6size <- input$g6size
            
        }
        
        if(input$numgrps == 7){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g4mean <- input$g4mean
            params$g5mean <- input$g5mean
            params$g6mean <- input$g6mean
            params$g7mean <- input$g7mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g4sd <- input$g4sd
            params$g5sd <- input$g5sd
            params$g6sd <- input$g6sd
            params$g7sd <- input$g7sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
            params$g4size <- input$g4size
            params$g5size <- input$g5size
            params$g6size <- input$g6size
            params$g7size <- input$g7size
        }
        
        if(input$numgrps == 8){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g4mean <- input$g4mean
            params$g5mean <- input$g5mean
            params$g6mean <- input$g6mean
            params$g7mean <- input$g7mean
            params$g8mean <- input$g8mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g4sd <- input$g4sd
            params$g5sd <- input$g5sd
            params$g6sd <- input$g6sd
            params$g7sd <- input$g7sd
            params$g8sd <- input$g8sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
            params$g4size <- input$g4size
            params$g5size <- input$g5size
            params$g6size <- input$g6size
            params$g7size <- input$g7size
            params$g8size <- input$g8size
        }
        
        if(input$numgrps == 9){
            params$g1mean <- input$g1mean
            params$g2mean <- input$g2mean
            params$g3mean <- input$g3mean
            params$g4mean <- input$g4mean
            params$g5mean <- input$g5mean
            params$g6mean <- input$g6mean
            params$g7mean <- input$g7mean
            params$g8mean <- input$g8mean
            params$g9mean <- input$g9mean
            params$g1sd <- input$g1sd
            params$g2sd <- input$g2sd
            params$g3sd <- input$g3sd
            params$g4sd <- input$g4sd
            params$g5sd <- input$g5sd
            params$g6sd <- input$g6sd
            params$g7sd <- input$g7sd
            params$g8sd <- input$g8sd
            params$g9sd <- input$g9sd
            params$g1size <- input$g1size
            params$g2size <- input$g2size
            params$g3size <- input$g3size
            params$g4size <- input$g4size
            params$g5size <- input$g5size
            params$g6size <- input$g6size
            params$g7size <- input$g7size
            params$g8size <- input$g8size
            params$g9size <- input$g9size
        }
        
    })
    
    # the main output, which is a dataframe but looks nicer when printed
    output$power <- renderPrint({
        
        # a progress counter
        progress <- 0

        meanform <- paste0("params$g", 1:params$numgrps, "mean", collapse = ",")
        meanform <- paste0("c(", meanform, ")")
        sdform <- paste0("params$g", 1:params$numgrps, "sd", collapse = ",")
        sdform <- paste0("c(", sdform, ")")
        sizeform <- paste0("params$g", 1:params$numgrps, "size", collapse = ",")
        sizeform <- paste0("c(", sizeform, ")")
        
        if(input$update == 0){
            print("Please input parameters and press the Calculate button.")
        } else {

            # loop repeating anova1way() with appropriate parameters for the number of
            # iterations specified by the user    
            set.seed(params$seed)
            for(i in 1:params$iter){
                    tracker$NumSig[1] <- tracker$NumSig[1] + anova1way(means = eval(parse(text = meanform)), 
                                                                     sds = eval(parse(text = sdform)), 
                                                                     grpsize = eval(parse(text = sizeform)),
                                                                     num.grp = params$numgrps)
                    # progress <- progress + 1
                    # print(paste0(progress, " out of ", params$iter, " iterations completed!"))
            }
            tracker$NumIterations <- params$iter
            rownames(tracker) <- "Main Effect"
            tracker$Power <- tracker$NumSig / tracker$NumIterations
            tracker
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

