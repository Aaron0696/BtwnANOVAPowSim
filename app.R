# load and install packages
if (require('shinycssloaders') == FALSE){
  install.packages('shinycssloaders')
}
library(shinydashboard)

if (require('shinydashboard') == FALSE){
  install.packages('shinydashboard')
}
library(shinydashboard)

if (require('semTools') == FALSE){
  install.packages('semTools')
}
library(semTools)

if (require('car') == FALSE){
  install.packages('car')
}
library(car)

if (require('shinythemes') == FALSE){
  install.packages('shinythemes')
}
library(shinythemes)

if (require('ggplot2') == FALSE){
  install.packages('ggplot2')
}
library(ggplot2)

if (require('plotly') == FALSE){
  install.packages('plotly')
}
library(plotly)

if (require('stringr') == FALSE){
  install.packages('stringr')
}
library(stringr)

# load these functions maintained in another file
## anova1way()
## anova1way.sim()
source("functions.R")

# crafting user-interface
ui <- fluidPage(
  # select theme
  theme = shinytheme("lumen"),
  # title
  titlePanel(h1("One-Way ANOVA Power Simulator"),
             windowTitle = "PowerPsych"),
  # descrption in prose form
  fluidRow(
    column(
      wellPanel("This is a personal project for myself and it is essentially a power simulator for One-Way ANOVAs.",
                br(),
                "In experiments, power is usually defined as the probability that an effect 
                      would be statistically significant given that the effect truly exists.",
                br(),
                "There are formulas to calculate power but these formulas assume that certain assumptions are met.
                      A less restrictive approach to calculating power is to draw a sample from a known population where the effect is present,
                      test the hypothesis and repeat this multiple times (iterations). Count the relative frequency that a statistically significant effect
                      was observed and that is power.",
                br(),
                "For example, I generated samples of n = 100 each while assuming that the effect existed.
                      If 600 out of 1000 iterations produced a statistically significant effect,
                      my estimated power would be 0.6.",
                br(),
                "The downside to this simulation-based approach is that an intermediate level of scripting is needed,
                      which may gatekeep users from accessing this technique. This RShiny was created to allow users of
                      all scripting level to perform such simulations.",
                br(),
                tags$code("For those who are seeing this now, I am still testing things out, so if there are any comments on how 
                        it can be further improved (aesthetics, features, user-experience, bugs), do tell me. 
                        I plan to extend this to include Two-Way ANOVAs and Multiple Regression once I got the bugs, features and aesthetics down.")
      ),
      width = 10),
    column(
      wellPanel(h4("Access the codes on "),
                h4(tags$a(href = "https://github.com/Aaron0696/PowerPsych", "Github")),
                h4(" or connect with me on "),
                h4(tags$a(href = "https://www.linkedin.com/in/aaron-lim-b30898135/", "LinkedIn."))),
      width = 2
    )),
  hr(),
  fluidRow(
    column(width = 3,
           # general settings panel
           h2("Step One: General Settings"),
           hr(),
           wellPanel(
             
             radioButtons("numways", label = h3("ANOVA Design"),
                          choices = list("One-Way Between-Subjects" = "B1",
                                         "Two-Way Between-Subjects" = "B2"), 
                          selected = "B1"),
             
             numericInput("iter",label = h3("Number Of Iterations"), value = 100),
             "The option above determines the number of iteration for the simulation, please note that selecting 
                   larger number of iterations will increase computational time.",
             
             numericInput("seed", label = h3("Set Seed For Replicability"), value = 123456),
             "The seed ensures that the simulation is replicable, it is ideal to record down the seed utilized. 
                   Any sufficiently-large number can be used for the seed.",
             
             sliderInput("alphalevel", label = h3("Alpha"), min = 0.001, max = 0.999, value = 0.05, step = 0.001),
             "This is the alpha level used for significance testing, defaults to 0.05.",
           )),
    column(
      # panel for condition parameters that is dependent on the number of conditions selected
      h2("Step Two: Condition Parameters"), 
      hr(),
      h4("Input the unstandardized means, standard deviations and condition sizes in the panels below. 
         Greater differences in means, smaller standard deviations and larger condition sizes will lead to greater power."),
      hr(),
      
      column(width = 6, conditionalPanel(condition = "input.numways == 'B2'",
                                         wellPanel(textInput("iv1name", label = h3("First Independent Variable"), value = "X1"),
                                                   uiOutput("numgrp.sel1"))),
             conditionalPanel(condition = "input.numways == 'B2'",
                              uiOutput("lvlnames1"))),
      
      column(width = 6, conditionalPanel(condition = "input.numways == 'B2'",
                                         wellPanel(textInput("iv2name", label = h3("Second Independent Variable"), value = "X2"),
                                                   uiOutput("numgrp.sel2"))),
             conditionalPanel(condition = "input.numways == 'B2'",
                              uiOutput("lvlnames2"))),
      
      conditionalPanel(condition = "input.numways == 'B1'",
                       wellPanel(textInput("ivname", label = h3("First Independent Variable"), value = "X1"),
                                 uiOutput("numgrp.sel"))),
      conditionalPanel(condition = "input.numways == 'B1'",
                       uiOutput("cndparams1")),
      conditionalPanel(condition = "input.numways == 'B2'",
                       uiOutput("cndparams2")),
      width = 7),
    column(
      # panel for the run button
      wellPanel(h3("All Done?"),
                "Click Run! when you are satisfied with your settings and are ready to begin the simulation.
                      The application may take a while so hang tight!",
                hr(),
                actionButton("update", label = "Run!", icon = icon("random")),
                h4("Output will be generated below...")), width = 2)
  ),
  hr(),
  fluidRow(
    column(
      # description for the graph
      conditionalPanel(condition = "input.update != 0",
                       h3("The density plots approximates the population described by the chosen parameters."),
                       h3("Power is maximized when the conditions overlap minimally, this can be achieved by selecting 
                                   greater mean differences or lower standard deviations."),
                       h3("Each condition is assigned a different color and X denotes the value of the mean."),
                       h3("The simulated power will be reported below.")), 
      width = 4),
    column(
      # interactive graph
      plotlyOutput("graph"), 
      width = 8)
  ),
  hr(),
  fluidRow(
    column(
      # text communicating the simulated power
      conditionalPanel(condition = "input.update != 0",
                       withSpinner(verbatimTextOutput("power"), 
                                   size = getOption("spinner.size", default = 0.5),
                                   proxy.height = "100px")), 
      offset = 0, width = 12))
)


server <- function(input, output) {
  
  output$numgrp.sel <- renderUI({
    
    sliderInput("numcnds", label = h3("Number Of Conditions/Levels"), min = 2, max = 9, value = 2)
  })
  
  output$numgrp.sel1 <- renderUI({
    
    sliderInput("numcnds1", label = h3("Number Of Conditions/Levels"), min = 2, max = 9, value = 2)
  })
  
  output$lvlnames1 <- renderUI({
    lapply(1:input$numcnds1, function(i) {
      column(wellPanel(textInput(paste0("lvl1names", i), label = paste0("Level ", i, " Name"), value = "Input...")),
                       width = 6) 
    })
  })
  
  output$numgrp.sel2 <- renderUI({
    
    sliderInput("numcnds2", label = h3("Number Of Conditions/Levels"), min = 2, max = 9, value = 2)
  })
  
  output$lvlnames2 <- renderUI({
    lapply(1:input$numcnds2, function(i) {
      column(wellPanel(textInput(paste0("lvl2names", i), label = paste0("Level ", i, " Name"), value = "Input...")),
             width = 6) 
    })
  })
  
  # output that lets user specify the names of the IVs
  
  
  # take number of conditions as input and
  # create dynamic UI that take means as input
  
    output$cndparams1 <- renderUI({
        numcnds <- as.integer(input$numcnds)
        lapply(1:numcnds, function(i) {
          # header <- eval(parse(text = paste0("input$lvlnames", i)))
          column(wellPanel(textInput(paste0("lvlnames", i), label = paste0("Level ", i, " Name"), value = "Input..."),
                           numericInput(paste0("g",i,"mean"),
                                        label = paste0("Mean"),
                                        value = i/2),
                           numericInput(paste0("g",i,"sd"),
                                        label = paste0("SD"),
                                        value = 1),
                           numericInput(paste0("g",i,"size"),
                                        label = paste0("Size"),
                                        value = 50)), width = 4) 
        })
    })
  
    output$cndparams2 <- renderUI({
      numcnds1 <- as.integer(input$numcnds1)
      numcnds2 <- as.integer(input$numcnds2)
      lapply(1:numcnds1, function(i) {
        ######
        lapply(1:numcnds2, function(r) {
          
          column(wellPanel(
            h3(paste0(input$iv1name, ": ", eval(parse(text = paste0("input$lvl1names", i))))),
            h3(paste0(input$iv2name, ": ", eval(parse(text = paste0("input$lvl2names", r))))),
            
            numericInput(paste0("g",i, r, "mean"),
                                        label = paste0("Mean"),
                                        value = i/2),
                           numericInput(paste0("g",i, r, "sd"),
                                        label = paste0("SD"),
                                        value = 1),
                           numericInput(paste0("g",i, r, "size"),
                                        label = paste0("Size"),
                                        value = 50)), width = 3)
        })
      })
    })
  
  # tracker is the object which will keep track of how many times the
  # main effect of X1 was statistically significant in the simulation
  tracker <- data.frame(NumIterations = 0,
                        NumSig = 0)
  
  # params is a list of reactive values that will create the interactivity of the app
  # used to link observeEvent() to the inputs
  params <- reactiveValues(numcnds = 2)
  
  # ensure that the results only update when the run button is pressed and input$update is invalidated
  observeEvent(input$update,{
    params$iter <- input$iter
    params$numcnds <- input$numcnds
    params$numcnds1 <- input$numcnds1
    params$numcnds2 <- input$numcnds2
    params$seed <- input$seed
    params$alpha <- input$alphalevel
    params$ivname <- input$ivname
    params$iv1name <- input$iv1name
    params$iv2name <- input$iv2name
    
    
    if (input$numways == "B1") {
      for (r in 1:input$numcnds){
        params[[paste0("g",r,"mean")]] <- input[[paste0("g",r,"mean")]]
        params[[paste0("g",r,"sd")]] <- input[[paste0("g",r,"sd")]]
        params[[paste0("g",r,"size")]] <- input[[paste0("g",r,"size")]]
      }
    }
    
    if (input$numways == "B2") {
      for (i in 1:input$numcnds1) {
        
        for (r in 1:input$numcnds2){
          params[[paste0("g",i, r,"mean")]] <- input[[paste0("g",r,"mean")]]
          params[[paste0("g",i, r,"sd")]] <- input[[paste0("g",r,"sd")]]
          params[[paste0("g",i, r,"size")]] <- input[[paste0("g",r,"size")]]
        }
      }
    }
    
    
  })
  
  # power output
  output$power <- renderText({
    
    # creating the formulas to be parsed as arguments
    meanform <- paste0("params$g", 1:params$numcnds, "mean", collapse = ",")
    meanform <- paste0("c(", meanform, ")")
    sdform <- paste0("params$g", 1:params$numcnds, "sd", collapse = ",")
    sdform <- paste0("c(", sdform, ")")
    sizeform <- paste0("params$g", 1:params$numcnds, "size", collapse = ",")
    sizeform <- paste0("c(", sizeform, ")")
    
    # for the first time the run button is pressed, say loading
    if(input$update == 0){
      paste0("Loading!")
    } else {
      # loop repeating anova1way() with the chosen parameters for the number of
      # iterations specified by the user    
      set.seed(params$seed)
      
      if(input$numways == "B1"){
        for(i in 1:params$iter){
          tracker$NumSig[1] <- tracker$NumSig[1] + anova1way(means = eval(parse(text = meanform)), 
                                                             sds = eval(parse(text = sdform)), 
                                                             grpsize = eval(parse(text = sizeform)),
                                                             num.grp = params$numcnds,
                                                             alphalvl = params$alpha)
        }
        
        # save the number of iterations from params
        tracker$NumIterations <- params$iter
        # calculate power
        tracker$Power <- tracker$NumSig / tracker$NumIterations
        
        # print text
        print(paste0(params$iter, " iterations were simulated and ", 
                     tracker$NumSig, " iterations had statistically significant main effects.",
                     "\nThe simulated power for the overall main effect of ", params$ivname, " is ", tracker$Power, "."))
      }
      
      if(input$numways == "B2"){
        
        anova2way(means = c(1,1,1,1),
                  sds = c(1,1,1,1),
                  grpsize = c(50,50,50,50),
                  iv1.lvl = c(1:2),
                  iv2.lvl = c(1:2),
                  alphalvl = 0.05)
        # anova2way(means = c(params$g11mean, params$g21mean,
        #                     params$g12mean, params$g22mean),
        #           sds = c(params$g11sd,params$g21sd,
        #                     params$g12sd, params$g22sd),
        #           grpsize = c(params$g11size,params$g21size,
        #                               params$g12size, params$g22size),
        #           iv1.lvl = c(1:params$numcnds1),
        #           iv2.lvl = c(1:params$numcnds2),
        #           alphalvl = params$alpha)
      }
    }
  })
  
  # plotly output
  output$graph <- renderPlotly({
    
    # same as above
    meanform <- paste0("params$g", 1:params$numcnds, "mean", collapse = ",")
    meanform <- paste0("c(", meanform, ")")
    sdform <- paste0("params$g", 1:params$numcnds, "sd", collapse = ",")
    sdform <- paste0("c(", sdform, ")")
    sizeform <- paste0("params$g", 1:params$numcnds, "size", collapse = ",")
    sizeform <- paste0("c(", sizeform, ")")
    
    # same as above, show blank graph before button is pressed
    if(input$update == 0){
      
    } else {
      # simulate data with 10,000 rows in each condition to approximate population
      data <- anova1way.sim(means = eval(parse(text = meanform)), 
                            sds = eval(parse(text = sdform)), 
                            grpsize = rep(10000, params$numcnds),
                            num.grp = params$numcnds)
      names(data) <- c("Y", "condition")
      myplot <- ggplot(data = data, aes(x = Y, fill = condition, label = condition)) + 
        geom_density(alpha = 0.4, color = "black", size = 0.8)
      # get numeric points used to plot density
      plotdetails <- ggplot_build(myplot)
      # get the maximum density of each condition
      cndmax <- aggregate(ymax ~ group,
                          data = plotdetails[["data"]][[1]],
                          FUN = max)
      # get the mean of each condition
      cndmax$mean <- round(aggregate(Y ~ condition,
                                     data = data,
                                     FUN = mean)$Y, 2)
      cndmax$group <- as.ordered(cndmax$group)
      ggplotly(
        ggplot() + 
          geom_density(data = data, aes(x = Y, fill = condition), alpha = 0.7, size = 0.8) + 
          geom_point(data = cndmax, aes(x = mean, y = 0.5, fill = group), color = "violetred", size = 6, shape = 4) + 
          ylim(c(0,NA)) +  
          ylab("Density") +
          xlab("Y") +
          theme_classic() + 
          theme(axis.title.y = element_text(size = 11),
                axis.title.x = element_text(size = 12))
        # make hovertext only show condition information
        , tooltip = c("fill")) %>%
        layout(legend = list(orientation = "h", y = 1.2))
    }
  })
}

# run
shinyApp(ui = ui, server = server)
