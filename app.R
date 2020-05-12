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
                          selected = "B2"),
             
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
                                         wellPanel(textInput("iv1name", label = h3("First Independent Variable"), value = "IV1"),
                                                   uiOutput("numgrp.sel1"))),
             conditionalPanel(condition = "input.numways == 'B2'",
                              uiOutput("lvlnames1"))),
      
      column(width = 6, conditionalPanel(condition = "input.numways == 'B2'",
                                         wellPanel(textInput("iv2name", label = h3("Second Independent Variable"), value = "IV2"),
                                                   uiOutput("numgrp.sel2"))),
             conditionalPanel(condition = "input.numways == 'B2'",
                              uiOutput("lvlnames2"))),
      
      conditionalPanel(condition = "input.numways == 'B1'",
                       wellPanel(textInput("ivname", label = h3("First Independent Variable"), value = "X1"),
                                 uiOutput("numgrp.sel"))),
      conditionalPanel(condition = "input.numways == 'B1'",
                       uiOutput("cndparams1")),
     fluidRow(conditionalPanel(condition = "input.numways == 'B2'",
                       uiOutput("cndparams2"))),
      width = 7),
    column(
      h2("Step Three: Run!"), 
      hr(),
      # panel for the run button
      wellPanel(h3("All Done?"),
                h4("Click Run! when you are satisfied with your settings and are ready to begin the simulation.
                      The application may take a while so hang tight!"),
                hr(),
                actionButton("update", label = "Run!", icon = icon("random")),
                h4("Output will be generated below...")), width = 2)
  ),
  hr(),
  fluidRow(
    column(
      # text communicating the simulated power
      conditionalPanel(condition = "input.update != 0",
                       wellPanel(h3("Simulated Power"),
                                 hr(),
                       withSpinner(tableOutput("power"), 
                                   size = getOption("spinner.size", default = 0.5),
                                   proxy.height = "100px"))), 
      offset = 3, width = 2),
    column(
      # description for the graph
      conditionalPanel(condition = "input.update != 0",
                       h3("The graph below visualizes the condition means as bars."), 
                          h4("Doublecheck that the displayed means correspond to the values selected above and are the intended values."),
                       plotOutput("graph")), 
      width = 4),
  ),
  hr(),
  fluidRow(
    )
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
      column(wellPanel(textInput(paste0("lvl1names", i), label = paste0("Level ", i, " Name"), value = paste(i))),
                       width = 6) 
    })
  })
  
  output$numgrp.sel2 <- renderUI({
    
    sliderInput("numcnds2", label = h3("Number Of Conditions/Levels"), min = 2, max = 9, value = 2)
  })
  
  output$lvlnames2 <- renderUI({
    lapply(1:input$numcnds2, function(i) {
      column(wellPanel(textInput(paste0("lvl2names", i), label = paste0("Level ", i, " Name"), value = LETTERS[i])),
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
          column(wellPanel(textInput(paste0("lvlnames", i), label = paste0("Level ", i, " Name"), value = LETTERS[i]),
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
          params[[paste0("g",i, r,"mean")]] <- input[[paste0("g",i, r,"mean")]]
          params[[paste0("g",i, r,"sd")]] <- input[[paste0("g",i, r,"sd")]]
          params[[paste0("g",i, r,"size")]] <- input[[paste0("g",i, r,"size")]]
        }
      }
    }
    
    
  })
  
  # power output
  # put the if loop outside
  output$power <- renderTable({
    
    # for the first time the run button is pressed, say loading
    if(input$update == 0){
      paste0("Loading!")
    } else {
      # loop repeating anova1way() with the chosen parameters for the number of
      # iterations specified by the user    
      set.seed(params$seed)
      
      if(input$numways == "B1"){
        
        tracker <- data.frame(Effect = c(params$ivname, 
                                         "Error"),
                              NumSig = 0)
        
        # creating the formulas to be parsed as arguments
        meanform <- paste0("params$g", 1:params$numcnds, "mean", collapse = ",")
        meanform <- paste0("c(", meanform, ")")
        sdform <- paste0("params$g", 1:params$numcnds, "sd", collapse = ",")
        sdform <- paste0("c(", sdform, ")")
        sizeform <- paste0("params$g", 1:params$numcnds, "size", collapse = ",")
        sizeform <- paste0("c(", sizeform, ")")
        
        for(i in 1:params$iter){
          tracker$NumSig <- tracker$NumSig + anova1way(means = eval(parse(text = meanform)),
                                                             sds = eval(parse(text = sdform)),
                                                             grpsize = eval(parse(text = sizeform)),
                                                             num.grp = params$numcnds,
                                                             alphalvl = params$alpha)
          # tracker$NumSig <- tracker$NumSig + anova1way(means = c(1,2),
          #                                              sds = c(1,1),
          #                                              grpsize = c(50,50),
          #                                              num.grp = 2,
          #                                              alphalvl = 0.05)
        }

        # save the number of iterations from params
        tracker$NumIterations <- params$iter
        # calculate power
        tracker$Power <- tracker$NumSig / tracker$NumIterations
        tracker[,c(1,4)]
      } else if(input$numways == "B2"){
        
        # tracker is the object which will keep track of how many times the
        # main effect of X1 was statistically significant in the simulation
        tracker <- data.frame(Effect = c(params$iv1name, 
                                         params$iv2name, 
                                        paste0(params$iv1name, "*", params$iv2name), 
                                              "Error"),
                              NumSig = 0)
        
        # creating the formulas to be parsed as arguments
        meanform <- c()
        for(r in 1:params$numcnds2){
          for(i in 1:params$numcnds1){
            meanform <- paste0(meanform, "params$g", i, r, "mean", ",")
          }
        }
        meanform <- substr(meanform, start = 1, stop = nchar(meanform)-1)
        meanform <- paste0("c(", meanform, ")")
        
        sdform <- c()
        for(r in 1:params$numcnds2){
          for(i in 1:params$numcnds1){
            sdform <- paste0(sdform, "params$g", i, r, "sd", ",")
          }
        }
        sdform <- substr(sdform, start = 1, stop = nchar(sdform)-1)
        sdform <- paste0("c(", sdform, ")")
        
        sizeform <- c()
        for(r in 1:params$numcnds2){
          for(i in 1:params$numcnds1){
            sizeform <- paste0(sizeform, "params$g", i, r, "size", ",")
          }
        }
        sizeform <- substr(sizeform, start = 1, stop = nchar(sizeform)-1)
        sizeform <- paste0("c(", sizeform, ")")
        
        
        for(i in 1:params$iter){
          tracker$NumSig <- tracker$NumSig + anova2way(means = eval(parse(text = meanform)),
                                                             sds = eval(parse(text = sdform)),
                                                             grpsize = eval(parse(text = sizeform)),
                                                            iv1.lvl = 1:params$numcnds1,
                                                       iv2.lvl = 1:params$numcnds2,
                                                             alphalvl = params$alpha)
        }
        
        # save the number of iterations from params
        tracker$NumIterations <- params$iter
        # calculate power
        tracker$Power <- tracker$NumSig / tracker$NumIterations
        tracker[,c(1,4)]
      }
    }
  }, striped = TRUE,  
  hover = TRUE, spacing = 'm',  
  align = 'c',  
  digits = 2)
  
  # plotly output
  output$graph <- renderPlot({
    
   # browser()
    
    # same as above, show blank graph before button is pressed
    if(input$update == 0){
      
    } else if(input$numways == "B1"){
      # same as above
      meanform <- paste0("params$g", 1:params$numcnds, "mean", collapse = ",")
      meanform <- paste0("c(", meanform, ")")
      sdform <- paste0("params$g", 1:params$numcnds, "sd", collapse = ",")
      sdform <- paste0("c(", sdform, ")")
      sizeform <- paste0("params$g", 1:params$numcnds, "size", collapse = ",")
      sizeform <- paste0("c(", sizeform, ")")
      
      # formula for IV1 and IV2
      text1 <- c()
      for(i in 1:params$numcnds){
        text1 <- c(text1, eval(parse(text = paste0("input$lvlnames", i))))
      }
      
      aggre <- data.frame(Mean = eval(parse(text = meanform)),
                          SD = eval(parse(text = sdform)),
                          IV1 = text1)
      
      
      ggplot(data = aggre, aes(x = IV1, y = Mean, label = SD)) + 
        geom_bar(stat = "identity", position = position_dodge(), fill = "violetred") +
        theme_classic() + 
        theme(axis.title.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 20)) + 
        xlab(params$ivname)
      
      
    } else if(input$numways == "B2"){
      # creating the formulas to be parsed as arguments
      meanform <- c()
      for(r in 1:params$numcnds2){
        for(i in 1:params$numcnds1){
          meanform <- paste0(meanform, "params$g", i, r, "mean", ",")
        }
      }
      meanform <- substr(meanform, start = 1, stop = nchar(meanform)-1)
      meanform <- paste0("c(", meanform, ")")
      
      sdform <- c()
      for(r in 1:params$numcnds2){
        for(i in 1:params$numcnds1){
          sdform <- paste0(sdform, "params$g", i, r, "sd", ",")
        }
      }
      sdform <- substr(sdform, start = 1, stop = nchar(sdform)-1)
      sdform <- paste0("c(", sdform, ")")
      
      sizeform <- c()
      for(r in 1:params$numcnds2){
        for(i in 1:params$numcnds1){
          sizeform <- paste0(sizeform, "params$g", i, r, "size", ",")
        }
      }
      sizeform <- substr(sizeform, start = 1, stop = nchar(sizeform)-1)
      sizeform <- paste0("c(", sizeform, ")")
      
      # formula for IV1 and IV2
      text1 <- c()
        for(i in 1:params$numcnds1){
          text1 <- c(text1, eval(parse(text = paste0("input$lvl1names", i))))
        }
      
      text2 <- c()
      for(i in 1:params$numcnds2){
        text2 <- c(text2, rep(eval(parse(text = paste0("input$lvl2names", i))),
                              params$numcnds1))
      }
      
      aggre <- data.frame(Mean = eval(parse(text = meanform)),
                          SD = eval(parse(text = sdform)),
                          IV1 = text1,
                          IV2 = text2)
      
      
     ggplot(data = aggre, aes(x = IV1, y = Mean, fill = IV2, label = SD)) + 
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_classic() + 
        theme(axis.title.y = element_text(size = 20),
              axis.title.x = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 20)) + 
          xlab(params$iv1name) + 
          labs(fill = params$iv2name)
    }
  })
}

# run
shinyApp(ui = ui, server = server)
