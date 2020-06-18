
# load packages and functions -----------------------------------------------------------
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

# containts anova1way() and anova2way()
source("functions.R")

# making UI ---------------------------------------------------------------
ui <- fluidPage(
  # select theme
  theme = shinytheme("flatly"),
  # title
  titlePanel(h1("Fully-Between ANOVA Power Simulator"),
             windowTitle = "FBA_PowerSim"),
  # description and introduction
  fluidRow(
    column(
      wellPanel(h3("Brief Description"),
        "This is a power simulator for Between-Subject ANOVAs and it is a personal project for myself to test my R and Shiny skills.",
                br(),
                tags$blockquote(strong("Power"), "is defined as the probability that an effect would be statistically significant (in a collected sample), given that the effect truly exists in the population."),
                "There are existing power calculators that uses mathematical formulas to calculate power 
                but these formulas make certain assumptions about the samples and population.",
                br(),
                "Another less restrictive approach to calculating power is to simulate (repeatedly) the drawing of samples from a known population where the effect exist.",
                tags$ul(
                  tags$li("For each sample, test the hypothesis. Each sample is an iteration."), 
                  tags$li("Power is the proportion of iterations that produced a statistically significant results.")
                ),
                "For example,",
                tags$ul(
                  tags$li("I generate 1000 samples, each sample contains 100 subjects, from a population where males have shorter hair compared to females."),
                  tags$li("In each sample, I use an ANOVA to determine if there is any mean diferences in hair length between males and females"),
                  tags$li("If 800 out of the 1000 iterations produced a statistically significant main effect of gender, the simulated power would be 0.8.")
                  ),
                "The limitations to this simulation-based approach is that an intermediate level of scripting is needed to implement this,
                      which may gatekeep beginner users from accessing this technique.",
                br(),
                      "This RShiny was created to allow users of all scripting level to access to such simulations, without the daunting task of writing complicated scripts.",
                br(),
      ),
      width = 7, offset = 0),
    column(
      wellPanel(h4("Access the README and codes on ", tags$a(href = "https://github.com/Aaron0696/BtwnANOVAPowSim", "Github.")),
                h4("Connect with me on ", tags$a(href = "https://www.linkedin.com/in/aaron-lim-b30898135/", "LinkedIn.")),
                h4("Leave feedback or comments anonymously on", tags$a(href = "https://forms.gle/LyZb3H64ZfwhKjcZ9", "Google Forms.")),
                h4("To run the application on your local machine, use R and install the ", tags$code('shiny'), "package."),
                h4("Run this command directly from the console:"),
                h4(tags$code('shiny::runGitHub("BtwnANOVAPowSim", "Aaron0696")'))),
      width = 4)
    ),
  
  hr(),
  
  fluidRow(
    column(width = 3,
           # general settings
           h2("Step One: General Settings"),
           
           hr(),
           
           wellPanel(
             radioButtons("numways", 
                          label = h3("ANOVA Design"),
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
             
           )
    ),
    column(
      # panel for condition parameters that is dependent on the number of conditions selected
      h2("Step Two: Condition Parameters"),
      
      hr(),
      h4("Input the expected ", tags$em("population"), " unstandardized means, ", tags$em("population"), " standard deviations and expected condition sample sizes in the panels below. 
         Greater differences in means, smaller standard deviations and larger condition sizes will lead to greater power."),
      hr(),
      
      column(width = 6, 
             conditionalPanel(condition = "input.numways == 'B2'",
                              wellPanel(textInput("iv1name", 
                                                  label = h3("First Independent Variable"), 
                                                  value = "IV1"),
                                        uiOutput("numgrp.sel1")
                              )
             ),
             conditionalPanel(condition = "input.numways == 'B2'",
                              uiOutput("lvlnames1")
                              )
             ),
      
      column(width = 6, 
             conditionalPanel(condition = "input.numways == 'B2'",
                              wellPanel(textInput("iv2name", 
                                                  label = h3("Second Independent Variable"),
                                                  value = "IV2"),
                                        uiOutput("numgrp.sel2")
                              )
             ),
             conditionalPanel(condition = "input.numways == 'B2'",
                              uiOutput("lvlnames2")
             )
      ),
      
      conditionalPanel(condition = "input.numways == 'B1'",
                       wellPanel(textInput("ivname", 
                                           label = h3("First Independent Variable"), 
                                           value = "X1"),
                                 uiOutput("numgrp.sel")
                       )
      ),
      conditionalPanel(condition = "input.numways == 'B1'",
                       uiOutput("cndparams1")
      ),
      fluidRow(
        conditionalPanel(condition = "input.numways == 'B2'",
                         uiOutput("cndparams2")
        )
      ),
      width = 7),
    
    column(
      h2("Step Three: Run!"), 
      
      hr(),
      
      # panel for the run button
      wellPanel(h3("All Done?"),
                h4("Click Run! when you are satisfied with your settings and 
                are ready to begin the simulation.
                The application may take a while so hang tight!"),
                hr(),
                actionButton("update", label = "Run!", icon = icon("random")),
                h4("Output will be generated below...")), 
      width = 2)
  ),
  
  hr(),
  
  fluidRow(
    column(
      # text communicating the simulated power
      conditionalPanel(condition = "input.update != 0",
                       wellPanel(h3("Simulated Power"),
                                 hr(),
                                 withSpinner(tableOutput("power"), 
                                             size = getOption("spinner.size", 
                                                              default = 0.5),
                                             proxy.height = "100px")
                                 )
                       ), 
      offset = 3, width = 2),
    
    column(
      # description for the graph
      conditionalPanel(condition = "input.update != 0",
                       h3("The graph below visualizes the condition means as bars."), 
                       h4("Doublecheck that the displayed means correspond to the values 
                          selected above and are the intended values."),
                       plotOutput("graph")),
      width = 4),
  ),
  
  hr(),
)

# making server ------------------------------------------------------------------
server <- function(input, output) {
  # ui for selecting number of groups/levels for 1 way
  output$numgrp.sel <- renderUI({
    
    sliderInput("numcnds", label = h3("Number Of Conditions/Levels"), 
                min = 2, max = 9, value = 2)
  })
  
  # ui for selecting number of groups/levels for 2 way, first IV
  output$numgrp.sel1 <- renderUI({
    
    sliderInput("numcnds1", label = h3("Number Of Conditions/Levels"), 
                min = 2, max = 9, value = 2)
  })
  
  # ui for inputing the names of the groups/levels for 2 way, first IV
  output$lvlnames1 <- renderUI({
    lapply(1:input$numcnds1, function(i){
      column(
        wellPanel(
          textInput(paste0("lvl1names", i), 
                    label = paste0("Level ", i, " Name"), 
                    value = paste(i)
          )
        ),
        width = 6)
    })
  })
  
  # ui for inputing the number of the groups/levels for 2 way, second IV
  output$numgrp.sel2 <- renderUI({
    
    sliderInput("numcnds2", label = h3("Number Of Conditions/Levels"), min = 2, max = 9, value = 2)
  })
  
  # ui for inputing the names of the groups/levels for 2 way, second IV
  output$lvlnames2 <- renderUI({
    lapply(1:input$numcnds2, function(i){
      column(
        wellPanel(
          textInput(paste0("lvl2names", i), 
                    label = paste0("Level ", i, " Name"), 
                    value = LETTERS[i])
        ),
        width = 6) 
    })
  })
  
  # take number of conditions as input and
  # create dynamic UI that take means as input
  # for one way
  output$cndparams1 <- renderUI({
    numcnds <- as.integer(input$numcnds)
    lapply(1:numcnds, function(i){
      # header <- eval(parse(text = paste0("input$lvlnames", i)))
      column(
        wellPanel(
          textInput(paste0("lvlnames", i), 
                    label = paste0("Level ", i, " Name"), 
                    value = LETTERS[i]),
          numericInput(paste0("g",i,"mean"),
                       label = paste0("Population Mean"),
                       value = i/2),
          numericInput(paste0("g",i,"sd"),
                       label = paste0("Population SD"),
                       value = 1),
          numericInput(paste0("g",i,"size"),
                       label = paste0("Condtion Size"),
                       value = 50)), width = 4) 
    })
  })
  
  # for two way
  output$cndparams2 <- renderUI({
    numcnds1 <- as.integer(input$numcnds1)
    numcnds2 <- as.integer(input$numcnds2)
    lapply(1:numcnds1, function(i){
      lapply(1:numcnds2, function(r){
        
        column(
          wellPanel(
            h3(paste0(input$iv1name, ": ", eval(parse(text = paste0("input$lvl1names", i))))),
            h3(paste0(input$iv2name, ": ", eval(parse(text = paste0("input$lvl2names", r))))),
            
            numericInput(paste0("g",i, r, "mean"),
                         label = paste0("Population Mean"),
                         value = i/2),
            numericInput(paste0("g",i, r, "sd"),
                         label = paste0("Population SD"),
                         value = 1),
            numericInput(paste0("g",i, r, "size"),
                         label = paste0("Condition Size"),
                         value = 50)), width = 3)
      })
    })
  })
  
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
    
    # if one way
    if (input$numways == "B1"){
      for (r in 1:input$numcnds){
        params[[paste0("g",r,"mean")]] <- input[[paste0("g",r,"mean")]]
        params[[paste0("g",r,"sd")]] <- input[[paste0("g",r,"sd")]]
        params[[paste0("g",r,"size")]] <- input[[paste0("g",r,"size")]]
      }
    }
    # f two way
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
  output$power <- renderTable({
    # for the first time before the run button is pressed, say loading
    if(input$update == 0){
      paste0("Loading!")
    } else {
      
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
        
        # loop repeating anova1way() with the chosen parameters for the number of
        # iterations specified by the user 
        for(i in 1:params$iter){
          tracker$NumSig <- tracker$NumSig + anova1way(means = eval(parse(text = meanform)),
                                                       sds = eval(parse(text = sdform)),
                                                       grpsize = eval(parse(text = sizeform)),
                                                       num.grp = params$numcnds,
                                                       alphalvl = params$alpha)
        }
        
        # save the number of iterations from params
        tracker$NumIterations <- params$iter
        # calculate power
        tracker$Power <- tracker$NumSig / tracker$NumIterations
        tracker[,c(1,4)]
        
      } else if(input$numways == "B2"){
        # tracker is a dataframe to keep count the number of significant effects
        tracker <- data.frame(Effect = c(params$iv1name, 
                                         params$iv2name, 
                                         paste0(params$iv1name, "*", params$iv2name), 
                                         "Error"),
                              NumSig = 0)
        
        # creating the formulas to be parsed as arguments
        # mean formula
        meanform <- c()
        for(r in 1:params$numcnds2){
          for(i in 1:params$numcnds1){
            meanform <- paste0(meanform, "params$g", i, r, "mean", ",")
          }
        }
        meanform <- substr(meanform, start = 1, stop = nchar(meanform)-1)
        meanform <- paste0("c(", meanform, ")")
        
        # sd formula
        sdform <- c()
        for(r in 1:params$numcnds2){
          for(i in 1:params$numcnds1){
            sdform <- paste0(sdform, "params$g", i, r, "sd", ",")
          }
        }
        sdform <- substr(sdform, start = 1, stop = nchar(sdform)-1)
        sdform <- paste0("c(", sdform, ")")
        
        # size formula
        sizeform <- c()
        for(r in 1:params$numcnds2){
          for(i in 1:params$numcnds1){
            sizeform <- paste0(sizeform, "params$g", i, r, "size", ",")
          }
        }
        sizeform <- substr(sizeform, start = 1, stop = nchar(sizeform)-1)
        sizeform <- paste0("c(", sizeform, ")")
        
        # loop
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
  }, 
  striped = TRUE,  
  hover = TRUE, 
  spacing = 'm',  
  align = 'c',  
  digits = 2)
  
  # plot output
  output$graph <- renderPlot({
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
      
      # formula for IV1 and IV2 to input level names into dataframe
      text1 <- c()
      for(i in 1:params$numcnds){
        text1 <- c(text1, eval(parse(text = paste0("input$lvlnames", i))))
      }
      
      # create the dataframe that contains the aggregated means of the IVs
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
      # same as above
      # means
      meanform <- c()
      for(r in 1:params$numcnds2){
        for(i in 1:params$numcnds1){
          meanform <- paste0(meanform, "params$g", i, r, "mean", ",")
        }
      }
      meanform <- substr(meanform, start = 1, stop = nchar(meanform)-1)
      meanform <- paste0("c(", meanform, ")")
      # sds
      sdform <- c()
      for(r in 1:params$numcnds2){
        for(i in 1:params$numcnds1){
          sdform <- paste0(sdform, "params$g", i, r, "sd", ",")
        }
      }
      sdform <- substr(sdform, start = 1, stop = nchar(sdform)-1)
      sdform <- paste0("c(", sdform, ")")
      # sizes
      sizeform <- c()
      for(r in 1:params$numcnds2){
        for(i in 1:params$numcnds1){
          sizeform <- paste0(sizeform, "params$g", i, r, "size", ",")
        }
      }
      sizeform <- substr(sizeform, start = 1, stop = nchar(sizeform)-1)
      sizeform <- paste0("c(", sizeform, ")")
      
      # formula for IV1 and IV2 to create aggre dataframe
      # IV1
      text1 <- c()
        for(i in 1:params$numcnds1){
          text1 <- c(text1, eval(parse(text = paste0("input$lvl1names", i))))
        }
      # IV2
      text2 <- c()
      for(i in 1:params$numcnds2){
        text2 <- c(text2, rep(eval(parse(text = paste0("input$lvl2names", i))),
                              params$numcnds1))
      }
      # same
      aggre <- data.frame(Mean = eval(parse(text = meanform)),
                          SD = eval(parse(text = sdform)),
                          IV1 = text1,
                          IV2 = text2)
      
      # formula for converting IV2 into a factor with the correct levels
      text2.1 <- c()
      for(i in 1:params$numcnds2){
        text2.1 <- c(text2.1, eval(parse(text = paste0("input$lvl2names", i))))
      }
      # change levels to the correct order
      aggre$IV2 <- factor(aggre$IV2,
                          levels = text2.1)

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
