## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Additive Hazards"),
                  # Model for Survival Analysis"),
  dashboardSidebar(
   sidebarMenu(
      menuItem(strong("Intro", style="font-size: 14pt"), tabName = "dashboard", icon = icon("dashboard")),
      menuItem(strong("Data", style="font-size: 14pt"), tabName = "data", icon = icon("database")),
      menuItem(strong("Model", style="font-size: 14pt"), tabName = "model", icon = icon("yahoo")),
     # menuItem("Sampling", tabName = "sampling", icon = icon("bitbucket")),
      menuItem(strong("Results", style="font-size: 14pt"), tabName = "results", icon = icon("magic")),
      menuItem(strong("Plots", style="font-size: 14pt"), tabName = "plots", icon = icon("line-chart"))

  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
                fluidPage(
                  titlePanel("Introduction"),
                  
                  p("This web app is a tool for obtaining parameter estimates from single- and 
                    two-phase additive hazards models and visualizing predicted individual-specific hazards.
                    For fitting two-phase models with auxiliary information, it also supports in-app 
                    calculation of calibration variables.", 
                    style = "font-family: 'Calibri'; font-size: 14pt"),
                  
                  p("Currently supported input file types include comma separated values, Excel spreadsheets,
                    delimited text files, as well as SAS, Stata (up to Version 12), and SPSS data sets. 
                    Estimates with model-based or robust standard errors and figures may be downloaded for further manipulation.",
                    style = "font-family: 'Calibri'; font-size: 14pt"),
  
                  p("Documentation for the associated R package may be found", 
                  a("here.", href="https://cran.r-project.org/web/packages/addhazard/addhazard.pdf"), 
                    style = "font-family: 'Calibri'; font-size: 14pt")
                )),

      # Second tab content
      tabItem(tabName = "data",
              fluidPage(
                titlePanel("Data Upload"),

                tags$head( 
                  tags$link(rel = "stylesheet", type = "text/css", href="ah.css")
                 ),
                
                  sidebarPanel(
                    
                   tags$b("Set File Options"),
                    checkboxInput('header', 'Variable names as headers', TRUE),
                    radioButtons('sep', 'Field separator character',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('quote', 'Quoting characters',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '"'),
                    fileInput('file1', 'Choose File (.csv, .xlsx, .txt, .dta, .sav, .sas7bdat)',
                              accept=c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')),
                    tags$hr()
                  ),
              
                  mainPanel(dataTableOutput('contents'))
               )
           ),
      # third tab item 
      tabItem(tabName ="model",
              fluidPage(
                withMathJax(helpText("Additive Hazards Model $$\\lambda(t|Z=z) =\\lambda_0(t) + \\beta^Tz$$")),
                
                sidebarPanel(
                  uiOutput("surv"),
                  uiOutput("cen"),
                  uiOutput("covariates"),
                  #checkboxInput('wgts', 'Weights', FALSE),
                  #uiOutput("weights"),
                  checkboxInput('robust', 'Robust Standard Errors', TRUE),
                  
                  # from Sampling tab
                  # selectInput("Sampling", "Sampling Scheme:",
                  #            choices=c("Random Sampling", "Two-phase Sampling")),
                  #h6("If two-phase sampling:"),

                  #uiOutput("phase1"),
                  #uiOutput("phase2"),
                  checkboxInput('ties', 'Ties', TRUE),
                  
                  checkboxInput('twophase', 'Two-Phase Sampling', FALSE),
                  helpText("Uncheck if records were randomly sampled."),
                  
                  uiOutput("R"),
                  uiOutput("p2probs"),
                  #checkboxInput('calibration', 'Calibration', FALSE),
                  # helpText("Sometimes you need to create new calibration variables based on phase I variables.
                  #          The key is to find the variables highly correlated with phase II variable "),
                  #uiOutput("cal"),
                  #helpText("The inverse of the phase II selection probability for each subject. 
                  #         It is a number greater than or equal to 1."),
                  
                  actionButton("fitModel", "Fit Model")  
                  # verbatimTextOutput("modelSummary")  
                ),
                  mainPanel(
                    tableOutput("regTab")
                    #textOutput("text1")
                    )               
              ) # end fluidPage
      ), # end tabItem
      
      ## fourth tab 
      tabItem(tabName ="Two-phase Sampling",
              fluidPage(
                #  selectInput("Sampling", "Sampling Scheme:",
                #            choices=c("Random Sampling", "Two-phase Sampling")),
                #  h6("If two-phase sampling:"),

                  uiOutput("phase1"),
                  uiOutput("phase2")#,
                 # uiOutput("R"),
                 # uiOutput("weights"),
                 # helpText("The inverse of the phase II selection probability for each subject. It is a number greater or equal to 1."),
                  # checkboxInput('calibration', 'Calibration', TRUE),
                  # uiOutput("cal"),
                  #helpText("Sometimes you need to create new calibration variables based on phase I variables.
                  #         The key is to find the variables highly correlated with phase II variable ")

              ) # end fluidPage
          ), # end tabItem

      ## fifth tab
      tabItem( tabName = "results",
        fluidPage(
          titlePanel("Inference on coefficients"),
          mainPanel(
            plotOutput('plot')
           # navbarPage(
              # title = 'DataTable Options',
            #  tabPanel('Display length',    
            #           dataTableOutput('table')
            #  )
            #)
          )
         ) # endfluidPage
       ) # end tabItem
     
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage






