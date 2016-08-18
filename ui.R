## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Additive Hazards"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(strong("Intro", style="font-size: 14pt"), tabName = "dashboard", icon = icon("dashboard")),
      menuItem(strong("Data", style="font-size: 14pt"), tabName = "data", icon = icon("database")),
      menuItem(strong("Explore", style="font-size: 14pt"), tabName = "explore", icon = icon("search")),
      menuItem(strong("Model", style="font-size: 14pt"), tabName = "model", icon = icon("yahoo")),
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
                    two-phase additive hazards models (Lin, 1997) and visualizing predicted individual-specific hazards.
                    For fitting two-phase models with auxiliary information, it also supports in-app 
                    calculation of calibration variables, which are observed for all subjects and are most
                    useful if correlated with the characteristics additionally ascertained in the
                    smaller Phase II sample.", 
                    style = "font-family: 'Calibri'; font-size: 14pt"),
                  
                  p("Supported input file types include comma separated values, Excel spreadsheets,
                    delimited text files, as well as SAS, Stata (up to Version 12), and SPSS data sets. 
                    Tools for visualizing distributions and time-to-event trajectories are available to inform 
                    whether hazards are additive or multiplicative, in which case the traditional Cox proportional 
                    hazards model (Cox, 1972) would be more appropriate. Estimates with model-based or robust 
                    standard errors and hazard plots may be downloaded for further manipulation.",
                    style = "font-family: 'Calibri'; font-size: 14pt"),
  
                  p("Documentation for the associated 'addhazard' R package may be found", 
                  a("here.", href="https://cran.r-project.org/web/packages/addhazard/addhazard.pdf"),
                    "This web app was created with",
                  a("R", href="www.r-project.org"), ",", 
                  a("Shiny", href="shiny.rstudio.com"), ", and",
                  a("shinydashboard", href="rstudio.github.io/shinydashboard"), 
                    "and includes methodologies from version 2.38 of the",
                  a("survival", href="http://CRAN.R-project.org/package=survival"), 
                    "R package (Therneau, 2015) and the following 
                    for data uploads:",
                  a("memisc", href="http://CRAN.R-project.org/package=memisc"), ",",
                  a("xlsx", href="http://CRAN.R-project.org/package=xlsx"), ", and",
                  a("sas7bdat", href="http://CRAN.R-project.org/package=sas7bdat"), ".", 
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
                    hr(),
                    uiOutput("showvars")#,
                    # numericInput('numdec', 'Number of decimal places', 2, min = 0, max = 10, step=1)
                  ),
                
                  mainPanel(
                    dataTableOutput('contents')
                  )
               )
           ),
      
      # Third tab content
      tabItem(tabName ="explore",
              navbarPage(
                title = 'Data Visualization',
                tabPanel(title='Histogram',
                
                sidebarPanel(
                  uiOutput("histvar"),
                  checkboxInput('histprob', 'Show density on y-axis', FALSE),
                  numericInput('nbreaks', 'Number of breaks', NA, min = 2, max = 50, step=2, width='150px')
                  # tags$b("Set histogram color using RGB inputs"),
                  # sliderInput('colR', 'Red', value=0, min = 0, max = 1, step=0.1, ticks=F, width='150px'),
                  # sliderInput('colG', 'Green', value=0, min = 0, max = 1, step=0.1, ticks=F, width='150px'),
                  # sliderInput('colB', 'Blue', value=0.5, min = 0, max = 1, step=0.1, ticks=F, width='150px'),
                  # sliderInput('alpha', 'Opacity', value=0.3, min = 0, max = 1, step=0.1, ticks=F, width='150px')
                ),
                mainPanel(
                  plotOutput("hist")
                )
                ), # end first tabPanel
              
                tabPanel(title='Kaplan-Meier',
                  sidebarPanel(
                    uiOutput("cen0"),
                    # helpText("Can leave blank if all subjects have an event."),
                    uiOutput("surv0"),
                    checkboxInput('KMconfint', 'Show 95% confidence intervals', FALSE),
                    checkboxInput('KMticks', 'Show censoring times', FALSE),
                    checkboxInput('KMcuminc', 'Plot cumulative incidence', FALSE),
                    numericInput('KMheight', 'Zoom in', 0, min = 0, max = 1, step=0.1, width='150px'),
                    uiOutput("KMvar"),
                    checkboxInput('KMbygrp', 'Plot by group', FALSE)
                  ),
                  mainPanel(
                    plotOutput("KM")
                  )
              ) # end second tabPanel
                
            ) # end navbarPage      
      ),
      
      # Fourth tab content 
      tabItem(tabName ="model",
              fluidPage(
                withMathJax(helpText("Additive Hazards Model $$\\lambda(t|Z=z) =\\lambda_0(t) + \\beta^Tz$$")),
                
                sidebarPanel( 
                  radioButtons('modeltype', 'Choose Model',
                               c('Cox Proportional Hazards'=0,
                                 'Single-Phase Additive Hazards'=1,
                                 'Two-Phase Additive Hazards'=2,
                                 'Calibrated Two-Phase Additive Hazards'=3),
                                 NA),
                  tags$b("General Settings"),
                  uiOutput("cen"),
                  uiOutput("surv"),
                  uiOutput("covariates"),
                  checkboxInput('robust', 'Robust Standard Errors', TRUE),
                  helpText("Uncheck to estimate model-based standard errors."),
                  br(),
                  actionButton("fitModel", "Fit Model") 
                  ),
                  
                  mainPanel(
                  conditionalPanel("input.modeltype == 0",
                    br(),    
                    tags$b("Cox Proportional Hazards Model Settings"),
                    br(), 
                    radioButtons('Coxties', 'Method for Ties',
                                 c('Efron'='efron',
                                   'Breslow'='breslow',
                                   'Exact'='exact'),
                                 'efron'),
                    br(),
                    helpText("The Efron approximation is more accurate when there are many tied event times. 
                              All three methods are equivalent if there are no ties, and they are statistically 
                              indistinguishable if the number of ties is small.")
                  ),
                                         
                  conditionalPanel("input.modeltype == 1",
                    br(),              
                    tags$b("Single-Phase Additive Hazards Model Settings"),
                    checkboxInput('ties', 'Ties', TRUE),
                    checkboxInput('wgts', 'Sampling weights used', FALSE),
                    uiOutput("weights")
                    ),
                  
                  conditionalPanel("input.modeltype == 2",
                    br(),
                    tags$b("Two-Phase Additive Hazards Model Settings"),
                    br(),
                    uiOutput("R"),
                    uiOutput("p2p")
                    ),
                
                  conditionalPanel("input.modeltype == 3",
                    br(),
                    tags$b("Calibrated Two-Phase Additive Hazards Model Settings"),
                    br(),
                    uiOutput("Rcal"),
                    uiOutput("p2pcal"),
                    radioButtons('calib', 'Choose one',
                                 c('All calibration variables available in data set'=0,
                                   'Some calibration variables available in data set'=1,
                                   'Need to calculate all calibration variables'=2),
                                   NA),
                    uiOutput("calvars")
                    )
                  ) # end mainPanel
                  
                ) # end fluidPage
      ), # end tabItem
      
      ## Fifth tab content
      tabItem( tabName = "results",
        fluidPage(
          titlePanel("Inference on Coefficients"),
          mainPanel(
            #br(),
            #numericInput('numdec', 'Number of decimal places to show', 2, min = 0, max = 10, step=1),
            br(),
            tags$b("Table 1. Parameter estimates with 95% confidence intervals."),
            tableOutput("regTab"),
            br(),
            downloadButton('downloadTab', 'Download Table')
          ) 

         ) # endfluidPage
       ), # end tabItem

      ## Last tab content
      tabItem(tabName ="plots",
              fluidPage(
                titlePanel("Predicted Hazards"),
                sidebarPanel(
                  tags$b("Enter covariate value(s) for hazard prediction"),
                  br(),
                  br(),
                  uiOutput("modelCovariates"),
                  checkboxInput('predHazCI', 'Show 95% confidence intervals', FALSE),
                  numericInput('haztime', 'Timepoint of interest', NA, min = 0, max = NA, step=1)
                  ),
                mainPanel(
                  plotOutput("plotPredHaz")
                )
        
              ) # end fluidPage
      ) # end tabItem
      
     
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage