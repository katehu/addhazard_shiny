## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Additive Hazards"),
  dashboardSidebar(
   sidebarMenu(
      menuItem(strong("Intro", style="font-size: 14pt"), tabName = "dashboard", icon = icon("dashboard")),
      menuItem(strong("Data", style="font-size: 14pt"), tabName = "data", icon = icon("database")),
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
      
      # Third tab content 
      tabItem(tabName ="model",
              fluidPage(
                withMathJax(helpText("Additive Hazards Model $$\\lambda(t|Z=z) =\\lambda_0(t) + \\beta^Tz$$")),
                
                #mainPanel(   
                fluidRow(
                  column(3,
                  tags$b("General Settings"),
                  br(),
                  uiOutput("surv"),
                  uiOutput("cen"),
                  uiOutput("covariates"),
                  checkboxInput('robust', 'Robust Standard Errors', TRUE),
                  helpText("Uncheck to estimate model-based standard errors."),
                  tags$b("Choose Model"),
                  checkboxInput('twophase', 'Two-Phase Model', FALSE),
                  br(),
                  actionButton("fitModel", "Fit Model") 
                  ),
                  
                  column(3, offset = 1,
                  tags$b("Single-Phase Model Settings"),
                  checkboxInput('ties', 'Ties', TRUE),
                  checkboxInput('wgts', 'Sampling weights used', FALSE),
                  uiOutput("weights")
              
                  ),
                  
                  column(3, offset = 1,
                  tags$b("Two-Phase Model Settings"),
                  br(),
                  uiOutput("R"),
                  uiOutput("p2probs")
                  
                  #checkboxInput('calibration', 'Calibration', FALSE),
                  # helpText("Sometimes you need to create new calibration variables based on phase I variables.
                  #          The key is to find the variables highly correlated with phase II variable "),
                  #uiOutput("cal"),
                  #helpText("The inverse of the phase II selection probability for each subject. 
                  #         It is a number greater than or equal to 1."),
                  
                  )
                  
                  ) # end fluidRow
                ) # end fluidPage
      ), # end tabItem
      
      ## Fourth tab content
      tabItem( tabName = "results",
        fluidPage(
          titlePanel("Inference on Coefficients"),
          mainPanel(
            tableOutput("regTab")
          ) 

         ) # endfluidPage
       ), # end tabItem
      
      # navbarPage(
      #  title = 'DataTable Options',
      #   tabPanel('Display length',
      #            dataTableOutput('table1')
      #   )
      #  ) # end navbarPage
      # ), # end tabItem
      
      ## Fifth tab content
      tabItem(tabName ="plots",
              fluidPage(
                titlePanel("Plots"),
                sidebarPanel(
                  checkboxInput('histprob', 'Show density on y-axis', FALSE),
                  numericInput('nbreaks', 'Number of breaks', NA, min = 2, max = 50, step=5, width='150px'),
                  tags$b("Set histogram color using RGB inputs"),
                  sliderInput('colR', 'Red', value=0, min = 0, max = 1, step=0.1, ticks=F, width='150px'),
                  sliderInput('colG', 'Green', value=0, min = 0, max = 1, step=0.1, ticks=F, width='150px'),
                  sliderInput('colB', 'Blue', value=0.5, min = 0, max = 1, step=0.1, ticks=F, width='150px'),
                  sliderInput('alpha', 'Opacity level', value=0.3, min = 0, max = 1, step=0.1, ticks=F, width='150px')
                ),
                mainPanel(
                  plotOutput("plot"),
                  tags$b("Figure 1. Histogram of survival times.")
                )
        
              ) # end fluidPage
      ) # end tabItem
      
     
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage