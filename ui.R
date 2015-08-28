## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Additive Hazards"),
                  # Model for Survival Analysis"),
  dashboardSidebar(
   sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Model", tabName = "model", icon = icon("yahoo")),
      menuItem("Sampling", tabName = "sampling", icon = icon("bitbucket")),
      menuItem("Results", tabName = "results", icon = icon("magic")),
      menuItem("Plots", tabName = "plots", icon = icon("line-chart"))

    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard"),

      # Second tab content
      tabItem(tabName = "data",
              fluidPage(
                titlePanel("Uploading Files"),

                tags$head( 
                  tags$link(rel = "stylesheet", type = "text/css", href="ah.css")
                 ),
                
                  sidebarPanel(
                    
                    fileInput('file1', 'Choose CSV File',
                              accept=c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')),
                    tags$hr(),
                    checkboxInput('header', 'Header', TRUE),
                    radioButtons('sep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('quote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '"')
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
                  checkboxInput('robust', 'Robust Standard Errors', TRUE)
                ),
                  mainPanel(
                    tableOutput("regTab")
                    #textOutput("text1")
                    )               
              )
      ),
      
      ## fourth tab 
      tabItem(tabName ="Two-phase Sampling",
              fluidPage(
                #  selectInput("Sampling", "Sampling Scheme:",
                #            choices=c("Random Sampling", "Two-phase Sampling")),
                #  h6("If two-phase sampling:"), 
                  uiOutput("phase1"),
                  uiOutput("phase2"),
                  uiOutput("R"),
                  uiOutput("weights"), 
                  helpText("The inverse of the phase II selection probability for each subject. It is a number greater or equal to 1."),
                  checkboxInput('calibration', 'Calibration', TRUE),
                  uiOutput("cal"),
                  helpText("Sometimes you need to create new calibration variables based on phase I variables. The key is to find the variables highly correlated with phase II variable ")
               )
              ), 
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
         )
       )
     
              
        
              
              
              
              
      
    )
  )
)






