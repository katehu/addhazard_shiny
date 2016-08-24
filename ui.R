## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Additive Hazards"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(strong("README", style="font-size: 14pt"), tabName = "dashboard", icon = icon("map-marker")),
      menuItem(strong("Upload", style="font-size: 14pt"), tabName = "data", icon = icon("cloud-upload")),
      menuItem(strong("Explore", style="font-size: 14pt"), tabName = "explore", icon = icon("search")),
      menuItem(strong("Model", style="font-size: 14pt"), tabName = "model", icon = icon("magic")),
      menuItem(strong("Interpret", style="font-size: 14pt"), tabName = "results", icon = icon("key")),
      menuItem(strong("Predict", style="font-size: 14pt"), tabName = "plots", icon = icon("line-chart"))

  )),
  
  dashboardBody(
    tabItems(
      
      #-----------
      # Intro tab
      #-----------
      
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
                    hazards model (Cox, 1972) would be more appropriate.",
                    style = "font-family: 'Calibri'; font-size: 14pt"),
  
                  p("Documentation for the associated 'addhazard' R package may be found", 
                  a("here.", href="https://cran.r-project.org/web/packages/addhazard/addhazard.pdf"),
                    "This web app was created with",
                  a("R,", href="http://www.r-project.org"),
                  a("Shiny,", href="http://shiny.rstudio.com"), " and",
                  a("shinydashboard", href="http://rstudio.github.io/shinydashboard"), 
                    "and includes methodologies from version 2.38 of the",
                  a("survival", href="http://CRAN.R-project.org/package=survival"), 
                    "R package (Therneau, 2015), as well as the following 
                    for data uploads:",
                  a("memisc,", href="http://CRAN.R-project.org/package=memisc"), 
                  a("xlsx,", href="http://CRAN.R-project.org/package=xlsx"), " and",
                  a("sas7bdat.", href="http://CRAN.R-project.org/package=sas7bdat"), 
                    style = "font-family: 'Calibri'; font-size: 14pt")
                )),

      #----------
      # Data tab
      #----------
      
      tabItem(tabName = "data",
              fluidPage(
                titlePanel("Data Upload"),

                tags$head( 
                  tags$link(rel = "stylesheet", type = "text/css", href="ah.css")
                 ),
                
                  sidebarPanel(
                    
                   tags$b("Set File Options"),
                    checkboxInput('header', 'Variable names as headers', TRUE),
                    radioButtons('sep', h5('Field Separator'),
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                   ','),
                    radioButtons('quote', h5('Quoting Characters'),
                                 c('Double Quotes'='"',
                                   'Single Quotes'="'",
                                   None=''),
                                   '"'),
                    fileInput('file1', 'Choose File',
                              accept=c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')),
                    uiOutput("showvars") #,
                    # helpText("Select and press 'Backspace' to delete variable name(s)")
                  ),
                
                  mainPanel(
                    dataTableOutput('contents')
                  )
               )
           ),
      
      #-------------
      # Explore tab
      #-------------
      
      tabItem(tabName ="explore",
              navbarPage(inverse = TRUE, 
                title = 'Data Exploration',
                tabPanel(title = tags$b('Histogram'),
                
                sidebarPanel(
                  uiOutput("histvar"),
                  checkboxInput('histprob', 'Show density on y-axis', FALSE),
                  numericInput('nbreaks', 'Customize Bin Width', NA, min = 2, max = 50, step=2, width='150px')
                ),
                
                mainPanel(
                  box(
                    title = "Histogram", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, width = 12,
                    plotOutput("hist")
                  )
                )
                ), 
              
                tabPanel(title = tags$b('Kaplan-Meier'), 
                  sidebarPanel(
                    uiOutput("cen0"),
                    uiOutput("surv0"),
                    checkboxInput('KMconfint', 'Show 95% confidence intervals', FALSE),
                    checkboxInput('KMticks', 'Show censoring times', FALSE),
                    checkboxInput('KMcuminc', 'Plot cumulative incidence', FALSE),
                    numericInput('KMheight', 'Zoom in', 0, min = 0, max = 0.99, step=0.1, width='150px'),
                    uiOutput("KMvar"),
                    checkboxInput('KMbygrp', 'Plot by group', FALSE)
                  ),
                  mainPanel(
                    box(
                      title = "Kaplan-Meier", status = "primary", solidHeader = TRUE,
                      collapsible = FALSE, width = 12, 
                      plotOutput("KM")
                    )
                  )
              ),
              
              tabPanel(title = tags$b('Scatter Plot'), 
                       sidebarPanel(
                         uiOutput("scatterX"),
                         uiOutput("scatterY")
                       ),
                       mainPanel(
                         box(
                           title = "Scatter Plot", status = "primary", solidHeader = TRUE,
                           collapsible = FALSE, width = 12, 
                           plotOutput("scatter")
                         )
                       )
              ) 
                
            )    
      ),
      
      #-----------
      # Model tab
      #-----------
      
      tabItem(tabName ="model",
              titlePanel("Data Modeling"),
              fluidPage(withMathJax(),
                
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
                  actionButton("fitModel", "Fit Model") 
                  ),
                  
                  mainPanel(
                  conditionalPanel("input.modeltype == 0",
                    hr(), hr(), hr(), hr(), hr(), br(),
                    box(
                      title = "Additional Settings", status = "primary", solidHeader = TRUE,
                      collapsible = FALSE, width = 6,
                      withMathJax(helpText("$$\\lambda(t|Z=z) =\\lambda_0(t)\\exp(\\beta^Tz)$$")),
                      radioButtons('Coxties', h5('Method for Ties'),
                                   c('Efron'='efron',
                                     'Breslow'='breslow',
                                     'Exact'='exact'),
                                     'efron')
                    ),
                    hr(), hr(), hr(),
                    helpText("The Efron approximation is more accurate when there are many tied event times. 
                              All three methods are equivalent if there are no ties, and they are statistically 
                              indistinguishable if the number of ties is small.")
                  ),
                                         
                  conditionalPanel("input.modeltype == 1",
                    hr(), hr(), hr(), hr(), hr(), br(),                    
                    box(
                      title = "Additional Settings", status = "primary", solidHeader = TRUE,
                      collapsible = FALSE, width = 6,
                      withMathJax(helpText("$$\\lambda(t|Z=z) =\\lambda_0(t) + \\beta^Tz$$")),
                      checkboxInput('ties', 'Ties', TRUE),
                      checkboxInput('wgts', 'Sampling weights used', FALSE),
                      uiOutput("weights")
                      )
                    ),
                  
                  conditionalPanel("input.modeltype == 2",
                    hr(), hr(), hr(), hr(), hr(), br(),
                    box(
                      title = "Additional Settings", status = "primary", solidHeader = TRUE,
                      collapsible = FALSE, width = 7,
                      withMathJax(helpText("$$\\lambda(t|Z=z) =\\lambda_0(t) + \\beta^Tz$$")),
                      uiOutput("R"),
                      uiOutput("p2p")
                      )
                    ),
                
                  conditionalPanel("input.modeltype == 3",
                    box(
                      title = "Additional Settings", status = "primary", solidHeader = TRUE,
                      collapsible = FALSE, width = 7,
                      withMathJax(helpText("$$\\lambda(t|Z=z) =\\lambda_0(t) + \\beta^Tz$$")),
                      uiOutput("Rcal"),
                      uiOutput("p2pcal"),
                      uiOutput("calvars"),
                      uiOutput("calVarlist"),
                      helpText("Leave blank if no calculations needed."),
                      uiOutput("calcVars"),
                      helpText("To transform a variable, enter desired transformation into the pop-up field(s) as a function of x, 
                             e.g.", code("x^2"), "to square a variable or", code("sqrt(x)"), "to take its square root. All mathematical
                               operators in the R language are supported.")
                    )
                  )
                  ) 
                  
                ) 
      ), 
      
      #-------------
      # Results tab
      #-------------
      
      tabItem( tabName = "results",
        fluidPage(
          titlePanel("Inference on Coefficients"),
          mainPanel(
            br(),
            box(
              title = "Results", status = "primary", solidHeader = TRUE,
              collapsible = FALSE, width = 11,
              uiOutput("modelEq"),
              tableOutput("regTab")
            )
          ) 
         ) 
       ), 

      #-----------
      # Plots tab
      #-----------
      
      tabItem(tabName ="plots",
              fluidPage(
                titlePanel("Hazard Prediction"),
                helpText("This feature is only available for additive hazard models."),
                sidebarPanel(
                  tags$b("Enter covariate value(s) for hazard prediction"),
                  br(),
                  br(),
                  uiOutput("modelCovariates"),
                  checkboxInput('predHazCI', 'Show 95% confidence intervals', FALSE),
                  numericInput('haztime', 'Timepoint of interest', NA, min = 0, max = NA, step=1)
                  ),
                mainPanel(
                  box(
                    title = "Predicted Hazards", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, width = 12,
                    plotOutput("plotPredHaz")
                  )
                )
        
              ) 
      ) 
      
     
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage