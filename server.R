setwd("/Users/kate.hu/addhazard_shiny")
#data<-read.csv("aric.csv", header=TRUE)
#aric$survtime<-aric$SURVTIME+runif(dim(aric)[1],0,1)*1e-8
#write.csv(aric, file="newaric.csv")
#data<-read.csv("newaric.csv", header=TRUE)

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir("/Users/kate.hu/Documents/ah/R",trace=TRUE)
library(shiny)
library(xtable)
shinyServer(function(input, output) {
  
  data  <- reactive({
   inFile <- input$file1
   dat <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
   #return(colnames(data))
   dat <-data.frame(dat)
   return(dat)
   })
  
  #mydata <- dat()
  
  
  output$contents <- renderDataTable(
      {
        # input$file1 will be NULL initially. After the user selects and uploads a
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
        # columns. The 'datapath' column will contain the local filenames where the
        # data can be found.
        
        inFile <- input$file1
        if (is.null(inFile))
           return(NULL) else
        #read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
           return(data())
      }, options = list(pageLength = 10))
  

 
  output$surv <- renderUI({
   
    selectInput("surv", label = h5("Survival Outcomes"), choices = names(data()))
  })
  
  
  output$cen <- renderUI({
    selectInput("cen", label = h5("Censoring Indicator"), choices = names(data()))
  })
  
  output$covariates <- renderUI({
  selectizeInput("covariates", label = h5("Multi-select Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  
  output$phase1<- renderUI({
    selectInput("ph1.cov", label = h5("Phase I Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  output$phase2 <- renderUI({
    selectInput("ph2.cov", label = h5("Phase II Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  output$R <- renderUI({
    selectInput("R", label = h5("Phase II membership"), choices = names(data()))
  })
  
  output$weights<- renderUI({
    selectInput("weights", label = h5("Weights"), choices = names(data()))
  })
  
  
  output$cal <- renderUI({
    selectInput("cal", label = h5("Calibration variables"), choices = names(data()), multiple = TRUE)
  })
  
# output$regTab <- renderTable({
#    inFile <- input$file1
#  if (input$Sampling == "Random Sampling"){
     #data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote) 
 #   fit1 <- ah(Surv(input$surv, input$cen) ~ input$covariates, data = data )
#     summary(fit1)
#   }
    
    
#  })

# output$plot <- renderPlot({
#   dat <- get(input$dataset)
#   hist(input$surv)
# })


runRegression <- reactive({
  #f1<-ah(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),data=data, ties=FALSE)
  #mysummary(f1)$coef
  
  lm(as.formula(paste(input$surv," ~ ",paste(input$covariates,collapse="+"))),data=data)
  
  })

output$regTab <- renderTable({
  if(!is.null(input$covariates)){
    summary(runRegression())$coefficients
  } else {
    print(data.frame(Warning="Please select Model Parameters."))
  }
 })



#  formula<-Surv(survtime,CHD)~crp+AGE+SEX+RACE+as.factor(SMOK)+SBP+LDL+HDL+DIABTS
 # fit1.2<-ah.2ph(formula,data=aric.LDL, R=aric.LDL$R, Pi=1/aric.LDL$wts, robust=robust,ties=FALSE)
  #su
  
})
