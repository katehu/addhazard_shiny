# setwd("/Users/kate.hu/addhazard_shiny") 

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
# sourceDir("/Users/kate.hu/Documents/ah/R",trace=TRUE)
library(shiny)
library(xtable)
library(addhazard)
library(survival)

## packages required for file imports: memisc, xlsx, foreign, sas7bdat

shinyServer(function(input, output) {
  
  #-------------
  # data upload
  #-------------
   data  <- reactive({
      inFile <- input$file1
   
      k <- nchar(inFile) 
      if (substr(inFile[[1]], k-2, k) == "csv"){
          dat <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
   
      } else if (substr(inFile[[1]], k-2, k) == "txt"){
          dat <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
   
      } else if (substr(inFile[[1]], k-3, k) == "xlsx"){
          library(xlsx)
          if (input$header == T){
             dat <- read.xlsx2(inFile$datapath, sheetIndex=1, startRow=1)
          } else { 
             dat <- read.xlsx2(inFile$datapath, sheetIndex=1, startRow=2)
      } 
   
      } else if (substr(inFile[[1]], k-2, k) == "dta"){
          library(foreign)
          dat <- read.dta(inFile$datapath)
         
      } else if (substr(inFile[[1]], k-2, k) == "sav"){
          library(memisc)
          dat <- as.data.frame(as.data.set(spss.system.file(inFile$datapath)))
   
      } else if (substr(inFile[[1]], k-7, k) == "sas7bdat"){
          library(sas7bdat)
          dat <- read.sas7bdat(inFile$datapath)
      }
   
      dat <- data.frame(dat)
   return(dat)
   })
  
   output$contents <- renderDataTable({
        # input$file1 will be NULL initially. After the user selects and uploads a
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
        # columns. The 'datapath' column will contain the local filenames where the
        # data can be found.
        
        inFile <- input$file1
        dat <- data()
        dat <- round(dat, 2) #input$numdec) # later change to user-defined 
        
        if (is.null(inFile)){
          return(NULL)
        } else {
          return(dat[, c(unlist(input$showvars))])
        }
      }, options = list(pageLength = 10, orderClass = TRUE)
   )
  
  #------------
  # variables
  #------------
   
  output$showvars <- renderUI({  
    selectizeInput('showvars', 
                   'Displayed columns', 
                    names(data()),
                    selected = names(data()), multiple = TRUE)
  })
   
  output$histvar <- renderUI({
    selectInput("histvar", label = h4("Select Variable"), choices = c('', names(data())),
                selected = '')
  })
  
  output$KMvar <- renderUI({
    selectInput("KMvar", label = h5("Select Group Variable"), choices = c('', names(data())),
                selected = '')
  })
  
  output$surv0 <- renderUI({
    selectInput("surv0", label = h5("Survival Outcomes"), choices = c('',names(data())),
                selected = '')
  })
  
  
  output$cen0 <- renderUI({
    selectInput("cen0", label = h5("Censoring Indicator"), choices = c('', names(data())),
                selected = '')
  })
  
 
  output$surv <- renderUI({
    selectInput("surv", label = h5("Survival Outcomes"), choices = c('', names(data())),
                selected = '')
  })
  
  
  output$cen <- renderUI({
    selectInput("cen", label = h5("Censoring Indicator"), choices = c('', names(data())),
                selected = '')
  })
  
  output$covariates <- renderUI({
  selectizeInput("covariates", label = h5("Multi-select Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  output$R <- renderUI({
    selectInput("R", label = h5("Phase II Membership"), choices = c('', names(data())),
                selected = '')
  })
  
  output$weights<- renderUI({
    selectInput("weights", label = h5("Weights"), choices = c('', names(data())),
                selected = '')
  })
  
  output$p2p <- renderUI({
    selectInput("p2p", label = h5("Phase II Subsampling Probabilities"), choices = c('', names(data())),
                selected = '')
  })
  
  output$Rcal <- renderUI({
    selectInput("Rcal", label = h5("Phase II Membership"), choices = c('', names(data())),
                selected = '')
  })
  
  output$p2pcal <- renderUI({
    selectInput("p2pcal", label = h5("Phase II Subsampling Probabilities"), choices = c('', names(data())),
                selected = '')
    
  })
  
  output$calvars <- renderUI({
    selectizeInput("calvars", label = h5("Select calibration variable(s) from data set"), choices = names(data()), multiple = TRUE)
  })

  #-----------
  # fit model
  #-----------
  observeEvent(input$fitModel, {
  
    dat <- data() 
    
    if (!is.null(input$modeltype) & !is.null(input$covariates) &
        input$surv != input$cen & is.numeric(dat[, unlist(input$surv)]) & 
      !(input$surv %in% input$covariates) & !(input$cen %in% input$covariates)){
        
      ## Cox model
      if (input$modeltype == 0){
          fit1 <- coxph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))), 
                        data=dat, robust=input$robust, ties=input$Coxties)
      }
      
      ## additive hazards model
      if (input$modeltype == 1){
         if (input$wgts == T){
          dat$w <- dat[, unlist(input$weights)]
          fit1 <- ah(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),
                    data=dat, robust=input$robust, ties=input$ties, weights=w)
         } else {
           fit1 <- ah(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),
                     data=dat, robust=input$robust, ties=input$ties)
         }
        
      ## additive hazards model with 2-phase sampling
      } else if (input$modeltype == 2) {
         dat$R <- dat[, unlist(input$R)]            
         dat$Pi <- dat[, unlist(input$p2p)]
         fit1 <- ah.2ph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),
                        data=dat, robust=input$robust, R=R, Pi=Pi)
      
      ## additive hazards model with 2-phase sampling and calibration
      } else if (input$modeltype == 3) {
         dat$R <- dat[, unlist(input$Rcal)]            
         dat$Pi <- dat[, unlist(input$p2pcal)]
         
         ## all calibration variables available
         if (input$calib == 0){
         fit1 <- ah.2ph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ", paste(input$covariates,collapse="+"))),
                        data=dat, robust=input$robust, R=R, Pi=Pi, calibration.variables = unlist(input$calvars))
         }
      }
    } 
      output$regTab <- renderTable({
        # headings: coef	 se	 lower.95	 upper.95	 z	 p.value
        summary(fit1)$coef
      }, digits = 4)
      
      output$plotPredHaz <- renderPlot({
        newdata <- data.frame(t(c(input$var1, input$var2, input$var3, input$var4, input$var5)))
        ncov <- length(unlist(input$covariates))
        newdata <- newdata[, 1:ncov]
        names(newdata) <- unlist(input$covariates)
        predtab <- predict(fit1, newdata, newtime = 1:max(dat[, unlist(input$surv)]), na.rm=T)
        
        vartab <- cbind(names(newdata), c(newdata[1,]))
        vars <- paste(vartab[,1], "=", vartab[,2], sep='')
        vars <- paste(vars, collapse = ", ")
        plot(predtab[,1], predtab[,2], type='l', xlab="Time", ylab="Predicted Hazard Rate",
             main=paste("Predicted Hazards Over Time \n", vars, sep=''))
        
        if (!is.na(input$haztime)){
          abline(v=input$haztime, col="red", lty=3)
        }
      })
      
  }) # end observeEvent
  
  #-----------
  # downloads
  #-----------
    
#     f <- function(a,b,c, ...){
#       
#     }
#   
#   mydata <- reactive(f(input$seed,
#                        input$ncases,
#                        input$branches))
  
  # printSummary <- renderPrint({
  #    tab <- fitModel()
  # })
  
  printSummary <- reactive({
    # Fetch the appropriate output table, depending on the value
    # of input$fitModel
    tab <- input$fitModel
    data.frame(tab)
    #data()
  })
  
  output$downloadTab <- downloadHandler(
    #filename = function() {
    #  paste('Model_estimates_', Sys.Date(), '.csv', sep='')
    #},
    filename = function() {"Model_estimates.csv"},
    content = function(filename) {
      write.csv(printSummary(), filename, row.names = F)  # mydata()
      # https://talesofr.wordpress.com/tag/shiny/
    }
  )
  
  #-----------
  # figures
  #-----------
   output$hist <- renderPlot({
      inFile <- input$file1
      dat <- data() 
      
      if (is.na(input$nbreaks)){
          hist(dat[, unlist(input$histvar)], main=paste("Histogram of", paste(input$histvar)), xlab=paste(input$histvar), 
          probability = input$histprob,
          col=rgb(input$colR, input$colG, input$colB, alpha=input$alpha))
        
      } else {
          hist(dat[, unlist(input$histvar)], main=paste("Histogram of", paste(input$histvar)), xlab=paste(input$histvar), 
           probability = input$histprob, breaks = input$nbreaks,
           col=rgb(input$colR, input$colG, input$colB, alpha=input$alpha))
      }
   }) 
   
   output$KM <- renderPlot({
     inFile <- input$file1
     dat <- data() 
     
     if (input$KMbygrp==F){
        KMfit <- survfit(as.formula(paste("Surv(", input$surv0, ",", input$cen0, ") ~ 1")), data=dat)
        if (input$KMcuminc==F){
          plot(KMfit, main="Overall Kaplan-Meier Survival Curve", xlab="Time", ylab="Proportion without Event",
               conf.int=input$KMconfint, mark.time=input$KMticks, ylim=c(input$KMheight, 1))
        } else {
          plot(KMfit, main="Overall Cumulative Inicidence Curve",  
               xlab="Time", conf.int=input$KMconfint, mark.time=input$KMticks, fun="event", 
               ylab="Cumulative Incidence", ylim=c(0, 1-input$KMheight))  
        }
       
     } else {
       KMfit <- survfit(as.formula(paste("Surv(", input$surv0, ",", input$cen0, ") ~", input$KMvar)), data=dat)
       ngroups <- length(table(dat[, unlist(input$KMvar)]))
       
       if (input$KMcuminc==F){
         plot(KMfit, main=paste("Kaplan-Meier Survival Curves by", paste(input$KMvar)), 
              xlab="Time", conf.int=input$KMconfint, mark.time=input$KMticks, 
              ylab="Proportion without Event", ylim=c(input$KMheight, 1), col=1:ngroups)
         legend("bottomleft", lty=1, paste(input$KMvar, "=", names(table(dat[, unlist(input$KMvar)])), sep=''), col=1:ngroups, bty='n')
       } else {
         plot(KMfit, main=paste("Cumulative Incidence Curves by", paste(input$KMvar)), 
              xlab="Time", conf.int=input$KMconfint, mark.time=input$KMticks, fun="event", 
              ylab="Cumulative Incidence", ylim=c(0, 1-input$KMheight), col=1:ngroups)
         legend("topleft", lty=1, paste(input$KMvar, "=", names(table(dat[, unlist(input$KMvar)])), sep=''), col=1:ngroups, bty='n')
         
       }
     }
   })
  
}) ## end document