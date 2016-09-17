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
# library(xtable)
library(addhazard)
library(survival)

## packages required for file imports: memisc, xlsx, foreign, sas7bdat

shinyServer(function(input, output, session) {
  
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
        inFile <- input$file1
       
        if (is.null(inFile)){
          return(NULL)
        } else {
          dat <- data()
          
          ## round numeric variables 
          for (i in 1:ncol(dat)){
            if (is.numeric(dat[,i])){
              dat[,i] <- round(dat[,i], 2)
            }
          }
          
          return(dat[, c(unlist(input$showvars))])
        }
      }, options = list(pageLength = 10, orderClass = TRUE)
   )
   
   addPopover(session, "contents", "", content = paste0("Click on an arrow to the right of a column name to sort column in ascending or descending order. ", 
                                                        "Enter a value into the box under the column of interest to display only rows containing that value."),
              trigger = "hover", placement = "left", options = list(container = "body"))
  
  #------------
  # variables
  #------------
   
  output$showvars <- renderUI({  
    inFile <- input$file1
    
    if (!is.null(inFile)){
    selectizeInput("showvars", label = "Displayed Columns", 
                    choices = names(data()),
                    selected = names(data()), multiple = TRUE)
    } else {
      return()
    }
  })
  
  addTooltip(session, "showvars", title = paste0("To hide a data field, click on its name and press the delete key."),
            trigger = "hover", placement = "right", options = list(container = "body"))
  
  output$histvar <- renderUI({
    selectInput("histvar", label = tags$b("Select Variable"), choices = c('', names(data())),
                selected = '')
  })
  
  output$surv0 <- renderUI({
    selectInput("surv0", label = "Follow Up Time", choices = c('',names(data())),
                selected = '')
  })
  
  addTooltip(session, "surv0", title = "Index date to event or censoring date",
             trigger = "hover", placement = "right", options = list(container = "body"))
  
  output$cen0 <- renderUI({
    selectInput("cen0", label = "Status Indicator", choices = c('', names(data())),
                selected = '')
  })
  
  addTooltip(session, "cen0", title = "0=no event, 1=event",
             trigger = "hover", placement = "right", options = list(container = "body"))
  
  
  output$KMvar <- renderUI({
    selectInput("KMvar", label = "Group By Variable", choices = c('', names(data())),
                selected = '')
  })
  
  addTooltip(session, "KMvar", title = "Must be a categorical variable",
             trigger = "hover", placement = "right", options = list(container = "body"))
  
  
  output$scatterX <- renderUI({
    selectInput("scatterX", label = "X-axis Variable", choices = c('', names(data())),
                selected = '')
  })
  
  output$scatterY <- renderUI({
    selectInput("scatterY", label = "Y-axis Variable", choices = c('', names(data())),
                selected = '')
  })
  
  output$surv <- renderUI({
    selectInput("surv", label = h5("Follow Up Time"), choices = c('', names(data())),
                selected = '')
  })
  
  output$cen <- renderUI({
    selectInput("cen", label = h5("Status Indicator"), choices = c('', names(data())),
                selected = '')
  })
  
  output$covariates <- renderUI({
    selectizeInput("covariates", label = h5("Multi-select Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  addPopover(session, "covariates", "", content = "To exclude a selected covariate, click on its name and press the delete key.",
             trigger = "hover", placement = "right", options = list(container = "body"))
  
  
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
    selectizeInput("calvars", label = h5("Select existing variable(s)"), choices = names(data()), multiple = TRUE)
  })
  
  output$calVarlist <- renderUI({  
    selectizeInput("calVarlist", label = h5("Select variable(s) to transform"), 
                   choices = names(data()), multiple = TRUE)
  })
  
  output$calcVars <- renderUI({
    nvars <- length(unlist(input$calVarlist))
    if (nvars > 0){
      lapply(1:nvars, function(i){
        list(paste0("Enter expression for '", input$calVarlist[i], "'"), 
             textInput(paste0("expr", i), label=NA, value=NA))
      }
      )
    }
  })
  
  addPopover(session, "calcVars", "", content = paste0("Enter desired transformation as an expression of x, e.g. ", 
  "x^2 to square a variable or ", "sqrt(x) to take its square root. ",
  "Other commonly used mathematical operators include '+', '-', '*', and '/' for ",
  "addition, subtraction, multiplication, and division, respectively."),
             trigger = "hover", placement = "right", options = list(container = "body"))
  

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
      
      ## additive hazards model
      } else if (input$modeltype == 1){
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
         if (length(input$calVarlist) == 0){
            fit1 <- ah.2ph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ", paste(input$covariates,collapse="+"))),
                        data=dat, robust=input$robust, R=R, Pi=Pi, calibration.variables = unlist(input$calvars))
         
         ## calculate new variable(s)
         } else {
           last <- ncol(dat)
           exprlist <- list(input$expr1, input$expr2, input$expr3, input$expr4, input$expr5,
                            input$expr6, input$expr7, input$expr8, input$expr9, input$expr10)
           
           for (i in 1:length(input$calVarlist)){
             # substitute variable for x
             x <- dat[,input$calVarlist[i]]
             newvar <- eval(parse(text=unlist(exprlist[[i]])))
             # insert into dataframe
             dat[, last + i] <- newvar
           } 
           last2 <- ncol(dat)
           newVarnames <- names(dat)[(last+1):last2]  
           
           fit1 <- ah.2ph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ", paste(input$covariates,collapse="+"))),
                          data=dat, robust=input$robust, R=R, Pi=Pi, calibration.variables = c(newVarnames, input$calvars))
           
         } # end else
      }
      
      terms <- c()
      for (i in 1:length(unlist(input$covariates))){
        terms[i] <- paste0("+ \\beta_", i, unlist(input$covariates[i]))
      }
      if (input$modeltype == 0){
        terms[1] <- paste0("\\beta_", 1, unlist(input$covariates[1]))
      }
    } 
    
      output$modelEq <- renderUI({
        cond <- paste(input$covariates, collapse = ", ")
            tab1txt <- "Table 1. Parameter estimates from "
            if (input$modeltype == 1){
              out <- paste(c("$$\\lambda(t|", cond, ") = \\lambda_0(t)", terms, "$$", tab1txt, "single-phase additive hazards model."), collapse = "")
            } else if (input$modeltype == 2){
              out <- paste(c("$$\\lambda(t|", cond, ") = \\lambda_0(t)", terms, "$$", tab1txt, "two-phase additive hazards model."), collapse = "")
            } else if (input$modeltype == 3){
              out <- paste(c("$$\\lambda(t|", cond, ") = \\lambda_0(t)", terms, "$$", tab1txt, "two-phase additive hazards model with calibration."), collapse = "")
            } else if (input$modeltype == 0){
              out <- paste(c("$$\\lambda(t|", cond, ") = \\lambda_0(t)\\exp(", terms, ")$$", tab1txt, "proportional hazards model."), collapse = "")
            } 
            return(withMathJax(h5(tags$b(helpText(out)))))
      })
      
      output$regTab <- renderTable({
        # headings: coef	 se	 lower.95	 upper.95	 z	 p.value
        regTab <- summary(fit1)$coef
        if (input$robust == T & input$modeltype > 0){
          colnames(regTab)[2] <- "robust.se"
        }
        return(regTab)
      }, digits = 4)
      
      output$modFit <- renderText("Model fitted. Please see next page for results.")
    
      output$modelCovariates <- renderUI({
        if (input$modeltype > 0){
        ncov <- length(unlist(input$covariates))
        # define variable ranges
        lapply(1:ncov, function(i){
          list(tags$b(input$covariates[i]), 
               numericInput(paste0("cov", i), label=NA, value=NA))
          }
        )
        }
      })
      
      output$plotPredHaz <- renderPlot({
        if (input$modeltype>0){
          
        ncov <- length(unlist(input$covariates))
        
        varlist <- c(input$cov1, input$cov2, input$cov3, input$cov4, input$cov5,
                     input$cov6, input$cov7, input$cov8, input$cov9, input$cov10)[1:ncov]
        newdata <- data.frame(t(varlist))
        names(newdata) <- unlist(input$covariates)
        
        vartab <- cbind(names(newdata), c(newdata[1,]))
        vars <- paste(vartab[,1], "=", vartab[,2], sep='')
        vars <- paste(vars, collapse = ", ")
       
        #predtab <- predict(fit1, newdata, newtime = seq(0.1, 20, by=0.1))
        predtab <- predict(fit1, newdata, newtime = 1:max(dat[, unlist(input$surv)], na.rm=T))
        if (!("time" %in% names(predtab))){
          predtab$time <- 1:max(dat[, unlist(input$surv)])  # change to rounded value if seq()
        }
        
        predtab$CI_l <- with(predtab, L - qnorm(0.975)*L.se)
        predtab$CI_u <- with(predtab, L + qnorm(0.975)*L.se)
        
        maxY <- ifelse(input$predHazCI == F, max(predtab$L, na.rm=T), max(predtab$CI_u, na.rm=T))*1.1
        
        plot(predtab$time, predtab$L, type='l', xlab="Time", ylab="Predicted Hazard Rate",
             main=paste("Predicted Hazards Over Time \n", vars, sep=''),
             ylim=c(0, maxY))
        
        if (input$predHazCI == T){
          lines(predtab[,"time"], predtab[,"CI_l"], type='l', lty=2)
          lines(predtab[,"time"], predtab[,"CI_u"], type='l', lty=2)
        }
        
        if (!is.na(input$haztime)){
          abline(v=input$haztime, col="red", lty=3)
          #points(input$haztime, predtab[which.min(abs(predtab[,1] - input$haztime)), 2])
        }
        } else {
          return(NULL)
        }
      })
      
  }) # end observeEvent
  
  #-----------
  # figures
  #-----------
  
   output$hist <- renderPlot({
      inFile <- input$file1
      dat <- data() 
      
      if (input$histvar != ""){
        if (is.na(input$nbreaks)){
          hist(dat[, unlist(input$histvar)], main=paste("Histogram of", paste(input$histvar)), xlab=paste(input$histvar), 
          probability = input$histprob,
          col=rgb(0.7, 0, 0.1, alpha=0.2))

        } else {
          hist(dat[, unlist(input$histvar)], main=paste("Histogram of", paste(input$histvar)), xlab=paste(input$histvar), 
           probability = input$histprob, breaks = input$nbreaks,
           col=rgb(0.7, 0, 0.1, alpha=0.2))
        }
      } else {
        return(NULL)
      }
   }) 
   
   output$KM <- renderPlot({
     inFile <- input$file1
     dat <- data() 
     
     if (input$surv0 != ""){
        if (input$KMbygrp == F){
          KMfit <- survfit(as.formula(paste("Surv(", input$surv0, ",", input$cen0, ") ~ 1")), data=dat)
          if (input$KMcuminc == F){
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
       
          if (input$KMcuminc == F){
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
     } else {
       return(NULL)
     }
   })
   
   output$scatter <- renderPlot({
     inFile <- input$file1
     dat <- data() 
     
     if (input$scatterX != "" & input$scatterY != ""){
       scatter.smooth(dat[,input$scatterX], dat[,input$scatterY], 
                      lpars = list(col = "red", lwd = 2, lty = 1),
                      xlab = input$scatterX, ylab = input$scatterY,
                      main = "Scatter plot with loess smooth")
     } else {
       return(NULL)
     }
   }) 
  
}) ## end document