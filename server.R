library(shiny)
library(shinydashboard)
library(knitr)

shinyServer(function(input, output){
  
  datasetInput <- reactive({
    fail1 <- input$datamasuk
    if(is.null(fail1)) {return()}
    source("prepare_data.R")
    prepare(read.fwf(file=fail1$datapath, widths=c(1,2,2,3,2,2,2,2,2,2,1,1,2,1,1,2,2,1,2,2,2,2,2,2,2,2,2,2,4,4,1,1,2,1,4,3,1,1,2,1,4,3,1,2,1,4,3,1,2,1,4,3,1,4,2,2), sep=',', na.strings = "NA", fill = TRUE))
  })
  
  output$DI <- renderTable({
    if(is.null(datasetInput())) {return()}
    input$datamasuk
  })
  
  output$MD <- renderTable({
    if(is.null(datasetInput())) {return()}
    datasetInput()
  })
  
  output$SD <- renderTable({
    if(is.null(datasetInput())) {return()}
    summary(datasetInput())
  })
  
  output$J1 <- renderTable({
    if(is.null(datasetInput())) {return()}
    source("jadual1.R")
    jadual1(datasetInput())
  })
  
  output$J2 <- renderTable({
    if(is.null(datasetInput())) {return()}
    source("jadual1.R")
    source("jadual2.R")
    jadual2(jadual1(datasetInput()))
  })
  
  output$J3 <- renderTable({
    if(is.null(datasetInput())) {return()}
    source("jadual3.R")
    jadual3(datasetInput())
  })
  
  output$J4 <- renderTable({
    if(is.null(datasetInput())) {return()}
    source("jadual4.R")
    jadual4(datasetInput())
    
  })
  
  output$PT <- renderTable({
    if(is.null(datasetInput())) {return()}
    source("opsyentebangan.R")
    source("percentdiptb4.R")
    source("jadual3.R")
    j3 <- jadual3(datasetInput())
    pd <- percentdiptb4(datasetInput())
    opsyen(j3, pd)
  })
  
})