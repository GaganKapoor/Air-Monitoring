library(RColorBrewer)
library(scales)
library(lattice)
library(shiny)
library(shinyalert)
library(tidyverse)
library(lubridate)
library(shinyTime)
library(openair)
library(DT)
library(rsconnect)
library(shinydashboard)



source("request.r")
source("plots.r")

Sys.setenv(TZ='America/Edmonton')
stations <- read.csv("data/stations.csv")


function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
#   shinyalert("Terms Of Use", "
# The fish and wildlife inventory data accessible from this site does not represent a complete record of all fish and wildlife collections and observations available. It only represents the data currently residing in the Fish & Wildlife Management Information System (FWMIS). Many areas in Alberta have not been subjected to a comprehensive species inventory. Information accessible through this site is not intended to be a definitive statement on the presence, absence, or status of a species within a given area, nor as a substitute for on-site surveys.
# 
# While every effort has been made to ensure that the information accessible from this site is complete, accurate, and current, the Government of Alberta, their agents and employees are not liable for any loss or damage arising directly or indirectly from the possession, publication, or use of, or reliance on, that information. This information is provided as is without expressed or implied warranty.
# 
# FWMIS data is provided to individuals and organizations for use related to appropriate conservation and management of Alberta's fish and wildlife, including the review of development proposals. Redistribution of the data is prohibited.
# 
# 
# Please click on the continue button to consent to the content of this page and proceed to Fish and Wildlife site.", confirmButtonText = "Agree & Continue")
  
  observe({
    directions <- stations %>%
      filter(location == input$station) %>%
      filter(grepl(input$resolution, resolution)) %>%
      filter(substr(measure, 1, 2) == "WD") %>%
      .$measure
    updateSelectInput(session, "wd", choices = directions)
  })
  
  observe({
    speeds <- stations %>%
      filter(location == input$station) %>%
      filter(grepl(input$resolution, resolution)) %>%
      filter(substr(measure, 1, 2) == "WS") %>%
      .$measure
    updateSelectInput(session, "ws", choices = speeds)
  })
  
  observe({
    pollutants <- stations %>%
      filter(location == input$station) %>%
      filter(grepl(input$resolution, resolution)) %>%
      filter(grepl('ppb|ppm|ug\\/m3', measure)) %>%
      .$measure
    
    updateSelectizeInput(session, "pollutant", choices = pollutants)
    updateSelectizeInput(session, "pollutants", choices = pollutants)
    #updateSelectizeInput(session, "pollutants_corr", choices = pollutants)
  })
  
  timeWindow <- reactive({
    d <- input$date
    center <- update(input$time, year=year(d), month=month(d), day=day(d))
    
    list(
      start = center - hours(input$minus),
      center = center,
      end = center + hours(input$plus)
    )
  })
  
  requestDataFrame <- reactive({
    window <- timeWindow()
    
    selected <- stations %>%
      filter(location == input$station) %>%
      filter(grepl(input$resolution, resolution)) %>%
      select(number, measure)
    
    response <- wbea_request(selected$number, window$start, window$end)
    df <- parse(response)[[1]]
    
    names(df) <- stations$measure[match(names(df), stations$number)]
    names(df)[1] <- "date"
    
    df
  })
  
  selectedPollutants <- reactive({
     if(input$tabs == "Pollutant Roses") {
       pollutants <- c(input$pollutant)
     }
    # else if (input$nav == "Correlation Plot"){
    #   pollutants <- input$pollutants_corr
    #  }
    else {
      pollutants <- input$pollutants
    }
    pollutants
  })
  
  selectedData <- reactive({
    selected <- c("date", selectedPollutants(), input$ws, input$wd)
    
    filtered <- requestDataFrame() %>%
      select(all_of(selected))
    
    names(filtered)[names(filtered) == input$ws] <- 'ws'
    names(filtered)[names(filtered) == input$wd] <- 'wd'
    
    filtered
  })
  
  output$table <- renderDT(server= FALSE,{ 
    df <- selectedData()
    df$date <- format(df$date, "%Y-%m-%d %H:%M")
    df$wd <- round(df$wd, 2)
    df$ws <- round(df$ws, 2)
    names(df)[which(names(df) == "date")] <- "Timestamp"
    names(df)[which(names(df) == "wd")] <- "Wind Direction (Deg)"
    names(df)[which(names(df) == "ws")] <- "Wind Speed (km/h)"
    
    datatable(df, extensions = 'Buttons', options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel')))
  })
  
  output$timePlot <- renderPlot({
    validate(
      need(length(input$pollutants) >= 1, "Please select at least one pollutant.")
    )
    
    df <- selectedData()
    
    timep(df, pollutants = input$pollutants, window = timeWindow())
  })
  
  output$corrPlot <- renderPlot({
    validate(
      need(length(input$pollutants) >= 2, "Please select at least two pollutants.")
    )
    
    df <- selectedData()
    
    validate(
      need(sum(sapply(df[input$pollutants], function(x) !all(is.na(x)))) >= 2,
           "Please select at least two pollutants with non-missing data, see data table for details.")
    )
    
    corrp(df, pollutants = input$pollutants)
  })
  
  output$prosePlot <- renderPlot({
    validate(
      need((input$plus +input$minus) <= 24, "Please select a time window 24 hours or less.")
    )
    
    df <- selectedData()
    
     validate(
       need(!all(is.na(df[[input$pollutant]])), "All pollutant data is missing, see data table for details.")
     )
     
     prosep(df, pollutant = input$pollutant)
   })
   
  output$windPlot <- renderPlot({
    validate(
      need((input$plus +input$minus) <= 24, "Please select a time window 24 hours or less.")
    )
    
    df <- selectedData()
    
    windp(df)
  })

  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      df = selectedData()
      pollutant_p = input$pollutant
      params <- list(df= df, pollutant = pollutant_p, pollutants = input$pollutants, window = timeWindow())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
      
    }
  )
  
  
}