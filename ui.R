library(leaflet)
library(DT)
library(shinyBS)
library(shinyalert)
library(shinydashboard)

 # import dependencies

shinyUI(dashboardPage(
  # useShinyalert(),

  dashboardHeader(title = "WBEA Openair Data Explorer"),
  dashboardSidebar(
    dateInput("date", "Date:", value=(today(Sys.timezone()) - 1)),
    selectInput("resolution", "Resolution:", choices = c("5min", "1hr")),
    
    column(12, align = "left", timeInput("time", "Time (hh:mm)", value = strptime("12:00:00", "%T"), seconds = FALSE)),
    column(6, align = "left", numericInput("minus", "- Hours", value = 6, min = 1)),
    column(6, align = "left", numericInput("plus", "+ Hours", value = 6, min = 1)),
    selectInput("station", "Station:", choices = unique(stations$location), selected = stations[1, "location"] ),
    conditionalPanel(condition = "input.tabs == 'Pollutant Roses'",
                     selectInput("pollutant", "Pollutant:", choices="", multiple = FALSE)),
    conditionalPanel(condition = "input.tabs != 'Pollutant Roses'",
                     selectInput("pollutants", "Pollutants:", choices="", multiple = TRUE)),
    selectInput("ws", "Wind Speed:", choices = ""),
    selectInput("wd", "Wind Direction:", choices = "")
  ),
  
dashboardBody( tabBox(type = "tabs", id = "tabs", width = 12,
                               tabPanel("Time Plot", plotOutput("timePlot", height="800px", width = '100%')),
                               tabPanel("Pollutant Roses", plotOutput("prosePlot", height="800px",  width = '100%')),
                               tabPanel("Corr Plot", plotOutput("corrPlot", height="800px",  width = '100%')),
                               tabPanel("Wind Roses", plotOutput("windPlot", height="800px",  width = '100%')),
                               tabPanel("Report", DTOutput("table"), downloadButton("report", "Generate report"))
))

))
           
           
           
           