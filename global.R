library(tidyverse)
library(lubridate)
library(shiny)
library(shinyTime)
library(openair)
library(shinydashboard)



source("request.r")
source("plots.r")

Sys.setenv(TZ='America/Edmonton')
stations <- read.csv("data/stations.csv")