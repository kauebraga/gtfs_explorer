# increase max upload size to 30mb
options(shiny.maxRequestSize=30*1024^2)

source(dir("fun", full.names = TRUE))

library(gtfstools)
library(shiny)
library(leaflet)
library(leafgl)
library(shinydashboard)
library(data.table)
library(waiter)
library(highcharter)
# library(kauetools)
library(htmltools)
library(shinyWidgets)