# increase max upload size to 30mb
options(shiny.maxRequestSize=50*1024^2)

lapply(dir("fun", full.names = TRUE), source)
lapply(dir("R", full.names = TRUE), source)

library(gtfstools)
library(shiny)
library(leaflet)
library(leafgl)
library(shinydashboard)
library(data.table)
library(waiter)
library(highcharter)
library(htmltools)
library(shinyWidgets)
# library(shinyjs)
# library(bsplus)
library(shinyBS)
# library(shinydisconnect)
# library(sf)
# library(tippy)
# library(mapdeck)

# mapdeck::set_token(fread("../../data/mapbox_key.csv")$key)

# install.packages('mapdeck')

# install.packages(c('gtfstools',
#                    'shiny',
#                    'leaflet',
#                    'leafgl',
#                    'shinydashboard',
#                    'data.table',
#                    'waiter',
#                    'highcharter',
#                    'htmltools',
#                    'shinyWidgets',
#                    'sf',
#                    'shinyBS'))