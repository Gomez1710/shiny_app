library(tigris)
library(dplyr)
library(viridis)
library(scales)
library(shiny)
library(knitr)
library(leaflet)
library(rsconnect)
library(flexdashboard)
library(htmltools)
library(plotly)
library(showtext)

source("1.Impact_summary.R", echo = TRUE, keep.source = TRUE)

source("2.Graduates.R", echo = TRUE, keep.source = TRUE)

source("3.All_awardees.R", echo = TRUE, keep.source = TRUE)

source("4.Disciplines.R", echo = TRUE, keep.source = TRUE)

shinyApp(ui = ui, server = server)
