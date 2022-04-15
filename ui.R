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




valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}
























ui <- fluidPage(
  includeCSS("styles.css"),
  
  navbarPage("CalMedForce Impact Summary",
  
  tabPanel("Impact Summary",
           tabsetPanel(type = "tabs"),
      
           mainPanel(
             tabsetPanel(type = "tabs",
               tabPanel("Impact Summary Cohort 1 through Cohort 4", leafletOutput("impact_summary", height = 850)),
               tabPanel("Resident Impact", leafletOutput("resident_summary", height = 850))),
                    ),
             valueBox(value = "programs",
                      subtitle = "# of programs",
                      icon = "tachometer",
                      color = "green")
                  ),
  
  tabPanel("Graduates"),
  
  tabPanel("Awardees")
  )
)

server <- function(input, output) {
  
  #programs awarded value box
  
  data <- read.csv("All Cycles.csv", stringsAsFactors = FALSE)
  dt <- subset(data, Awarded.Yes.No == "Yes")
  rm(data)
  
  dt <- subset(dt, Info != "Delete")
  
  x <- length(dt$Program.Name)

output$programs <- renderValueBox({
  
  total_P <- x
  
  valueBox(total_P,
           caption = "<b><p style='font-family:montserrat'># of Programs Funded</p></b>",
           icon = "fa-hospital-o",
           color = "#10C637")
  }
)    

  
  impact_summary$text3 <- sprintf(
    "<p style='font-size: 15px; font-family: montserrat'>%s</p>",
    impact_summary$x) %>% 
    lapply(htmltools::HTML)
  
  impact_summary$labeltext <- sprintf(impact_summary$label1) %>% 
    lapply(htmltools::HTML)
  
  pal <- colorBin("Blues", impact_summary$n, bins = 5, na.color = "white")
  palNA <- colorBin("Blues", impact_summary$n, bins = 5, na.color = NA)
  
  output$impact_summary <- renderLeaflet({
    leaflet(impact_summary, options = leafletOptions(zoomControl = FALSE)) %>%
      addPolygons(fillColor = pal(impact_summary$n),
                  popup = impact_summary$text3,
                  popupOptions = popupOptions(maxWidth = 500),
                  label = impact_summary$labeltext,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px",
                                                           "font-family" = "Montserrat"),
                                              textsize = "18px",
                                              direction = "auto",
                                              interactive = TRUE),
                  color = "#444444",
                  smoothFactor = .5,
                  weight = 1,
                  opacity = 1.0,
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(weight = 5,
                                                      color = "#666",
                                                      fillOpacity = 0,7,
                                                      bringToFront = TRUE)) %>% 
      addLegend(title = "Number of Awardees by County", pal = palNA, values = ~impact_summary$n, opacity = 1)
  })
  
  #resident map
  output$resident_summary <- renderLeaflet({
    
    residents$x <- sprintf(
      "<p style='font-size: 15px; font-family: montserrat'>%s</p>",
      residents$x) %>% 
      lapply(htmltools::HTML)
    
    residents$residenttext <- sprintf(residents$labeltext) %>% 
      lapply(htmltools::HTML)

    pal <- colorBin("Greens", residents$positions, na.color = "white")
    palNA <- colorBin("Greens", residents$positions, na.color = NA)

    leaflet(residents, options = leafletOptions(zoomControl = FALSE)) %>% 
      addPolygons(fillColor = pal(residents$positions),
                  popup = residents$x,
                  popupOptions = popupOptions(maxWidth = 500),
                  label = residents$residenttext,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px",
                                                           "font-family" = "Montserrat"),
                                              textsize = "18px",
                                              direction = "auto",
                                              interactive = TRUE),
                  color = "#444444",
                  smoothFactor = .5,
                  weight = 1,
                  opacity = 1.0,
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(weight = 5,
                                                      color = "#666",
                                                      fillOpacity = 0,7,
                                                      bringToFront = TRUE)) %>% 
      addLegend(title = "Number of Residents Funded", pal = palNA, values = ~residents$positions, opacity = 1)
  })
  
  
}


shinyApp(ui = ui, server = server)
