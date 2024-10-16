library(shiny)
library(ggplot2)
library(dplyr)

MiddleEarth <- read.csv("MapOfMiddleEarth.csv")
PathToMordor <- read.csv("PathToMordor.csv")

Map <- 
  ggplot(data = FullPlotData, 
         mapping = aes(x = x, 
                       y = y)) + 
  geom_tile(fill = FullPlotData$color_code) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 1200, by = 100)) + 
  scale_y_continuous(breaks = seq(0, 700, by = 100))

ui <- fluidPage(
  titlePanel("Walk Into Mordor"), 
  
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "num_miles", 
                   label = "Miles walked", 
                   value = 0), 
      actionButton(inputId = "btn_updateMap", 
                   label = "Update Map")
    ), 
    mainPanel(
      plotOutput("plt_middleEarth", 
                 height = "700px", 
                 width = "1200px")
    )
  )
)

server <- shinyServer(function(input, output, session){
  
  Plot <- reactiveValues(
    Map = Map
  )
  
  observeEvent(input$btn_updateMap, 
               {
                 user_dist <- input$num_miles
                 Complete <- filter(PathToMordor, 
                                    distance <= user_dist)
                 Plot$Map <- Map + geom_path(data = Complete,
                                             mapping = aes(x = x,
                                                           y = y),
                                             color = "red", 
                                             linewidth = 1)
               })
  
  output$plt_middleEarth <- 
    renderPlot(Plot$Map)
  
})


shinyApp(ui = ui, 
         server = server)  
