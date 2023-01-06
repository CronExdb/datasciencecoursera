library(shiny)
library(plotly)
library(leaflet)
data(iris)
Spain <- read.csv("es.csv")


ui <- fluidPage(
  titlePanel("Shiny Application"),
  fluidRow(
    column(6,
      plotlyOutput("graph", width = "100%")),
    column(6,  
  leafletOutput("map", width = "100%"))
),
  
  fluidRow(
    column(3,
           checkboxGroupInput("chkbox", label = "Checkbox", 
                              choices = list("Length" = 1, "Width" = 2, "Species" = 3),
                              selected = 1)
          
    ),
    column(4, offset = 1,
           selectInput("choice", "Choose", choices = names(iris), selected = NULL)
    ),
    column(4,
           radioButtons("period", "Choose a period:", 
                        c("Button1"="",
                          "Button2" = "",
                          "Button3" = "",
                          "Button4" = ""),
                        selected = "all")
    )
  ),

  
  
  
  

)





server <- function(input, output, session){

  
  output$graph <- renderPlotly({
    plot_ly(iris, x = ~get(input$choice), y = ~Sepal.Length, z = 1 + rnorm(150),  type = 'scatter3d', mode = 'markers', marker = list(color = ~Sepal.Length, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
  })
  output$map <- renderLeaflet({
    Spain <- read.csv("es.csv")
    leaflet() %>%  addTiles() %>%
      addMarkers(lat=Spain$lat, lng=Spain$lng,popup = paste
                 ("City: ",
                   (Spain$city),
                   "<br>Population: ",
                   (Spain$population)
                 ) , clusterOptions = markerClusterOptions)
  })
  

  
  
  
  
}

shinyApp(ui, server)
