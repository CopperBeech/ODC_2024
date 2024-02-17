library(shiny)
library(tidyverse)
library(leaflet)
library(raster)

ui <- fluidPage(
  
  titlePanel("Snow Crab Trawl Survey Data"),
  fluidRow(
    column(3, offset = 0.5,
           selectInput("year",
                       "Choose year",
                       list("All years",
                            2019, 2020, 2021, 2022)
           )
    ),
    column(4,
           selectInput("variable",
                       "Choose variable to plot:",
                       list("Total snow crabs",
                            "Male snow crabs",
                            "Female snow crabs",
                            "Proportion of female snow crabs",
                            "Simpson diversity index")
           )
    ),
    column(4,
           selectInput("pred",
                       "Show prediction:",
                       list("None",
                            "Total snow crabs",
                            "Proportion of female snow crabs")
                       )
           )
  ),
  
  fluidRow(
    column(5, offset = 0.5,
           plotOutput("distPlot")
    ),
    column(7,
           leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  
  crab_data <- readRDS("crab_data_clean.rds") %>% 
    rename("Total snow crabs" = "total_crabs",
           "Male snow crabs" = "male",
           "Female snow crabs" = "female",
           "Proportion of female snow crabs" = "female_percentage",
           "Simpson diversity index" = "Simpson_index") %>% 
    mutate(across(21:22, round, 3))
  pred_fem <- raster("PreFemaleAll_01.tif")
  pred_count <- raster("PreCountAll_01.tif")
  
  app_data <- reactive({
    if(input$year != "All years"){
      crab_data <- filter(crab_data, year == input$year)
    } else crab_data <- crab_data
    crab_data %>% 
      dplyr::select(lon, lat, input$variable)
  })
  
  scale_dat <- reactive({
    scale_dat <- req(app_data())
    if(input$variable %in% c("Total snow crabs",
                             "Male snow crabs",
                             "Female snow crabs")){
      scale_dat <- scale_dat %>%
        filter(.[[3]] < quantile(.[[3]], 0.999, na.rm = TRUE))
    } else{
      scale_dat <- scale_dat
    }
  })
  
  
  colorpal <- reactive({
    col_dat <- req(scale_dat())
    colorpal <- colorNumeric("magma", domain = col_dat[,3], reverse = TRUE)
  })
  
  output$distPlot <- renderPlot({
    plot_data <- req(app_data())
    var <- as.character(input$variable)
    colnames(plot_data)[3] <- var
    ggplot(plot_data, aes(x = .data[[var]]))+
      geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
      geom_density(alpha = 0.2, fill = "#FF6666") +
      ggtitle(paste("Histogram of", var))
  })
  
  output$map <- renderLeaflet({
    leaflet(crab_data) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  observe({
    pal <- colorpal()
    map_dat <- req(app_data())
    colnames(map_dat) <- c("lon", "lat", "chosen_var")
    leg_dat <- req(scale_dat())
    colnames(leg_dat) <- c("lon", "lat", "chosen_var_leg")
    lab <- paste0(input$variable, ": ", map_dat$chosen_var, 
                  "; (", map_dat$lat, ", ", map_dat$lon, ")")
    proxy <- leafletProxy("map", data = map_dat)
    if(input$pred == "None"){
      proxy %>%
        clearImages() %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addCircleMarkers(~lon, ~lat, radius = 1,
                         color = ~pal(chosen_var), label = lab) %>%
        addLegend("topright", pal = pal, leg_dat$chosen_var_leg,
                  title = input$variable)
    } else if(input$pred == "Total snow crabs") {
      pal1 <- colorNumeric("Spectral", domain = values(pred_count), 
                           na.color = "transparent")
      proxy %>% 
        clearImages() %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addRasterImage(pred_count, colors = pal1) %>% 
        addLegend(position = "topright", pal = pal1, values = values(pred_count),
                  title = "Predicted total count") 
    } else if(input$pred == "Proportion of female snow crabs") {
      pal2 <- colorNumeric("Spectral", domain = values(pred_fem), 
                           na.color = "transparent")
      proxy %>% 
        clearImages() %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        addRasterImage(pred_fem, colors = pal2) %>% 
        addLegend(position = "topright", pal = pal2, values = values(pred_fem),
                  title = "Predicted female proportion")
    }
  })

}

shinyApp(ui = ui, server = server)
