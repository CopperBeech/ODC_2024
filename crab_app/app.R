

library(shiny)
library(tidyverse)
library(leaflet)

# Define UI for application that draws histograms
ui <- fluidPage(

    # Application title
  titlePanel("Snow Crab Trawl Survey Data"),
  fluidRow(
    column(3, offset = 0.5,
           selectInput("year",
                       "Choose year",
                       list("All years",
                            2019, 2020, 2021, 2022)
                       )
           ),
    column(3,
           selectInput("variable",
                       "Choose variable to plot:",
                       list("Total snow crabs",
                            "Male snow crabs",
                            "Female snow crabs",
                            "Proportion of female snow crabs",
                            "Simpson diversity index")
             )
           )
    ),

        # Show a plot of the generated distribution
  fluidRow(
    column(5, offset = 0.5,
           plotOutput("distPlot")
           ),
    column(7,
           leafletOutput("map")
           )
    )
)

# Define server logic required to draw histograms
server <- function(input, output) {
  
  crab_data <- read_csv("crab_data_clean_for_app.csv") %>% 
    rename("Total snow crabs" = "total_crabs",
           "Male snow crabs" = "male",
           "Female snow crabs" = "female",
           "Proportion of female snow crabs" = "female_percentage",
           "Simpson diversity index" = "Simpson_index") %>% 
    mutate(across(21:22, round, 3))
  
  
  app_data <- reactive({
      if(input$year != "All years"){
        crab_data <- filter(crab_data, year == input$year)
      } else crab_data <- crab_data
      crab_data %>% 
        select(lon, lat, input$variable)
    })

  output$distPlot <- renderPlot({
    plot_data <- req(app_data())
    var <- as.character(input$variable)
    colnames(plot_data)[3] <- var
    ggplot(plot_data)+
      geom_histogram(aes(x = .data[[var]]))+
      ggtitle(paste("Histogram of", var))+
      theme_light()
    })
  
  output$map <- renderLeaflet({
    map_dat <- req(app_data())
    colnames(map_dat)[3] <- "chosen_var"
    if(input$variable %in% c("Total snow crabs",
                             "Male snow crabs",
                             "Female snow crabs")){
      scale_dat <- map_dat %>% 
        filter(chosen_var < quantile(chosen_var, 0.99, na.rm = TRUE))
    } else{
      scale_dat <- map_dat
    }
    pal <- colorNumeric("Purples", domain = scale_dat$chosen_var)
    lab <- paste0(input$variable, ": ", map_dat$chosen_var)
      m <- leaflet(map_dat) %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        addCircleMarkers(~lon, ~lat, radius = 1, 
                         color = ~pal(chosen_var), label = lab) %>% 
        addLegend("bottomleft", pal = pal, map_dat$chosen_var, 
                  title = input$variable)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
