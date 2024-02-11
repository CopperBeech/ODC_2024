

library(shiny)
library(tidyverse)

# Define UI for application that draws histograms
ui <- fluidPage(

    # Application title
    titlePanel("Snow Crab Trawl Survey Data"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("variable",
                        "Choose variable to plot:",
                        list("Total snow crabs" = "total_crabs",
                             "Male snow crabs" = "male",
                             "Female snow crabs" = "female",
                             "Proportion of female snow crabs" = "female_percentage",
                             "Simpson diversity index" = "Simpson_index")
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw histograms
server <- function(input, output) {
  crab_data <- read_csv("crab_data_clean_for_app.csv")
    output$distPlot <- renderPlot({
      ggplot(crab_data)+
        geom_histogram(aes_string(x=input$variable))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
