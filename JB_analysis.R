library(tidyverse)
library(leaflet)
crab_data <- read_csv("data/crab_data_clean.csv")
m <- leaflet(crab_data) %>% 
  addTiles() %>% fitBounds(-63, 45.5, -64, 50) %>% 
  addCircleMarkers(~lon, ~lat, radius = 1, color = ~total_crabs)
m
