library(tidyverse)
library(vegan)
crab_data <- read_csv("data/snow_crab_survey.csv")
# Remove the french from the column names, to make subsetting easier.
col_names_english <- as_vector(map(strsplit(colnames(crab_data), "__"), 1))
colnames(crab_data) <- col_names_english

crab_data <- crab_data %>% 
  rename(lat = latitude, lon = longitude,
         male = snow_crab_males,
         female = snow_crab_females) %>% 
  mutate(year = as.factor(year),
         month = as.factor(month),
         .keep = "unused") %>% 
  mutate(total_crabs = male+female,
         female_percentage = female/total_crabs) %>% 
  relocate(total_crabs, .after = female)
crab_data <- crab_data %>% 
  mutate(Simpson_index = diversity(crab_data[, 11:20], index = "simpson"))

write_csv(crab_data, "crab_data_clean.csv")
write_rds(crab_data, "crab_data_clean.rds")
# Make a few plots
ggplot(crab_data) +
  geom_histogram(aes(x=female_percentage))+
  labs(title = "Snow Crab Female Percentage")
ggplot(crab_data) +
  geom_histogram(aes(x=Simpson_index))+
  labs(title = "Simpson Index")
