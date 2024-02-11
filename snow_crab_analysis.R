library(tidyverse)
library(vegan)
crab_data <- read_csv("data/snow_crab_survey.csv")
# Remove the french from the column names, to make subsetting easier.
col_names_english <- as_vector(map(strsplit(colnames(crab_data), "__"), 1))
colnames(crab_data) <- col_names_english
# Calculate sex ratio, which is NA whenever there are 0 male or female crabs.
# sex_ratio_func <- function(m, f){
#   ratio <- m/f
#   ratio[ratio %in% c(NaN, Inf, 0)] <- NA
#   return(ratio)
# }
# male_pct_function <- function(m, f){
#   total = m+f
#   m_pct <- m/total
#   return(m_pct)
# }
# Simpson diversity index function (might want to replace with diversity from vegan)
# simpson_index_func <- function(species_mat){
#   species_mat <- as.matrix(species_mat)
#   total_catch <- rowSums(species_mat)
#   total_catch[total_catch == 0] <- NA
#   prop_sq <- matrix(nrow = nrow(species_mat), ncol = ncol(species_mat))
#   for(i in 1:ncol(species_mat)){
#     prop_sq[,i] <- (species_mat[,i]/total_catch)^2
#   }
#   lambda <- rowSums(prop_sq)
#   return(lambda)
# }


crab_data <- crab_data %>% 
  mutate(total_crabs = snow_crab_males+snow_crab_females,
         female_percentage = snow_crab_females/total_crabs) %>% 
  relocate(total_crabs, .after = snow_crab_females) %>% 
  mutate(Simpson_index = diversity(crab_data[, 11:19], index = "simpson"))
# Make a few plots
ggplot(crab_data) +
  geom_histogram(aes(x=female_percentage))+
  labs(title = "Snow Crab Female Percentage")
ggplot(crab_data) +
  geom_histogram(aes(x=Simpson_index))+
  labs(title = "Simpson Index")
