library(ggplot2)
dat <- readRDS("data/crab_data_clean.rds")
ggplot(dat, aes(x = total_crabs))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(title = "Histogram of total snow crabs", x = "total crabs")
ggplot(dat, aes(x = male))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(title = "Histogram of male snow crabs", x = "male crabs")
ggplot(dat, aes(x = female))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(title = "Histogram of female snow crabs", x = "female crabs")
ggplot(dat, aes(x = female_percentage))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(title = "Histogram of proportion of female crabs", 
       x = "proportion female crabs")
ggplot(dat, aes(x = Simpson_index))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(title = "Histogram of Simpson's diversity index", 
       x = "Simpson's diversity index")
