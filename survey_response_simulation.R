#### Preamble ####
# Purpose: This script simulates the responses to a survey about restaurants in Toronto.
# Author: Yingying Zhou
# Data: 14 February 2021
# Contact: yingying.zhou@utoronto.ca
# License: MIT
# Pre-requisites: 
# - None


#### Workspace setup ####
library(tidyverse)


#### Simulate questions ####
# Q1: What is the first three digits of your postal code?
# Q2: Which municipality is your restaurant located in?
# Q3: What is the type of your restaurant?
# Q4: Is your restaurant a franchise?
# Q5: How long has your restaurant been open (in years)?

# Do this one for treated and once for control and then bring them together

set.seed(116)
number_of_observations_treated <- 1637
simulated_dataset_treated <- 
  tibble(
    type = rep("Treated", number_of_observations_treated),
    Q2 = sample(x = c("Toronto", "Durham", "York", "Peel", "Halton"),
                size = number_of_observations_treated,
                replace = TRUE,
                prob = c(0.295, 0.128, 0.219, 0.246, 0.11)),
    
    Q3 = sample(x = c("Fast Food", "Fast Casual", "Casual Dining", "Premium Casual", "Family Style", "Fine Dining"),
                size = number_of_observations_treated,
                replace = TRUE,
                prob = c(0.11, 0.19, 0.27, 0.13, 0.21, 0.09)),
    
    Q4 = sample(x = c("Franchise", "No"),
                size = number_of_observations_treated,
                replace = TRUE,
                prob = c(0.35, 0.65)),
    
    Q5 = rnorm(n = number_of_observations_treated, mean = 9, sd = 7) %>% round(digits = 0) %>% abs() # round years to nearest integer and non-negative
    
  )  


number_of_observations_control <- 1637
simulated_dataset_control <- 
  tibble(
    type = rep("Control", number_of_observations_control),
    Q2 = sample(x = c("Toronto", "Durham", "York", "Peel", "Halton"),
                size = number_of_observations_control,
                replace = TRUE,
                prob = c(0.295, 0.128, 0.219, 0.246, 0.11)),
    
    Q3 = sample(x = c("Fast Food", "Fast Casual", "Casual Dining", "Premium Casual", "Family Style", "Fine Dining"),
                size = number_of_observations_control,
                replace = TRUE,
                prob = c(0.11, 0.19, 0.27, 0.13, 0.21, 0.09)),
    
    Q4 = sample(x = c("Franchise", "No"),
                size = number_of_observations_control,
                replace = TRUE,
                prob = c(0.35, 0.65)),
    
    Q5 = rnorm(n = number_of_observations_control, mean = 9, sd = 7) %>% round(digits = 0) %>% abs() # round years to nearest integer and non-negative
    
  )  

simulated_dataset <- 
  rbind(simulated_dataset_control, simulated_dataset_treated)


#### Save and clean-up
write_csv(simulated_dataset, 'inputs/simulated_data.csv')






#### Make some graphs very quickly

simulated_dataset %>% 
  ggplot(aes(x = Q2)) +
  geom_bar(stat="count") +
  labs(x = "Toronto regions",
       y = "Number of restaurants") +
  theme_minimal() +
  facet_wrap(vars(type))



simulated_dataset %>% 
  ggplot(aes(x = Q3)) +
  geom_bar(stat="count") +
  theme_minimal() +
  labs(x = "Restaurant type",
       y = "Number of restaurants") +
  scale_fill_brewer(palette = "Set1")


simulated_dataset %>% 
  ggplot(aes(x = Q5)) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Years in operation",
       y = "Number of restaurants") +
  scale_color_brewer(palette = "Set1")