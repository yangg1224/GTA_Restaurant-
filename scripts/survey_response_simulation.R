#### Preamble ####
# Purpose: This script simulates the responses to a survey about restaurants in Toronto.
# Author: Yingying Zhou, Xinyi Xu
# Data: 14 February 2021
# Contact: yingying.zhou@utoronto.ca; xiny.xu@mail.utoronto.ca; 
# License: MIT
# Pre-requisites: 
# - None


#### Workspace setup ####
library(tidyverse)
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")


# Function that takes a region and generates a random FSA code within that region
# An FSA code is the first three letters of a postal code
fsa_generate <- function(region) {
  
  # Toronto FSAs
  if(region=="Toronto") {
    return(paste("M", sample(c(0:9), 1), sample(LETTERS, 1), sep = ""))
    
    # Durham FSAs
  } else if(region=="Durham") {
    m <- sample(c(1, 9, 0), 1)
    if (m==1){
      l <- sample(c('G','H','J','K','L','M','N','P','R','V','W','X','Y','S','T','Z','B'), 1)
    } else if(m==9){
      l <- sample(c('P', 'L'), 1)
    } else {
      l <- sample(c('B','E','C'), 1)
    }
    return(paste("L", m, l, sep = ""))
    
    # York FSAs
  } else if(region=="York") {
    m <- sample(c(0, 3, 4, 6, 7, 9), 1)
    if (m==0){
      l <- sample(c('H','E','G','J','N'), 1)
    } else if(m==3){
      l <- sample(c('P', 'R','S','X','Y','Z'), 1)
    } else if(m==4){
      l <- sample(c('A','B','C','E','S','G','H','L','P'), 1)
    } else if(m==6){
      l <- sample(c('A','B','C','E','G'), 1)
    } else if(m==7){
      l <- sample(c('B', 'E'), 1)
    } else {
      l = "N"
    }
    return(paste("L", m, l, sep = ""))
    
    # Peel FSAs 
  } else if(region=="Peel") {
    m <- sample(c(4, 5, 6, 7), 1)
    if (m==4){
      l <- sample(c('T','V','W','X','Y','Z'), 1)
    } else if(m==5){
      l <- sample(c('A','B','C','E','G','H','J','K','L','M','N','P','R','S','T','V','W'), 1)
    } else if(m==6){
      l <- sample(c('P','R','S','T','V','W','X','Y','Z'), 1)
    } else {
      l <- sample(c('A','C','K'), 1)
    }
    return(paste("L", m, l, sep = ""))
    
    # Halton FSAs
  } else if(region=="Halton") {
    m <- sample(c(0, 6, 7, 9), 1)
    if (m==0){
      l = "P"
    } else if(m==6){
      l <- sample(c('H','J','K','L','M'), 1)
    } else if(m==7){
      l <- sample(c('L','M','N','P','R','S','T'), 1)
    } else if(m==9){
      l <- sample(c('E', 'T'), 1)
    } else {
      l = "N"
    }
    return(paste("L", m, l, sep = ""))
    
    # Return NULL if unknown region is used as an argument
  } else{
    return(NULL)
  }
}


#### Simulate questions ####
# Q1: What is the first three digits of your postal code?
# Q2: Which municipality is your restaurant located in?
# Q3: What is the type of your restaurant?
# Q4: Is your restaurant a franchise?
# Q5: How long has your restaurant been open (in years)?
# Q6: Have you offered a takeout service in the past month?
# Q7: Have you offered a delivery service in the past month?
# Q8: Number of employees in the restaurant
# Q9: On average, how much do your restaurant employees earn per hour ($CAD)?
# Q10: Has your restaurant been a site of a potential COVID case?
# Q11: How has your FIXED costs to run the restaurant changed within this month?
# Q12: How has your FLEX/VARIABLE costs to run the restaurant changed within this month?
# Q13: How much revenue did your restaurant make in the past month? ($CAD)

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
    
    Q5 = rnorm(n = number_of_observations_treated, mean = 9, sd = 7) %>% round(digits = 0) %>% abs(), # round years to nearest integer and non-negative
    
    Q6 = sample(x=c("Yes","No"),
                size = number_of_observations_treated ,
                replace = TRUE,
                prob = c(0.87,0.13)),
    
    Q7 = sample(x=c("Yes","No"),
                size = number_of_observations_treated ,
                replace = TRUE,
                prob = c(0.41,0.59)),
    
    Q8 = sample(x=c("1-10","10-20","20-30",">30"),
                size = number_of_observations_treated ,
                replace = TRUE,
                prob = (c(0.35,0.47,0.12,0.06))
                ),
    
    Q9 = rnorm(n= number_of_observations_treated , mean=18.34, sd= 2.5) %>% round(digits = 2),
    
    Q10 = sample(x=c("Yes","No"),
                 size = number_of_observations_treated , 
                 replace = TRUE,
                 prob = c(0.1, 0.9)),
    

  )  
# Q11 Q12 Q13
#big restaurant (number of employees >20)
big_restaurant<-
  filter(simulated_dataset_treated, simulated_dataset_treated$Q8 == ">30" | simulated_dataset_treated$Q8 == "20-30") 

big_restaurant<- cbind(big_restaurant,
  tibble(
     Q11 = sample(sample(x=c("Increase","Decrease","No change"), 
                         size=nrow(big_restaurant),
                         replace=TRUE,
                         prob=c(0.62,0.01,0.37))),
     Q12 = sample(sample(x=c("Increase","Decrease","No change"), 
                         size=nrow(big_restaurant),
                         replace=TRUE,
                         prob=c(0.91,0.01,0.01))),
     Q13 = rnorm(n=nrow(big_restaurant) , mean=142754, sd=32486) %>% round(digits = 0)
    ))
#small restaurant (number of employees <20)
small_restaurant<-
  filter(simulated_dataset_treated, simulated_dataset_treated$Q8 != ">30" & simulated_dataset_treated$Q8 != "20-30") 

small_restaurant<- cbind(small_restaurant,
  tibble(
    Q11 = sample(sample(x=c("Increase","Decrease","No change"), 
                        size=nrow(small_restaurant),
                        replace=TRUE,
                        prob=c(0.52,0.02,0.46))),
    Q12 = sample(sample(x=c("Increase","Decrease","No change"), 
                        size=nrow(small_restaurant),
                        replace=TRUE,
                        prob=c(0.84,0.03,0.13))),
    Q13 = rnorm(n=nrow(small_restaurant) , mean=45673, sd=8435) %>% round(digits = 0)
  ))
## combine two sub group 
simulated_dataset_treated<- rbind(small_restaurant,big_restaurant) 




# Control group
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
    
    Q5 = rnorm(n = number_of_observations_control, mean = 9, sd = 7) %>% round(digits = 0) %>% abs() ,# round years to nearest integer and non-negative
    
    Q6 = sample(x=c("Yes","No"),
                size = number_of_observations_control ,
                replace = TRUE,
                prob = c(0.88,0.12)),
    
    Q7 = sample(x=c("Yes","No"),
                size = number_of_observations_control ,
                replace = TRUE,
                prob = c(0.47,0.53)),
    
    Q8 = sample(x=c("1-10","10-20","20-30",">30"),
                size = number_of_observations_control ,
                replace = TRUE,
                prob = c(0.51,0.45,0.035,0.005)),
    
    Q9 = rnorm(n= number_of_observations_control , mean=17.35, sd= 2.5) %>% round(digits = 2),
    
    Q10 = sample(x=c("Yes","No"),
                 size = number_of_observations_control , 
                 replace = TRUE,
                 prob = c(0.01, 0.99))
  )  

# Q11 Q12 Q13 control group
#big restaurant (number of employees >20)
big_restaurant_c<-
  filter(simulated_dataset_control, simulated_dataset_control$Q8 == ">30" | simulated_dataset_control$Q8 == "20-30") 

big_restaurant_c<- cbind(big_restaurant_c,
                       tibble(
                         Q11 = sample(sample(x=c("Increase","Decrease","No change"), 
                                             size=nrow(big_restaurant_c),
                                             replace=TRUE,
                                             prob=c(0.02,0.01,0.97))),
                         Q12 = sample(sample(x=c("Increase","Decrease","No change"), 
                                             size=nrow(big_restaurant_c),
                                             replace=TRUE,
                                             prob=c(0.02,0.01,0.97))),
                         Q13 = rnorm(n=nrow(big_restaurant_c) , mean=131264, sd=32486) %>% round(digits = 0)
                       ))
#small restaurant (number of employees <20)
small_restaurant_c<-
  filter(simulated_dataset_control, simulated_dataset_control$Q8 != ">30" & simulated_dataset_control$Q8 != "20-30") 

small_restaurant_c<- cbind(small_restaurant_c,
                         tibble(
                           Q11 = sample(sample(x=c("Increase","Decrease","No change"), 
                                               size=nrow(small_restaurant_c),
                                               replace=TRUE,
                                               prob=c(0.01,0.01,0.98))),
                           Q12 = sample(sample(x=c("Increase","Decrease","No change"), 
                                               size=nrow(small_restaurant_c),
                                               replace=TRUE,
                                               prob=c(0.01,0.02,0.97))),
                           Q13 = rnorm(n=nrow(small_restaurant_c) , mean=45673, sd=8435) %>% round(digits = 0)
                         ))
## combine two sub group 
simulated_dataset_control<- rbind(small_restaurant_c,big_restaurant_c) 






# Create the simulated dataset
simulated_dataset <-
  rbind(simulated_dataset_control, simulated_dataset_treated)

# Loop through all of the rows and generate an FSA code based on the row's region
for (i in 1:nrow(simulated_dataset)){
  simulated_dataset$Q1[i] = fsa_generate(simulated_dataset$Q2[i])
}

# Order Q1 before Q2 (not actually necessary)
simulated_dataset <- simulated_dataset[c(1, 14, 2:13)]



#### Save and clean-up
write_csv(simulated_dataset, 'inputs/simulated_data.csv')


#### Exploratory Data Analysis ####
#### Statistical Inference ####
revenue_c <- simulated_dataset_control$Q13
revenue_t <- simulated_dataset_treated$Q13
t.test(revenue_c, revenue_t)

#### Correlation Matrix ####
chart.Correlation(simulated_dataset[c(6, 10, 14)], histogram=TRUE)

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

simulated_dataset %>% 
  ggplot(aes(x = Q2, y = type, color=Q2)) +
  geom_jitter(show.legend = FALSE) +
  labs(title = "Experimental Conditions across Regions", x = "Region", y = "Experimental Condition") +
  theme_minimal()
