# Version: 20190327

# Stimulate the revenue when flight are overbooked

# Setup -----------

library(tidyverse)


# Simulation------------

# parameters
# capaciity of flight
seat_cap <- 100


# prob of showing up
prob_showup <- 0.94


# revenue per customer
revenue_per_seat <- 700


# compensation cost per oversold tickets
comp_per_seat <- revenue_per_seat * 2


# function of counting show up customers
generate.showup <- function(oversold, prob_showup) {
  
  # generate list of showup
  showup <- 
    sample(c(TRUE, FALSE),
         seat_cap + oversold,
         replace = TRUE,
         prob = c(prob_showup, 1-prob_showup))
  
  # count number of show up
  sum(showup)
  
}


# build the data frame for capturing the result
result <- data.frame(
  Oversold = NA,
  Profit = NA
)


# max of tickets oversold
oversold_max <- 15


# no of simulation
sim <- 1000
 
 
# simulate the profit by running each scenario for 1000 times
for (oversold in 0:oversold_max) {
  
  for (i in 1:sim) {
    
    # random generate number of showing up
    showup_rand <- generate.showup(oversold, prob_showup)
    
    # revenue
    total_revenue = showup_rand * revenue_per_seat
    
    
    # compensation cost if more than cap are showing up
    comp_total <- ifelse(showup_rand >= seat_cap,
                         (showup_rand - seat_cap) * comp_per_seat,
                         0)
    
    # profit
    profit <- total_revenue - comp_total
    
    
    #add to result
    result <<- rbind(result, c(oversold, profit))
    
  }
}


# visualize the best oversold tickets
ggplot(
  result %>%
    filter(!is.na(Oversold)),
  aes(x = factor(Oversold), y = Profit, fill = between(Oversold, 5, 8))
) +
  geom_boxplot()+
  labs(title = "Possible Total Revenue for Tickets Oversold",
       x = "No of Tickets Oversold",
       y = "Profit after Compensation") +
  theme(legend.position = "none")


# End ---------------------

