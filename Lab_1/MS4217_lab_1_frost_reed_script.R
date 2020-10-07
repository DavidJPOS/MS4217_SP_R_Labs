###############################################################################################
## Project: MS4217 Stochastic processes Labs and tutorials
## Script purpose: Simulate Reed Frost model
## Date: 30-9-2020
## Author: David JP O'Sullivan
###############################################################################################

rm(list = ls())

# we'll be making extensive use of this series of packages 
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

set.seed(12)

# How does the model work? [https://en.wikipedia.org/wiki/Reed%E2%80%93Frost_model]

# ex 1: simulate the model --------------------------------------------------------------------------

# code up the frost reed to simulate a single realisation of a contagions trajectory

z = 0.003 # what is the probability of infections?
steps <- 30 # How long are we going to run the processes for?

realisation_df <- tibble(time = 1:steps, sus = numeric(steps), inf = numeric(steps))
sus <- numeric(steps) # where we going to save the number of sus people
inf <- numeric(steps) # and infected people
realisation_df$sus[1] <- 497 # initial conditions
realisation_df$inf[1] <- 3


# ex 2: build mc simulation -------------------------------------------------------------------------

# create functions to do one simulation

# mc wrapper for what we want to do

# ex 3: simulate model, plot and comment -----------------------------------------------------

# First generate mc simulations from frost reed.
# plot and comment on a 5 realisations
# generate summary statistics, the mean number of inf and sus, sd and a 80% CI based on percentiles
# plot the average number of infections with confidence intervals
# plot number of sus and inf on a single plot
# create a panel plot of both of the previous plots using the packages cowplot package

# simulate Reed-Frost 500 times using the
# z = 0.003, steps = 30, sus_0 =497, inf_0 = 3

#you can save using ggsave()


# explore different z values ------------------------------------------------------------------

# refactor your function to allow for different parameters
# simulate the following range of z values, plot the average number of infections
#   z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)


# change the mc_reed_function to so it can take a list for parameters





# how define the varaibles
rf_par <- list(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3)
z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)

# var the value of z for each across MC simulations


# lock down simulation ------------------------------------------------------------------------

# say we plan on implementing a lockdown at time 5 where drop by if it decreases to z = 0.0025 
# again, refactor you code to allow for this

# examine the difference in the average number of infected individuals per time under both lockdown and non-lock conditions compute appropriate numerical and graphical summary, and comment on the difference

# reed_frost_realisation <- function(){
#   # you will need to implement
#   # if(SOME IS LESS THAN SOME){
#   #   
#   # }else{
#   #   
#   # }
#   
#   }
# }

rf_par <- list(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3, lockdown_time = 4, z_lockdown = 0.0025)
z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)


