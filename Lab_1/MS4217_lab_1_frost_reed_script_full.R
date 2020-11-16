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

# what does the reed frost algo do?   

for(t in 1:(steps-1)){ # well for each time
  # we find the next number of infections by a binomial dist with prob below
  realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t], 1-(1-z)^realisation_df$inf[t])
  realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
  }

# what does this look like?
realisation_df %>% 
  ggplot(aes(x = time, y = inf)) + 
  geom_point() + geom_line() + 
  xlab('Time') + ylab('Number Infected') + 
  ggtitle('Single realisation of Frost-Reed')

# ex 2: build mc simulation -------------------------------------------------------------------------

# create functions to do one simulation
reed_frost_realisation <- function(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3){
  
  realisation_df <- tibble(time = 1:steps, sus = numeric(steps), inf = numeric(steps))
  sus <- numeric(steps) # where we going to save the number of sus people
  inf <- numeric(steps) # and infected people
  realisation_df$sus[1] <- sus_0 # initial conditions
  realisation_df$inf[1] <- inf_0
  
  for(t in 1:(steps-1)){
    realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z)^realisation_df$inf[t])
    realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
  }
  
  return(realisation_df)
}


# mc wrapper for what we want to do
mc_reed_frost <- function(M = 500){
  
  mc_results = tibble() # this is a really bad way of doing this....
  for(m in 1:M){
    realisation <- reed_frost_realisation() %>% mutate(sim_no = m) # add the sim no to df
    mc_results <- bind_rows(mc_results, realisation) # add results 
  }
  
  return(mc_results) 
}


# ex 3: simulate model, plot and comment -----------------------------------------------------

# First generate mc simulations from frost reed.
# plot and comment on a 5 realisations
# generate summary statistics, the mean number of inf and sus, sd and a 80% CI based on percentiles
# plot the average number of infections with confidence intervals
# plot number of sus and inf on a single plot
# create a panel plot of both of the previous plots using the packages cowplot package

# simulate Reed-Frost 500 times using the
# z = 0.003, steps = 30, sus_0 =497, inf_0 = 3
reed_frost_mc_df <- mc_reed_frost(M = 500)

# first plot one realisation
reed_frost_mc_df %>% 
  filter(sim_no %in% 1:4) %>% # print(n = Inf) %>%
  ggplot(aes(x = time, y = inf, color = factor(sim_no))) + 
  geom_point() + 
  geom_line(linetype = 'dashed') +
  ylab('No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results") + 
  labs(color = 'Sim No.')

# generate a summary table of behaviour of model over time
reed_frost_summ <- 
  reed_frost_mc_df %>% group_by(time) %>% 
  summarise(
    mean_inf = mean(inf),
    mean_sus = mean(sus),
    
    sd_inf = sd(inf),
    sd_sus = sd(sus), 
    
    l80_inf = quantile(inf, 0.1),
    u80_inf = quantile(inf, 0.9),
    
    l80_sus = quantile(sus, 0.1),
    u80_sus = quantile(sus, 0.9)
  )


# plot the expected number infections
p1 <- reed_frost_summ %>% 
  ggplot(aes(x = time, y = mean_inf)) + 
  geom_point() + 
  geom_line(linetype = 'dashed') +
  xlab('No Infected') + 
  ylab('Time') + 
  ggtitle("Reed-Frost Simulation Results")
p1

p2 <- reed_frost_summ %>% 
  ggplot(aes(x = time, y = cum_mean_inf)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 503, linetype = 'dashed', color = 'red') + 
  geom_line(linetype = 'dashed') +
  ylab('Total No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results")
p2

# plot both the number of sus and infected 
reed_frost_summ_long <- 
  reed_frost_summ %>% select(1:3) %>%  
  pivot_longer(2:3, names_to = 'varaible', values_to = 'values') %>% 
  arrange(time)
 
p3 <- reed_frost_summ_long %>% 
  ggplot(aes(x = time, y = values, color = varaible)) + 
  geom_point() + 
  geom_line(linetype = 'dashed') +
  ylab('No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results")


# add a confidence interval
p4 <- reed_frost_summ %>%
  ggplot(aes(x = time, y = mean_inf)) + 
  geom_ribbon(aes(ymin = l80_inf, ymax = u80_inf), color = 'black', fill = 'gold', alpha = 0.5) + 
  geom_point() + 
  geom_line(linetype = 'dashed') +
  ylab('No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results")

pg1 <- cowplot::plot_grid(p4, p3, labels = c('(A)', '(B)'))

#you can save using ggsave()


# explore different z values ------------------------------------------------------------------

# refactor your function to allow for different parameters
# simulate the following range of z values, plot the average number of infections
#   z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)


# change the mc_reed_function to so it can take a list for parameters
mc_reed_frost <- function(M = 500, rf_par = list(z = 0.003, steps = 20, sus_0 =497, inf_0 = 3)){
  
  mc_results = tibble()
  for(m in 1:M){
    realisation <- reed_frost_realisation(rf_par$z, rf_par$steps, rf_par$sus_0, rf_par$inf_0) %>% mutate(sim_no = m)
    mc_results <- bind_rows(mc_results, realisation)
  }
  
  return(mc_results) 
}

# how define the varaibles
rf_par <- list(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3)
z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)

# var the value of z for each across MC simulations
var_z_df <- tibble()
for(z_i in 1:length(z_vec)){
  rf_par$z <- z_vec[z_i]
  temp_df <- mc_reed_frost(M = 500, rf_par) %>% mutate(z = rf_par$z)
  
  var_z_df <- bind_rows(var_z_df, temp_df)
  print(z_i/length(z_vec))
}

var_z_summ <- 
  var_z_df %>% group_by(time, z) %>% 
  summarise(
    mean_inf = mean(inf),
    mean_sus = mean(sus),
    
    sd_inf = sd(inf),
    sd_sus = sd(sus), 
    
    l80_inf = quantile(inf, 0.1),
    u80_inf = quantile(inf, 0.9),
    
    l80_sus = quantile(sus, 0.1),
    u80_sus = quantile(sus, 0.9)
  ) %>% ungroup()

# plot the expected number infections
var_z_summ %>% 
  ggplot(aes(x = time, y = mean_inf, color = factor(z))) + 
  geom_point() + 
  geom_line(linetype = 'dashed') +
  ylab('No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results")



# lock down simulation ------------------------------------------------------------------------

# say we plan on implementing a lockdown at time 5 where drop by if it decreases to z = 0.0025 
# again, refactor you code to allow for this

# examine the difference in the average number of infected individuals per time under both lockdown and non-lock conditions compute appropriate numerical and graphical summary, and comment on the difference

reed_frost_realisation <- function(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3, lockdown_time = Inf, z_lockdown = 0.0025){
  
  realisation_df <- tibble(time = 1:steps, sus = numeric(steps), inf = numeric(steps))
  sus <- numeric(steps) # where we going to save the number of sus people
  inf <- numeric(steps) # and infected people
  realisation_df$sus[1] <- sus_0 # initial conditions
  realisation_df$inf[1] <- inf_0
  
  for(t in 1:(steps-1)){
    if(t <= lockdown_time){
      realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z)^realisation_df$inf[t])
      realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
    }else{
      realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z_lockdown)^realisation_df$inf[t])
      realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
    }
    
  }
  
  return(realisation_df)
}

mc_reed_frost <- function(M = 500, rf_par = list(z = 0.002, steps = 30, sus_0 =497, inf_0 = 3, z_lockdown = 0.0025)){
  
  mc_results = tibble()
  for(m in 1:M){
    realisation <- reed_frost_realisation(rf_par$z, rf_par$steps, rf_par$sus_0, rf_par$inf_0, rf_par$lockdown_time, rf_par$z_lockdown) %>% mutate(sim_no = m)
    mc_results <- bind_rows(mc_results, realisation)
  }
  
  return(mc_results) 
}

rf_par <- list(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3, lockdown_time = 4, z_lockdown = 0.0025)
z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)

var_z_lockdown_df <- tibble()
for(z_i in 1:length(z_vec)){
  rf_par$z <- z_vec[z_i]
  temp_df <- mc_reed_frost(M = 500, rf_par) %>% mutate(z = rf_par$z)
  
  var_z_lockdown_df <- bind_rows(var_z_lockdown_df, temp_df)
  print(z_i/length(z_vec))
}

var_z_lockdown_summ <- 
  var_z_lockdown_df %>% group_by(time, z) %>% 
  summarise(
    mean_inf = mean(inf),
    mean_sus = mean(sus),
    
    sd_inf = sd(inf),
    sd_sus = sd(sus), 
    
    l95_inf = quantile(inf, 0.025),
    u95_inf = quantile(inf, 0.975),
    
    l95_sus = quantile(sus, 0.025),
    u95_sus = quantile(sus, 0.975)
  ) %>% ungroup()

# plot the expected number infections
var_z_lockdown_summ %>% 
  ggplot(aes(x = time, y = mean_inf, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') + 
  ylab('No Infected') + 
  xlab('Time') + 
  labs(color = "z") + 
  ggtitle("Reed-Frost Simulation Results")


var_z_summ$mean_diff = var_z_lockdown_summ$mean_inf - var_z_summ$mean_inf

var_z_summ %>% 
  ggplot(aes(x = time, y = mean_diff, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +  
  labs(color='Prob of Inf') +
  ylab('No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results") 



# lifted lockdown -----------------------------------------------------------------------------


reed_frost_realisation <- function(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3, lockdown_time = Inf, z_lockdown = 0.0025){
  
  realisation_df <- tibble(time = 1:steps, sus = numeric(steps), inf = numeric(steps))
  sus <- numeric(steps) # where we going to save the number of sus people
  inf <- numeric(steps) # and infected people
  realisation_df$sus[1] <- sus_0 # initial conditions
  realisation_df$inf[1] <- inf_0
  
  for(t in 1:(steps-1)){
    if(t <= lockdown_time | t >= 12){
      realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z)^realisation_df$inf[t])
      realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
    }else{
      realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z_lockdown)^realisation_df$inf[t])
      realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
    }
    
  }
  
  return(realisation_df)
}

mc_reed_frost <- function(M = 500, rf_par = list(z = 0.002, steps = 30, sus_0 =497, inf_0 = 3, z_lockdown = 0.0025)){
  
  mc_results = tibble()
  for(m in 1:M){
    realisation <- reed_frost_realisation(rf_par$z, rf_par$steps, rf_par$sus_0, rf_par$inf_0, rf_par$lockdown_time, rf_par$z_lockdown) %>% mutate(sim_no = m)
    mc_results <- bind_rows(mc_results, realisation)
  }
  
  return(mc_results) 
}

rf_par <- list(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3, lockdown_time = 4, z_lockdown = 0.0025)
z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)

var_z_lockdown_df <- tibble()
for(z_i in 1:length(z_vec)){
  rf_par$z <- z_vec[z_i]
  temp_df <- mc_reed_frost(M = 500, rf_par) %>% mutate(z = rf_par$z)
  
  var_z_lockdown_df <- bind_rows(var_z_lockdown_df, temp_df)
  print(z_i/length(z_vec))
}

var_z_lockdown_summ <- 
  var_z_lockdown_df %>% group_by(time, z) %>% 
  summarise(
    mean_inf = mean(inf),
    mean_sus = mean(sus),
    
    sd_inf = sd(inf),
    sd_sus = sd(sus), 
    
    l95_inf = quantile(inf, 0.025),
    u95_inf = quantile(inf, 0.975),
    
    l95_sus = quantile(sus, 0.025),
    u95_sus = quantile(sus, 0.975)
  ) %>% ungroup()

# plot the expected number infections
var_z_lockdown_summ %>% 
  ggplot(aes(x = time, y = mean_inf, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') + 
  ylab('No Infected') + 
  xlab('Time') + 
  labs(color = "z") + 
  ggtitle("Reed-Frost Simulation Results")


var_z_summ$mean_diff = var_z_lockdown_summ$mean_inf - var_z_summ$mean_inf

var_z_summ %>% 
  ggplot(aes(x = time, y = mean_diff, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +  
  labs(color='Prob of Inf') +
  ylab('No Infected') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results") 



# Calculate of R_t for each time point --------------------------------------------------------

estimte_rt <- var_z_df %>% group_by(z, sim_no) %>% # for each z and sim
  # arrange(time) %>% 
  mutate(
    no_inf_last = c(NA, inf[1:(length(time)-1)]), # add a col with no inf last time set
    R_t = inf/no_inf_last # ratio of new infections to old ones
  ) %>% 
  ungroup() %>% group_by(z, time) %>% # regroup by z and time 
  summarise(
    mean_rt = mean(R_t, na.rm = TRUE), # what is the average R_t across all simulation @ t and z
    sd_rt = sd(R_t, na.rm = TRUE)
  ) %>% ungroup()

estimte_rt %>% filter(time <= 20) %>% 
  ggplot(aes(x = time, y = mean_rt, color = factor(z))) +
  geom_point() + geom_line()

# note that the probability z hasn't change but the number of people who CAN become inf is changing

# do the same as above but with the lockdown effect
estimte_rt_lockdown <- var_z_lockdown_df %>% group_by(z, sim_no) %>% 
  # arrange(time) %>% 
  mutate(
    no_inf_last = c(NA, inf[1:(length(time)-1)]),
    R_t = inf/no_inf_last
  ) %>% ungroup() %>% group_by(z, time) %>% 
  summarise(
    mean_rt = mean(R_t, na.rm = TRUE),
    sd_rt = sd(R_t, na.rm = TRUE)
  ) %>% ungroup()

estimte_rt_lockdown %>% filter(time <= 20) %>% 
  ggplot(aes(x = time, y = mean_rt, color = factor(z))) +
  geom_point() + geom_line()

