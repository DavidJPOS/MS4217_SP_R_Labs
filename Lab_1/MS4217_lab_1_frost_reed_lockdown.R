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
      ### !!!! This is where the magic happens, if we are outside of lockdown time
      realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z)^realisation_df$inf[t])
      realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
    }else{
      ### !!! the model works as normal, if we are inside lockdown time then the infection prob is replaced by z_lockdown
      realisation_df$inf[t+1] <- rbinom(1, realisation_df$sus[t],1-(1-z_lockdown)^realisation_df$inf[t])
      realisation_df$sus[t+1] <- realisation_df$sus[t] - realisation_df$inf[t+1]
    }
  }
  return(realisation_df)
}

#
mc_reed_frost <- function(M = 500, rf_par = list(z = 0.002, steps = 30, sus_0 =497, inf_0 = 3, z_lockdown = 0.0025)){
  
  mc_results = tibble()
  for(m in 1:M){
    realisation <- reed_frost_realisation(rf_par$z, rf_par$steps, rf_par$sus_0, rf_par$inf_0, rf_par$lockdown_time, rf_par$z_lockdown) %>% mutate(sim_no = m)
    mc_results <- bind_rows(mc_results, realisation)
  }
  
  return(mc_results) 
}



# simulate with lockdown ----------------------------------------------------------------------


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


# simulate with no lockdown -------------------------------------------------------------------

# change lockdown time to Inf, ie the if statement will always be tRUE
rf_par <- list(z = 0.003, steps = 30, sus_0 =497, inf_0 = 3, lockdown_time = Inf, z_lockdown = 0.0025)
z_vec <- seq(from = 0.003, to = 0.005, by = 0.0005)

var_z_no_lockdown_df <- tibble()
for(z_i in 1:length(z_vec)){
  rf_par$z <- z_vec[z_i]
  temp_df <- mc_reed_frost(M = 500, rf_par) %>% mutate(z = rf_par$z)
  
  var_z_no_lockdown_df <- bind_rows(var_z_no_lockdown_df, temp_df)
  print(z_i/length(z_vec))
}

var_z_summ <- 
  var_z_no_lockdown_df %>% group_by(time, z) %>% 
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


# add the differnece between lockdown and no lock down to the data frame
var_z_summ$inf_mean_diff = var_z_lockdown_summ$mean_inf - var_z_summ$mean_inf
var_z_summ$sus_mean_diff = var_z_lockdown_summ$mean_sus - var_z_summ$mean_sus

# what is the expected change in the number of cases? 
var_z_summ %>% 
  ggplot(aes(x = time, y = inf_mean_diff, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +  
  labs(color='Prob of Inf') +
  ylab('Difference in daily infected cases') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results") 

# what is the expected change in the number of cases? 
var_z_summ %>% 
  ggplot(aes(x = time, y = sus_mean_diff, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +  
  labs(color='Prob of Inf') +
  ylab('Difference in Susceptible') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results") 


ggplot(data = var_z_summ, aes(x = time, y = mean_inf, color = factor(z))) + 
  geom_point() + 
  geom_line() +
  geom_line(data = var_z_lockdown_summ, aes(x = time, y = mean_inf, color = factor(z)), linetype = 'dashed') +
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +  
  facet_wrap(~z, scale = 'free') + 
  labs(color='Prob of Inf') +
  ylab('Daily number of infected cases') + 
  xlab('Time') + 
  ggtitle("Reed-Frost Simulation Results") 

