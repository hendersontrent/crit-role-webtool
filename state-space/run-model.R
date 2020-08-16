#----------------------------------------
# This script sets out to produce a
# state-space model for healing and
# damage
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 16 August 2020
#----------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(data.table)
library(janitor)
library(rstan)

# Turn off scientific notation

options(scipen = 999)

# Load in data

damage <- read_excel("data/Damage Dealt - Wildemount.xlsx", sheet = 1)
healing <- read_excel("data/Healing Given - Wildemount.xlsx", sheet = 1)

#-----------------
# Process and join
#-----------------

cleaner <- function(data){
  data <- data[-c(1,2), ]
  
  data <- data %>%
    clean_names() %>%
    rename(episode = 1) %>%
    gather(key = character, value = value, 2:9) %>%
    mutate(episode = case_when(
      episode == "43132.0" ~ "02-01",
      episode == "43133.0" ~ "02-02",
      episode == "43134.0" ~ "02-03",
      episode == "43135.0" ~ "02-04",
      episode == "43136.0" ~ "02-05",
      episode == "43137.0" ~ "02-06",
      episode == "43138.0" ~ "02-07",
      episode == "43139.0" ~ "02-08",
      episode == "43140.0" ~ "02-09",
      episode == "43141.0" ~ "02-10",
      episode == "43142.0" ~ "02-11",
      episode == "43143.0" ~ "02-12",
      episode == "43144.0" ~ "02-13",
      episode == "43145.0" ~ "02-14",
      episode == "43146.0" ~ "02-15",
      episode == "43147.0" ~ "02-16",
      episode == "43148.0" ~ "02-17",
      episode == "43149.0" ~ "02-18",
      episode == "43150.0" ~ "02-19",
      episode == "43151.0" ~ "02-20",
      episode == "43152.0" ~ "02-21",
      episode == "43153.0" ~ "02-22",
      episode == "43154.0" ~ "02-23",
      episode == "43155.0" ~ "02-24",
      episode == "43156.0" ~ "02-25",
      episode == "43157.0" ~ "02-26",
      episode == "43158.0" ~ "02-27",
      episode == "43159.0" ~ "02-28",
      TRUE                 ~ episode)) %>%
    mutate(episode = gsub(".*-", "", episode)) %>%
    mutate(episode = as.numeric(episode)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(character = str_to_title(character))
  
  data[data == "N/A"] <- NA
  
  return(data)
}

damage_clean <- cleaner(damage) %>%
  rename(damage = value)

healing_clean <- cleaner(healing) %>%
  rename(healing = value)

dam_heals <- damage_clean %>%
  left_join(healing_clean, by = c("episode" = "episode", "character" = "character"))

#--------------------------- PRE PROCESSING ------------------------

# Aggregate to episode sums

raw_data <- dam_heals %>%
  gather(key = variable, value = value, c(damage, healing)) %>%
  mutate(variable = str_to_sentence(variable)) %>%
  group_by(episode, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(variable) %>%
  mutate(n = n(), sd = sd(value)) %>%
  ungroup()

#--------------------------- MODEL SPEC ----------------------------

the_vars <- unique(raw_data$variable)

some_list <- list()

for(i in the_vars){
  
  shorter <- raw_data %>%
    filter(variable == i)

  d1 <- list(
    mu_start = first(shorter$value),
    n_eps = nrow(shorter),
    y_values = shorter$value,
    sigma = unique(shorter$sd)
  )
  
  system.time({
    mod <- stan(file = "state-space/ss-model.stan", data = d1, iter = 4000, control = list(max_treedepth = 20))
  })
  
  ex <- as.data.frame(rstan::extract(mod, "mu"))
  
  outs <- ex %>%
    gather(key = episode, value = value, 1:105) %>%
    mutate(episode = gsub("mu.", "\\1", episode)) %>%
    mutate(episode = as.numeric(episode)) %>%
    group_by(episode) %>%
    summarise(mean = mean(value),
              upper = quantile(value, 0.975),
              lower = quantile(value, 0.025)) %>%
    ungroup() %>%
    mutate(variable = i)
  
  some_list[[i]] <- outs

}

full_models <- rbindlist(some_list, use.names = TRUE)

#--------------------------- OUTPUT --------------------------------

save(full_models, file = "webtool/data/full_models.Rda")
