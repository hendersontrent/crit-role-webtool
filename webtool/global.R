
library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(data.table)
library(shinyWidgets)
library(scales)
library(readxl)
library(janitor)
library(rstan)
library(png)
library(plotly)
library(shinycssloaders)
library(nnet)
library(caTools)
library(mgcv)
library(highcharter)
library(igraph)
library(networkD3)

# Load HTML files

import_files <- list.files("imports", full.names = TRUE, pattern = "\\.html")
for(f in import_files){
  object_name <- gsub("imports/", "", f)
  object_name <- gsub("\\.html", "", object_name)
  assign(object_name, readLines(f, warn = FALSE))
}

# Load R helper functions

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}

# Load state space data

data_files <- list.files("data", full.names= TRUE, pattern = "\\.Rda", all.files = TRUE)
for(d in data_files){
  load(d)
}

# Define tab names

navtab0 <- "HOME"
navtab1 <- "CHARACTER ANALYSIS"
navtab3 <- "ABOUT"
navtab4 <- "MONETARY ANALYSIS"
navtab5 <- "SPELLCASTING ANALYSIS"
navtab6 <- "POTION ANALYSIS"
navtab7 <- "VOX MACHINA"

# List of characters

the_nein <- c("Beau", "Caduceus", "Caleb", "Fjord", "Jester", "Veth/Nott", "Yasha", 
              "Molly")

# Define a vector of nice colours for each character to use in plotting

the_palette <- c("Beau" = "#A0E7E5",
                 "Caduceus" = "#75E6DA",
                 "Caleb" = "#189AB4",
                 "Fjord" = "#05445E",
                 "Jester" = "#9571AB",
                 "Veth/Nott" = "#FD62AD",
                 "Yasha" = "#F7C9B6",
                 "Molly" = "#E7625F")

vm_palette <- c("#A0E7E5","#75E6DA","#189AB4","#05445E","#9571AB",
                 "#FD62AD","#F7C9B6","#E7625F")

# Make a palette for the state space model

ss_palette <- c("#F84791", "#FFA384")

# Load data from Crit Role Stats website // NOTE: TO BE ADDED, ANALYSIS IS HARD CODED FOR NOW

damage <- read_excel("data/Damage Dealt - Wildemount.xlsx", sheet = 1)
healing <- read_excel("data/Healing Given - Wildemount.xlsx", sheet = 1)
rolls_raw <- read_excel_allsheets("data/All Rolls - Wildemount.xlsx")
money <- read_excel("data/money_clean.xlsx")
spellcasting <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 3)
potions <- read_excel("data/Potions Consumed - Wildemount.xlsx")
spellcasting_vm <- read_excel("data/Spells Cast - Tal'Dorei.xlsx", sheet = 2)

# Turn off scientific notation

options(scipen = 999)

# Useful lists for inputs

lev_choices <- c("All", "Levels under 6", "Levels over 6")
cluster_choices <- c("Yes", "No")
bar_rolls_data <- barplot_prep(rolls_raw)
the_roll_values <- unique(bar_rolls_data$total_value)

water_data <- money_prep(money)
the_episodes <- unique(water_data$episode)

spell_data <- spell_prep(spellcasting)
the_spell_levels <- unique(spell_data$spell_level)

spell_data_vm <- spell_prep_vm(spellcasting_vm)
the_spell_levels_vm <- unique(spell_data_vm$spell_level)

# Prep the data for character-level spellcasting

beau_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 4) %>%
  clean_names() %>%
  mutate(character = "Beau")

cad_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 5) %>%
  clean_names() %>%
  mutate(character = "Caduceus")

caleb_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 6) %>%
  clean_names() %>%
  mutate(character = "Caleb")

fjord_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 7) %>%
  clean_names() %>%
  mutate(character = "Fjord")

jester_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 8) %>%
  clean_names() %>%
  mutate(character = "Jester")

molly_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 9) %>%
  clean_names() %>%
  mutate(character = "Molly")

nott_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 10) %>%
  clean_names() %>%
  mutate(character = "Veth/Nott")

yasha_spells <- read_excel("data/Spells Cast - Wildemount.xlsx", sheet = 11) %>%
  clean_names() %>%
  mutate(character = "Yasha")

all_ch_spells <- bind_rows(beau_spells, cad_spells, caleb_spells, fjord_spells, jester_spells,
                           molly_spells, nott_spells, yasha_spells) %>%
  dplyr::select(c(1:4))

sankey_list <- unique(all_ch_spells$character)
