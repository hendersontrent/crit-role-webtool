
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
library(ggridges)
library(googlesheets4)
library(plotly)
library(shinycssloaders)
library(nnet)
library(sjPlot)

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

# Define tab names

navtab0 <- "HOME"
navtab1 <- "CHARACTER ANALYSIS"
navtab2 <- "STATISTICAL MODELLING"
navtab3 <- "ABOUT"

# List of characters

the_nein <- c("Beau", "Caduceus", "Caleb", "Fjord", "Jester", "Veth", "Yasha", 
              "Molly", "Nott")

# Define a vector of nice colours for each character to use in plotting

the_palette <- c("Beau" = "#A0E7E5",
                 "Caduceus" = "#75E6DA",
                 "Caleb" = "#189AB4",
                 "Fjord" = "#05445E",
                 "Jester" = "#9571AB",
                 "Veth/Nott" = "#FD62AD",
                 "Yasha" = "#F7C9B6",
                 "Molly" = "#E7625F")

# Make a palette for the state space model

ss_palette <- c("#F84791", "#FFA384")

# Load data from Crit Role Stats website // NOTE: TO BE ADDED, ANALYSIS IS HARD CODED FOR NOW

damage <- read_excel("data/Damage Dealt - Wildemount.xlsx", sheet = 1)
healing <- read_excel("data/Healing Given - Wildemount.xlsx", sheet = 1)
rolls_raw <- read_excel_allsheets("data/All Rolls - Wildemount.xlsx")

#damage <- read_sheet("https://docs.google.com/spreadsheets/d/1SSrrWbtx1i4EJBPXLSfhcbiOrWXHdd0WW7-9oGt2dm4/edit#gid=0")
#healing <- read_sheet("https://docs.google.com/spreadsheets/d/1wlauv_gP3m8JQyXNbwNYS-WwPtvYFo88mjNLJEEQstQ/edit#gid=0")

# Turn off scientific notation

options(scipen = 999)
