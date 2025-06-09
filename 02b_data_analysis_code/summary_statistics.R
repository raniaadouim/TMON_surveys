## Summary Statistics
# This file calculates summary statistics about the datasets
# Written by Abigail Lewis
# May 2025

#Set up repo
#This folder does not get pushed to GitHub
if(!file.exists("03a_figures")){dir.create("03a_figures")} 

#Load packages
library(tidyverse)
library(ggh4x) #To customize plots
library(trend) #for Sen's slope

#Load data
cover <- read_csv("01b_processed_data/plant_species_cover.csv")
other <- read_csv("01b_processed_data/other_environmental_cover.csv") %>%
  pivot_longer(cols = bare_ground:wrack,
               names_to = "species_code", values_to = "fractional_cover"
  )
bio <- read_csv("01b_processed_data/all_biomass.csv")

#Calculate statistics
length(unique(cover$species_code))
length(unique(bio$species_code))
