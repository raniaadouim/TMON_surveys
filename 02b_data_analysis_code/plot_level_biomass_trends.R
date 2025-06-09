## Plot level biomass trends
# This file plots the trend in biomass for several plant species
# Written by Abigail Lewis
# May 2025

#Set up repo
#This folder does not get pushed to GitHub
if(!file.exists("03a_figures")){dir.create("03a_figures")} 

#Load packages
library(tidyverse)

#Load data
bio <- read_csv("01b_processed_data/all_biomass.csv")

bio %>%
  filter(method == "allometry",
         transect %in% c("G1", "G2", "G3")) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(total_biomass, na.rm = T),
         species_code = ifelse(species_code %in% c("SPPA", "SCAM", "PHAU", 
                                                   "IVFR", "SOSE"),
                               species_code,
                               "OTHER")
  ) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fract_biomass = sum(total_biomass, na.rm = T)/unique(sum)) %>%
  ggplot(aes(x = year, y = fract_biomass, fill = species_code)) +
  geom_col()+
  facet_grid(transect~plot)
