## Plot level percent cover trends
# This file plots the trend in percent cover for several plant species
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

#Combine plant cover with other data
cover <- cover %>%
  bind_rows(other)

#Plot

# Set formatting for facet "strips" in the following plots
strips <- strip_nested(
  background_x = list(element_rect(fill = "white", color = c("grey20")),
                      element_blank()),
  text_x = elem_list_text(color = c("black", "grey50"),
                          size = c(NA, 7),
                          face = c("italic", NA),
                          margin = list(ggplot2::margin(t = 2, b = 1,0,0, unit = "pt"), NULL)),
  by_layer_x = TRUE,
  size = "variable")

# Create a function that will make these plots in the exact same way for each species
plot_plant <- function(code, # Code for this plant
                       scientific_name, # Scientific name for y-axis label
                       data # Dataset to use
                       ){
  #Plot
  plot_out <- data %>%
    filter(species_code %in% code) %>%
    ggplot(aes(x = year, y = fractional_cover*100)) +
    geom_line(aes(color = color))+
    geom_point(size = 0.5, aes(color = color)) +
    ylab(bquote(italic(.(scientific_name))~"percent cover"))+
    scale_x_continuous(breaks = seq(2017, 2022, 4),
                       minor_breaks = seq(2015, 2024, 1))+
    scale_color_manual(values = c("Increasing" = "green4",
                                  "Decreasing" = "darkred",
                                  "Insignificant" = "grey"),
                       name = "Trend")+
    ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "grey95"),
          axis.title.x = element_blank(),
          legend.position = c(.92,0.1))+
    force_panelsizes(rows = unit(.75, "in"),
                     cols = unit(1, "in"))
  
  return(plot_out)
}

# Calculate Sen's slopes
sens_slopes <- cover %>%
  #First, add 0s for years that a given plant was not observed
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  #Calculate the total percent cover
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  #Re-calculate fractional cover for each species
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         !is.na(fractional_cover)) %>%
  #Calculate Sen's slope trends
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  mutate(trend = trend::sens.slope(fractional_cover)$estimates[1],
         p = trend::sens.slope(fractional_cover)$p.value[1],
         color = ifelse(trend >= 0, 
                        "Increasing",
                        "Decreasing"),
         color = ifelse(p > 0.05, 
                        "Insignificant",
                        color))

# Use the function above to generate plots for focal species
# SPPA
jpeg("03a_figures/SPPA.jpg", res = 300, width = 6, height = 7.5, units = "in")
plot_plant("SPPA", "Spartina patens", sens_slopes)
dev.off()

# PHAU
jpeg("03a_figures/Rise of PHAU.jpg", res = 300, width = 6, height = 5, units = "in")
plot_plant("PHAU", "Phragmites australis", sens_slopes)
dev.off()

# IVFR
jpeg("03a_figures/IVFR.jpg", res = 300, width = 6, height = 6.5, units = "in")
plot_plant("IVFR", "Iva frutescens", sens_slopes)
dev.off()

# SCAM
jpeg("03a_figures/SCAM.jpg", res = 300, width = 6, height = 7, units = "in")
plot_plant("SCAM", "Schoenoplectus americanus", sens_slopes)
dev.off()

# SPCY
jpeg("03a_figures/SPCY.jpg", res = 300, width = 6, height = 3, units = "in")
plot_plant("SPCY", "Spartina cynosuroides", sens_slopes)
dev.off()

# Summarize trends
sens_slopes %>%
  select(transect, plot, species_code, trend, color) %>%
  distinct() %>%
  filter(species_code %in% c("SCAM", "IVFR", "PHAU")) %>% 
  group_by(species_code, color) %>%
  summarize(trend = mean(trend),
            n = n())
