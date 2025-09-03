# Messing around with data on biomass
# to look at: changes in % biomass
# zoom in on a particular year? 
# Combine data from plots to find year to year changes ? 
# Do something to fix significance - PHAU not getting identified

# Loading in packages
library(tidyverse)
library(kableExtra)
library(energy)
library(openmeteo)

# Reading in data
mass <- read_csv("raw_data/all_biomass.csv") 

# Cleaning and trimming data
biomass <- mass |>
  select(year, transect:species_code, total_biomass) |>
  filter(species_code %in% c("SPPA", "DISP", "PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  # Some plants were measured by both harvest and allometry. Averaging values
  summarize(total_biomass = round(mean(total_biomass), 3)) |>
  group_by(transect, plot, species_code) |>
  complete(year = 2015:2024,
           fill = list(total_biomass = 0)) |>
  arrange(transect, plot, species_code, year)

# Master plot of all data
masterplot_bio <- biomass |>
  ggplot(aes(x = year, y = total_biomass, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

masterplot_bio

# Notes from masterplot:
# Several plots where PHAU is by itself, without other plants. Still haas some 
# sharp fluctuations in biomass.
# SPPA looks to have the most year-to-year variation, across almost all plots it
# is in. Does not totally outcompete other plants though
# The last year has shown some pretty low biomass levels of all/most plants in 
# some plots. DISP seems ignorable

# Calculating sen's slope
slope_bio <- biomass |>
  mutate(trend = round(sens.slope(total_biomass)$estimates[1], 3),
         p = round(sens.slope(total_biomass)$p.value[1], 4)) |>
  arrange(transect, plot, species_code, year)

# Trying to filter out plots with no significant changes (convoluted)
tmpb <- slope_bio |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Plots with significant changes only
significant_bio <- slope_bio |>
  inner_join(tmpb, by = c("transect", "plot")) |>
  select(-ptmp)

# Graphing just the plots with significant changes in percent cover
sigplot_bio <- significant_bio |>
  ggplot(aes(x = year, y = total_biomass, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

sigplot_bio

# Notes from sigplot:
# 21 significant plots (fewer than with percent cover)
# across basically all plots where SPPA occurs, it has incredibly stark 
# year-to-year variation. After any increase, it returns to (near) 0 the next year
# G2 10: PHAU decreased to 0 after a sharp increase ... what happened?
# DISP had no significant changes in any of the plots - remove for future analysis?
# IVFR was low in the middle years and has grown in the last couple of years
# SPPA doesn't seem to outcompete the other plants

# Preparing for a table showing plants/plots with significant changes

tableprep <- significant_bio |>
  filter(p < 0.05, year == 2015) |>
  select(-c("year", "total_biomass")) |>
  arrange(trend)

sigtable <- kable(tableprep) 
sigtable

# Notes from sigtable
# IVFR had the top 2 greatest increases in biomass
# PHAU had the 3rd, 4th, 5th greatest increases in biomass. There was no
# significant decrease in any plot
# IVFR had the top 4 greatest decreases in biomass
# Remove DISP from future analysis - just one significant plot that has a trend of 0
# SCAM had a mix of positive and negative trends, and had the most significant 
# changes
# SPPA does not show up

# Reworking data to consider changes in % biomass
# For now: a change from 0 to some biomass is represented as a 100% change

percent_bio <- biomass |>
  mutate(previous = lag(total_biomass),
         percent_change = case_when(previous != 0 ~
                                      round((total_biomass - previous) / previous, 3),
                                    total_biomass == 0 ~ 0,
                                    TRUE ~ 1)) |>
  drop_na(previous) |>
  # Calculating sens slope
  group_by(transect, plot, species_code) |>
  mutate(trend = round(sens.slope(percent_change)$estimates[1], 3),
         p = round(sens.slope(percent_change)$p.value[1], 4)) |>
  relocate(percent_change, .after = year) |>
  arrange(transect, plot, species_code, year)

masterplot_percent_bio <- percent_bio |>
  ggplot(aes(x = year, y = percent_change, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

masterplot_percent_bio

# Notes from masterplot of percent change:
# IVFR has an insane percent change of over 200% in G3 3
# SPPA has very zig-zagged percent change that is at the extremes of its plot

# Finding plots with significant changes
tmppb <- percent_bio |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Finding data on only the plots with significant changes
significant_percent_bio <- percent_bio |>
  select(!c("total_biomass", "previous")) |>
  inner_join(tmppb, by = c("transect", "plot")) |>
  select(!ptmp)

sigplot_percent_bio <- significant_percent_bio |>
  ggplot(aes(x = year, y = percent_change, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"))

sigplot_percent_bio

# Creating a table for the plot-plants with significant changes
tableprep_pb <- significant_percent_bio |>
  filter(p < 0.05, year == 2016) |>
  select(-c("year", "percent_change")) |>
  arrange(trend)

sigtable_pb <- kable(tableprep_pb) 
sigtable_pb

# Trying a new approach: calculating % change based on not the previous biomass,
# but the initial biomass

# Finding the initial biomass for each plant-plot
tmp_initials <- biomass |>
  filter(year == 2015) |>
  select(!year) |>
  rename(original = total_biomass)

percent_bio_new <- biomass |>
  full_join(tmp_initials, by = c("transect", "plot", "species_code")) |>
  mutate(percent_change = case_when(original != 0 ~
                                      round((total_biomass - original) / original, 3),
                                    total_biomass == 0 ~ 0,
                                    TRUE ~ 1)) |>
  # Calculating sens slope
  group_by(transect, plot, species_code) |>
  mutate(trend = round(sens.slope(percent_change)$estimates[1], 3),
         p = round (sens.slope(percent_change)$p.value[1], 3)) 

# Finding plots with significant changes
tmppbnew <- percent_bio_new |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Finding data on only the plots with significant changes
sig_pbnew <- percent_bio_new |>
  select(!c("total_biomass", "original")) |>
  inner_join(tmppbnew, by = c("transect", "plot")) |>
  select(!ptmp)

sigplot_pbnew <- sig_pbnew |>
  ggplot(aes(x = year, y = percent_change, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

sigplot_pbnew

# Notes from sigplot_pbnew:
# Need to resolve the issue about calculating % change when starting from 0
# 2020 was a great year for SCAM growth
# 2019 was a great year for PHAU growth
# IVFR doesn't show up too much
# Just from the scales: Barely any plots, and by only small quantities, ended up
# with lower biomass than they started with

# Creating a table for the plant-plot combos with significant changes
tableprep_pbnew <- sig_pbnew |>
  filter(p < 0.05, year == 2015) |>
  select(-c("year", "percent_change")) |>
  arrange(trend)

sigtable_pbnew <- kable(tableprep_pbnew) 
sigtable_pbnew

# Notes from table:
# IVFR still has the top 4 largest % decreases, but does not show up in the top
# of largest % increases
# SCAM shows up a lot more in the top spots, with the #1 largest % increase
# DISP again does not show up except for its null trend. Remove from analysis
# SPPA also does not show up

# Attempting to perform a distance correlation test, on total_biomass (all)
# Creating a function to do this. 

dist_corr <- data.frame(transect = character(), plot = numeric(),
                        phau_scam = numeric(), phau_ivfr = numeric(), 
                        phau_sppa = numeric(), scam_ivfr = numeric())

dist_func <- function(one, two, dist_corr) {
  for(x in 1:19) {
    one_tmp <- biomass |>
      filter(species_code == toupper(one), transect == "G1", plot == x) |>
      pull(total_biomass)
    
    two_tmp <- biomass |>
      filter(species_code == toupper(two), transect == "G1", plot == x) |>
      pull(total_biomass)
    
    name <- paste(one, two, sep = "_")
    
    # Adding the transect, plot, and correlation values into the data frame
    dist_corr[x, 1] <- "G1"
    dist_corr[x, 2] <- x
    
    # Some plants don't show up in any plots
    if((length(one_tmp) == length(two_tmp)) & length(one_tmp !=0)) {
      dist_corr[x, name] <- dcor(one_tmp, two_tmp)
    } else {
      dist_corr[x, name] <- NA
    }
    
  }
  
  for(x in 1:11) {
    one_tmp <- biomass |>
      filter(species_code == toupper(one), transect == "G2", plot == x) |>
      pull(total_biomass)
    
    two_tmp <- biomass |>
      filter(species_code == toupper(two), transect == "G2", plot == x) |>
      pull(total_biomass)
    
    name <- paste(one, two, sep = "_")
    dist_corr[x+19, 1] <- "G2"
    dist_corr[x+19, 2] <- x
    
    if((length(one_tmp) == length(two_tmp)) & length(one_tmp !=0)) {
      dist_corr[x+19, name] <- dcor(one_tmp, two_tmp)
    } else {
      dist_corr[x+19, name] <- NA
    }
   
  }
  
  for(x in 1:5) {
    one_tmp <- biomass |>
      filter(species_code == toupper(one), transect == "G3", plot == x) |>
      pull(total_biomass)
    
    two_tmp <- biomass |>
      filter(species_code == toupper(two), transect == "G3", plot == x) |>
      pull(total_biomass)
    
    name <- paste(one, two, sep = "_")
    
    dist_corr[x+30, 1] <- "G3"
    dist_corr[x+30, 2] <- x
    
    if((length(one_tmp) == length(two_tmp)) & length(one_tmp !=0)) {
      dist_corr[x+30, name] <- dcor(one_tmp, two_tmp)
    } else {
      dist_corr[x+30, name] <- NA
    }
  }
  return(dist_corr)
}


# Running function on pairs of interest
dist_corr <- dist_func("phau", "scam", dist_corr)
dist_corr <- dist_func("phau", "ivfr", dist_corr)
dist_corr <- dist_func("phau", "sppa", dist_corr)
dist_corr <- dist_func("scam", "ivfr", dist_corr) |>
  # Removing plots that have no interactions of this combination
  filter(!if_all(3:6, is.na))


# Notes from distance correlation: 
# PHAU_SCAM ranges from 0.34 - 0.65
# PHAU_IVFR ranges from 0.1 - 0.72
# PHAU_SPPA ranges from 0.20 - 0.37
# SCAM_IVFR ranges from 0.32 - 0.84
# G2 7 had top 3 highest correlations across all pairs



  






  



