# In this R script, I source and clean data on potential explanatory variables.
# I then merge this with previously cleaned data on plant biomass and save
# this dataset as a .Rdata file

# Loading packages -------------------------------------------------------------
library(tidyverse)
library(VulnToolkit)

# Reading in data --------------------------------------------------------------
load("processed_data/biomass.Rdata")
elevation_raw <- read_csv("raw_data/gcrew_elevation.csv")
meteo_raw <- read_csv("raw_data/open_meteo.csv")

# Cleaning sea level & elevation data ------------------------------------------

# Scraping NOOA hourly water level data
water_level_raw <- noaa(begindate = 20141001, enddate = 20241001, 
                      station = "8575512", datum = "NAVD", interval = "hourly", 
                      time = "LST") |>
  janitor::clean_names()

# Cleaning water level data
water_level_tmp <- water_level_raw |>
  mutate(year = year(time_lst), month = month(time_lst), day = day(time_lst),
         # To join with elevation data later
         join = 1) |>
  rename(water = verified_water_level_at_8575512_meters_rel_to_navd) 
  
# Cleaning elevation data
elevation <- elevation_raw |>
  filter(measurement_type == "TMON vegetation plot") |>
  group_by(transect_id, plot_id) |>
  summarize(elevation = mean(elevation)) |>
  mutate(transect_id = case_when(transect_id == "g1" ~ "G1",
                                 transect_id == "g2" ~ "G2",
                                 TRUE ~ "G3"),
         join = 1) |>
  rename(plot = plot_id, transect = transect_id) |>
  # Removing plots where my species of interest don't show up
  filter(!((transect == "G1" & plot == 20) | (transect == "G3" & plot == 6))) 

# Calculating spring flooding duration and flooding depth
fld_spr <- water_level_tmp |>
  filter(month %in% c(3, 4, 5)) |>
  full_join(elevation, by = "join", relationship = "many-to-many") |>
  group_by(year, elevation) |>
  mutate(fld_dur_spr = fld.dur(elevation, water),
         fld_depth_spr = fld.depth(water, elevation)) |>
  select(year, transect, plot, elevation, fld_dur_spr, fld_depth_spr) |>
  distinct()

# Calculating summer flooding duration and flooding depth
fld_sum <- water_level_tmp |>
  filter(month %in% c(6, 7, 8)) |>
  full_join(elevation, by = "join", relationship = "many-to-many") |>
  group_by(year, elevation) |>
  mutate(fld_dur_sum = fld.dur(elevation, water),
         fld_depth_sum = fld.depth(water, elevation)) |>
  select(year, transect, plot, elevation, fld_dur_sum, fld_depth_sum) |>
  distinct()

# Calculating adjusted annual flooding duration and flooding depth
fld_adj <- water_level_tmp |>
  mutate(year = ifelse(year %in% c(10, 11, 12), year + 1, year)) |>
  filter(year < 2025 & year > 2014) |>
  full_join(elevation, by = "join", relationship = "many-to-many") |>
  group_by(year, elevation) |>
  mutate(fld_dur_adj = fld.dur(elevation, water),
         fld_depth_adj = fld.depth(water, elevation)) |>
  select(year, transect, plot, elevation, fld_dur_adj, fld_depth_adj) |>
  distinct()

# Merging with biomass data 
fld <- fld_spr |>
  full_join(fld_sum) |>
  full_join(fld_adj)

# Scraping NOAA monthly water level data
sea_level_raw <- noaa(begindate = 20141001, enddate = 20241231, 
                      station = "8575512", datum = "NAVD", interval = "monthly",
                      time = "LST") |>
  janitor::clean_names()

# Calculating spring mean sea level 
sea_level_spr <- sea_level_raw |>
  filter(month >= 3 & month <= 5) |>
  select(year, msl) |>
  group_by(year) |>
  summarize(msl_spr = mean(msl)) 

# Calculating adjusted annual mean sea level
sea_level_adj <- sea_level_raw |>
  mutate(year = ifelse(year %in% c(10, 11, 12), year + 1, year)) |>
  filter(year < 2025 & year > 2014) |>
  select(year, msl) |>
  group_by(year) |>
  summarize(msl_adj = mean(msl)) 

# Combining yearly and monthly msl data
sea_level <- sea_level_spr |>
  full_join(sea_level_adj, by = "year") 

# Cleaning meteorological data -------------------------------------------------

# Calculating total spring precipitation
meteo_spr <- meteo_raw |>
  mutate(time = ymd(time), year = year(time), month = month(time)) |>
  filter(month %in% c(3, 4, 5), year > 2014) |>
  group_by(year) |>
  summarize(precip_spr = sum(precip))

# Calculating total adjusted annual precipitation
meteo_adj <- meteo_raw |>
  mutate(time = ymd(time), year = year(time), month = month(time),
         year = ifelse(year %in% c(10, 11, 12), year + 1, year)) |>
  filter(year < 2025 & year > 2014) |>
  group_by(year) |>
  summarize(precip_adj = sum(precip)) 

# Combining yearly and monthly precipitation data
meteo <- meteo_spr |>
  full_join(meteo_adj)


# Cleaning species presence data -----------------------------------------------

# Calculating different measures of plant-specific biomass
scam <- biomass |>
  filter(species_code == "SCAM") |>
  group_by(transect, plot) |>
  mutate(lag_scam = lag(total_biomass),
         change_scam = total_biomass - lag_scam,
         log_scam = ifelse(total_biomass != 0, log(total_biomass), 0),
         lag_change_scam = lag(change_scam),
         loag_scam = ifelse(lag_scam != 0, log(lag_scam), 0),
         change_log_scam = log_scam - loag_scam,
         change_loag_scam = lag(change_log_scam)) |>
  select(year, transect, plot, change_scam, loag_scam, 
         lag_change_scam, lag_scam, change_log_scam, change_loag_scam)

phau <- biomass |>
  filter(species_code == "PHAU") |>
  group_by(transect, plot) |>
  mutate(lag_phau = lag(total_biomass),
         change_phau = total_biomass - lag_phau,
         lag_change_phau = lag(change_phau),
         log_phau = ifelse(total_biomass != 0, log(total_biomass), 0),
         loag_phau = ifelse(lag_phau != 0, log(lag_phau), 0),
         change_log_phau = log_phau - loag_phau,
         change_loag_phau = lag(change_log_phau)) |>
  select(year, transect, plot, lag_phau, change_phau, loag_phau, 
         lag_change_phau, change_log_phau, change_loag_phau)

ivfr <- biomass |>
  filter(species_code == "IVFR") |>
  group_by(transect, plot) |>
  mutate(lag_ivfr = lag(total_biomass),
         change_ivfr = total_biomass - lag_ivfr,
         lag_change_ivfr = lag(change_ivfr),
         loag_ivfr = ifelse(lag_ivfr != 0, log(lag_ivfr), 0),
         log_ivfr = ifelse(total_biomass != 0, log(total_biomass), 0),
         change_log_ivfr = log_ivfr - loag_ivfr,
         change_loag_ivfr = lag(change_log_ivfr)) |>
  select(year, transect, plot, lag_ivfr, change_ivfr, loag_ivfr, 
         lag_change_ivfr, change_log_ivfr, change_loag_ivfr)

# Combining species presence data
species_pres <- scam |>
  full_join(phau) |>
  full_join(ivfr)

# Combining all data sets ------------------------------------------------------

# Combining all data 
master <- biomass |>
  full_join(species_pres) |>
  full_join(fld) |>
  full_join(sea_level) |>
  full_join(meteo)

# Save a 'master' file containing all of our variables
save(master, file = "processed_data/master.RData")


