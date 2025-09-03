# In this R script, I clean up external data sets, and merge them with my
# existing files. 

# Loading packages -------------------------------------------------------------
library(tidyverse)
library(lmerTest)
library(VulnToolkit)
library(stringr)

# Reading in data --------------------------------------------------------------
load("processed_data/complete.Rdata")
salinity_raw <- read_csv("raw_data/porewater.csv")
elevation_raw <- read_csv("raw_data/gcrew_elevation.csv")
meteo_raw <- read_csv("raw_data/open_meteo.csv")

# Cleaning salinity data -------------------------------------------------------
salinity_tmp <- salinity_raw |>
  janitor::clean_names() |>
  filter((co2 == 0 & nitrogen == 0), depth == 20, year >= 2015, 
         salinity != -99) |>
  select(year, month, chamber, salinity) |>
  mutate(month = factor(month)) |>
  # Calculating sen's slope to test if salinity significantly changes over time
  group_by(chamber) |>
  arrange(year) |>
  mutate(ch_trend = trend::sens.slope(salinity)$estimates[1],
         ch_p = trend::sens.slope(salinity)$p.value[1]) |>
  group_by(month, chamber) |>
  arrange(year) |>
  mutate(month_trend = trend::sens.slope(salinity)$estimates[1],
         month_p = trend::sens.slope(salinity)$p.value[1]) 

# Grouping by either chamber or chamber and month, salinity does not sig. change 
# over time. However, random year-to-year variation may still be important

# Scatterplot of salinity over time
ggplot(salinity_tmp, aes(x = year, y = salinity, color = month)) +
  geom_point() +
  facet_wrap("chamber")

# Linear mixed effects model to test if salinity differs by month, accounting 
# for the random effects of year-to-year and chamber variation

smonth_lmer <- lmer(salinity ~ month + (1 | year) + (1 | chamber), 
                    data = salinity_tmp)
summary(smonth_lmer)

# Checking assumptions of model
# Homoscedasticity
plot(fitted(smonth_lmer), resid(smonth_lmer),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Normality of residuals
qqnorm(resid(smonth_lmer))
qqline(resid(smonth_lmer), col = "red")

# Normality of random intercepts
qqnorm(ranef(smonth_lmer)$chamber[[1]], main = "Chamber")
qqline(ranef(smonth_lmer)$chamber[[1]], col = "red")

qqnorm(ranef(smonth_lmer)$year[[1]], main = "Year")
qqline(ranef(smonth_lmer)$year[[1]], col = "red")

# Conclusion: Salinity does differ by month. I cannot group all the values
# together. I will instead just use May salinity over time, since that is the 
# only month that appears in every year

salinity <- salinity_tmp |>
  ungroup() |>
  filter(month == "May") |>
  group_by(year) |>
  summarize(salinity = mean(salinity)) 

# Merging with complete data set
complete <- complete |>
  full_join(salinity, by = "year")

# Cleaning sea level & elevation data ------------------------------------------

# Scraping NOOA hourly water level data
water_level_raw <- noaa(begindate = 20140801, enddate = 20240801, 
                      station = "8575512", datum = "NAVD", interval = "hourly", 
                      time = "LST") |>
  janitor::clean_names()

# Cleaning water level data
water_level_tmp <- water_level_raw |>
  mutate(year = year(time_lst), month = month(time_lst), day = day(time_lst),
         # To join with elevation data later
         join = 1) |>
  rename(water = verified_water_level_at_8575512_meters_rel_to_navd) 

high <- water_level_tmp |>
  # Extracting high tides
  group_by(year, month, day) |>
  mutate(high = max(water)) |>
  ungroup() |>
  select(year, high) |>
  distinct() 
  
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

# Calculating flooding duration and depth
spr_tmp <- water_level_tmp |>
  filter(month %in% c(3, 4, 5)) |>
  full_join(elevation, by = "join", relationship = "many-to-many") |>
  group_by(year, elevation) |>
  mutate(fld_dur_spr = fld.dur(elevation, water),
         fld_depth_spr = fld.depth(water, elevation, percentile = 0.5),
         fld_frq_spr = fld.frq(elevation, high$high)) |>
  select(year, transect, plot, elevation, fld_dur_spr, fld_depth_spr, fld_frq_spr) |>
  distinct()

# Merging with complete data set
complete <- complete |>
  full_join(spr_tmp, by = c("year", "transect", "plot"))

# Scraping NOAA monthly water level data
sea_level_raw <- noaa(begindate = 20150101, enddate = 20241231, 
                      station = "8575512", datum = "NAVD", interval = "monthly",
                      time = "LST") |>
  janitor::clean_names()

# Calculating avgs in the spring growing season
sea_level_spr <- sea_level_raw |>
  filter(month >= 3 & month <= 5) |>
  select(year, msl) |>
  group_by(year) |>
  summarize(msl = mean(msl)) 

# Calculating annual averages
sea_level_yr <- sea_level_raw |>
  select(year, msl) |>
  group_by(year) |>
  summarize(sd_msl = sd(msl, na.rm = T), msl = mean(msl))

# Combining yearly and monthly water level data
sea_level <- sea_level_spr |>
  full_join(sea_level_yr, by = "year") |>
  rename_with(~ str_replace(., "\\.x$", "_yr"), ends_with(".x")) |>
  rename_with(~ str_replace(., "\\.y$", "_spr"), ends_with(".y"))
  
# Merging with complete data set
complete <- complete |>
  full_join(sea_level, by = "year") 

# Cleaning meteorological data --------------------------------------------------

# Calculating spring precipitation, mean temp
spr <- meteo_raw |>
  mutate(time = ymd(time), year = year(time), month = month(time)) |>
  filter(month %in% c(3, 4, 5), year > 2014) |>
  group_by(year) |>
  summarize(precip_spr = sum(precip_sum),
            mean_temp_spr = mean(mean_temp))

yr <- meteo_raw |>
  mutate(month = month(time), 
         year = year(time)) |>
  group_by(year) |>
  summarize(precip_yr = sum(precip_sum), mean_temp_yr = mean(mean_temp), 
            sd_temp = sd(mean_temp)) 


# Merging with complete data set
complete <- complete |>
  full_join(spr, by = "year") |>
  full_join(yr, by = "year")

# Cleaning species presence data -----------------------------------------------
# Finding biomass from the previous year
scam <- complete |>
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

phau <- complete |>
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

ivfr <- complete |>
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

# Calculating how much biomass changes each year
change <- complete |>
  full_join(scam) |>
  full_join(phau) |>
  full_join(ivfr) |>
  group_by(transect, plot, species_code) |>
  mutate(previous = lag(total_biomass), change = total_biomass - previous,
         lag_change = lag(change),
         # Log transformations of biomass
         # A biomass of 0 is kept as 0 (can't take log(0))
         log_bio = case_when(total_biomass != 0 ~ log(total_biomass),
                             total_biomass == 0 ~ 0)) |>
  select(!previous) 


# Removing variables no longer needed
master <- change |>
  select(-c(month, day, flag_cover))

# Categorizing PHAU presence using % Cover--------------------------------------

phau_presence <- complete |>
  filter(species_code == "PHAU") |>
  mutate(fractional_cover = case_when(total_biomass == 0 ~ 0,
                                      TRUE ~ fractional_cover),
         phau_cover = case_when(fractional_cover > 0.5 ~ "dominant",
                                TRUE ~ "trace/none")) |>
  ungroup() |>
  select(year, transect, plot, phau_cover)

master <- master |>
  full_join(phau_presence) |>
  ungroup()

interest <- master |>
  select(year, transect, plot, species_code, total_biomass, log_bio, lag_phau, 
         loag_phau, lag_scam, loag_scam, lag_ivfr, loag_ivfr, change, 
         change_phau, change_scam, change_ivfr, lag_change_phau, lag_change_scam,
         lag_change_ivfr, elevation, fld_dur_spr, change_log_ivfr, change_log_phau,
         change_log_scam, change_loag_scam, change_loag_ivfr, change_loag_phau,
         msl_spr, precip_spr)

  
# Save a 'master' file containing all of our variables
save(master, file = "processed_data/master.RData")

# Save an 'interest' file containing just the primary variables of interest
save(interest, file = "processed_data/interest.RData")



