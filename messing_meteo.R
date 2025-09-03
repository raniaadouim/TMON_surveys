# Messing around with data on biomass and meteorology

# Loading in packages
library(tidyverse)
library(lubridate)
library(trend)

# Reading in data
biomass_raw <- read_csv("01b_processed_data/all_biomass.csv") 
meteo_raw <- read_csv("raw_data/open_meteo_data.csv") |>
  janitor::clean_names() |>
  rename(soil_temp_28_100 = soil_temperature_28_to_100cm_mean_c)
  
# Cleaning and trimming data
# Handling duplicate allometry/harvest measurements
dupes <- biomass_raw |>
  select(year, transect:species_code, total_biomass, method) |>
  filter(species_code %in% c("SPPA", "DISP", "PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  drop_na() |>
  filter(n() > 1,
         (species_code %in% c("DISP", "SPPA") & method == "harvest") |
           species_code %in% c("PHAU", "IVFR", "SCAM") & method == "allometry") |>
  select(!method)

# Cleaning biomass data 
biomass <- biomass_raw |>
  select(year, transect:species_code, total_biomass) |>
  filter(species_code %in% c("SPPA", "DISP", "PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  drop_na() |>
  filter(n() == 1) |>
  full_join(dupes, by = c("year", "transect", "plot", "species_code", 
                          "total_biomass")) |>
  mutate(total_biomass = round(total_biomass, 3)) |>
  group_by(transect, plot, species_code) |>
  complete(year = 2015:2024,
           fill = list(total_biomass = 0)) |>
  drop_na(species_code) |>
  group_by(species_code, year) |>
  complete(transect = "G1", plot = 1:20, fill = list(total_biomass = 0)) |>
  complete(transect = "G2", plot = 1:11, fill = list(total_biomass = 0)) |>
  complete(transect = "G3", plot = 1:6, fill = list(total_biomass = 0)) |>
  arrange(transect, plot, species_code, year)

# Combining biomass and elevation data 
bio_elev_2 <- biomass |>
  full_join(elevation_2, by = c("transect", "plot")) 
bio_elev_3 <- biomass |>
  full_join(elevation_3, by = c("transect", "plot")) |>
  mutate(rank = factor(rank, levels = c("low", "med", "high")))
bio_elev_3e <- biomass |>
  full_join(elevation_3e, by = c("transect", "plot")) |>
  mutate(rank = factor(rank, levels = c("low", "med", "high")))

# Calculating annual averages
meteo_annual <- meteo_raw |>
  mutate(year = year(time), month = month(time)) |>
  relocate(c(year, month), .after = time) |>
  select(!time) |>
  group_by(year) |>
  summarize(across(2:11, mean))

# Joining with biomass data
biomass_meteo_annual <- biomass |>
  full_join(meteo_annual, by = "year")
  
# Graph variables against total biomass
ggplot(biomass_meteo_annual, aes(x = precip_sum, y = total_biomass, 
                 color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

# Calculating sens slope
bma_sens <- biomass_meteo_annual |>
  select(transect, plot, species_code, precip_sum, total_biomass) |>
  group_by(transect, plot, species_code) |>
  arrange(precip_sum) |>
  mutate(trend = round(sens.slope(total_biomass)$estimates[1], 3),
         p = round(sens.slope(total_biomass)$p.value[1], 3)) |>
  filter(p < 0.05)

# Finding the plots with at least one significant relationship (convoluted)
temporary <- bma_sens |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Plots with significant changes only
significant_meteo <- bma_sens |>
  inner_join(temporary, by = c("transect", "plot")) |>
  select(-ptmp)

# Graphing just the plots with significant changes
significant_meteo |>
  ggplot(aes(x = precip_sum, y = total_biomass, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

# Plotting just the x variable over time
ggplot(meteo_annual, aes(x = year, y = mean_temp)) + 
  geom_point() +
  geom_line()

# Calculating monthly averages
meteo_monthly <- meteo_raw |>
  mutate(year = year(time), month = month(time)) |>
  relocate(c(year, month), .after = time) |>
  select(!time) |>
  group_by(year, month) |>
  summarize(across(2:10, mean)) |>
# Filtering out for the months when data was collected
  filter(((year == 2015 | year == 2018) & month == 10) | (year == 2016 & month == 8) |
           ((year %in% c(2017, 2019, 2020, 2021, 2022, 2023, 2024)) & month == 9))

# Joining with biomass data
biomass_meteo_monthly <- biomass |>
  full_join(meteo_monthly, by = "year")

# Calculating annual averages, not by calendar year but since last survey
meteo_annual_new <- meteo_raw |>
  mutate(year = year(time), month = month(time), day = day(time)) |>
  relocate(c(year, month, day), .after = time) |>
  select(!time) |>
  mutate(order= case_when(
    year == 2015 & month <= 10 ~ 2015,
    (year == 2015 & month > 10) | (year == 2016 & month <= 8) ~ 2016,
    (year == 2016 & month > 8) | (year == 2017 & month <= 9) ~ 2017,
    (year == 2017 & month > 9) | (year == 2018 & month <= 10) ~ 2018,
    (year == 2018 & month > 10) | (year == 2019 & month <= 9) ~ 2019,
    (year == 2019 & month > 9) | (year == 2020 & month <= 10) ~ 2020,
    (year == 2020 & month > 10) | (year == 2021 & month <= 9) ~ 2021,
    (year == 2021 & month > 9) | (year == 2022 & month <= 9) ~ 2022,
    (year == 2022 & month > 9) | (year == 2023 & month <= 9) ~ 2023, 
    (year == 2023 & month > 9) | (year == 2024 & month <= 9) ~ 2024
  )) |>
  select(!c(year, month, day)) |>
  group_by(order) |>
  summarize(across(1:10, mean)) |>
  drop_na() |>
  rename(year = order)

# Joining with biomass data
biomass_meteo_yr <- biomass |>
  full_join(meteo_annual_new, by = "year")


# Calculating sens slope
bmay_sens <- biomass_meteo_yr |>
  select(transect, plot, species_code, rain_sum, total_biomass) |>
  group_by(transect, plot, species_code) |>
  arrange(rain_sum) |>
  mutate(trend = round(sens.slope(total_biomass)$estimates[1], 3),
         p = round(sens.slope(total_biomass)$p.value[1], 3)) |>
  filter(p < 0.05)

# Finding the plots with at least one significant relationship (convoluted)
temporary <- bmay_sens |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Plots with significant changes only
significant_meteo <- bmay_sens |>
  inner_join(temporary, by = c("transect", "plot")) |>
  select(-ptmp)

# Graphing just the plots with significant changes in percent cover
significant_meteo |>
  ggplot(aes(x = rain_sum, y = total_biomass, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")


  



