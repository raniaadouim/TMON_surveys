library(tidyverse)
library(openmeteo)
library(trend)

# Reading in data
biomass_raw <- read_csv("01b_processed_data/all_biomass.csv") 

# Manually creating a data frame on annual sea level (Annapolis tide gauge)
tide_gauge <- data.frame(year = 2015:2024,
                         sea_lvl = c(7128, 7154, 7171, 7190, 7235, 7211, 7196, 7181,
                                     7256, 7283)) 

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

biomass_tide <- tide_gauge |>
  full_join(biomass, by = "year")

# Graphing biomass over time 
ggplot(biomass, aes(x = year, y = total_biomass, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

# Graphing just sea level over time
ggplot(tide_gauge, aes(x = year, y = sea_lvl)) + 
  geom_point() +
  geom_line()

# Graphing sea level vs. biomass in all plots
ggplot(biomass_tide, aes(x = sea_lvl, y = total_biomass, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")


# Collapsing transects
biomass_tc <- biomass_tide |>
  group_by(transect, year, species_code) |>
  summarize(total_biomass = sum(total_biomass),
            sea_lvl = mean(sea_lvl)) 

# Sum biomass in each transect. collapsed plots
ggplot(biomass_tc, aes(x = sea_lvl, y = total_biomass, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap("transect")

# Calculating sens slope significance
bt_sens <- biomass_tide |>
  group_by(transect, plot, species_code) |>
  arrange(sea_lvl) |>
  mutate(trend = round(sens.slope(total_biomass)$estimates[1], 3),
         p = round(sens.slope(total_biomass)$p.value[1], 3)) |>
  filter(p < 0.05)

# Finding the plots with at least one significant relationship (convoluted)
temp <- bt_sens |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Plots with significant changes only
significant_tide <- bt_sens |>
  inner_join(temp, by = c("transect", "plot")) |>
  select(-ptmp)

# Graphing just the plots with significant changes in 
significant_tide |>
  ggplot(aes(x = sea_lvl, y = total_biomass, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")

# Calculating % change in biomass. A change from 0 to nonzero is a 100% change
# This removes 2015 since there's no previous year to compare
percent_bio <- biomass |>
  mutate(previous = lag(total_biomass),
         percent_change = case_when(previous != 0 ~
                                      round((total_biomass - previous) / previous, 3),
                                    total_biomass == 0 ~ 0,
                                    TRUE ~ 1)) 

percent_bio_tide <- tide_gauge |>
  full_join(percent_bio, by = "year") |>
  drop_na()

# Calculating sens slope significance
pbt_sens <- percent_bio_tide |>
  select(!c(year, total_biomass, previous)) |>
  group_by(transect, plot, species_code) |>
  arrange(sea_lvl) |>
  mutate(trend = round(sens.slope(percent_change)$estimates[1], 3),
         p = round(sens.slope(percent_change)$p.value[1], 3)) |>
  filter(p < 0.05)

# Calculating % change in biomass using 2015 as a baseline
temp_original <- biomass |>
  filter(year == 2015) |>
  select(!year) |>
  rename(original = total_biomass)

percent_bio_tide_new <- biomass |>
  full_join(temp_original, by = c("transect", "plot", "species_code")) |>
  mutate(percent_change = case_when(original != 0 ~
                                      round((total_biomass - original) / original, 3),
                                    total_biomass == 0 ~ 0,
                                    TRUE ~ 1)) |>
  full_join(tide_gauge, by = "year") |>
  drop_na()

# Calculating sens slope significance
pbtn_sens <- percent_bio_tide_new |>
  select(!c(year, total_biomass, original)) |>
  group_by(transect, plot, species_code) |>
  arrange(sea_lvl) |>
  mutate(trend = round(sens.slope(percent_change)$estimates[1], 3),
         p = round(sens.slope(percent_change)$p.value[1], 3)) |>
  filter(p < 0.05)

# Finding the plots with at least one significant relationship (convoluted)
temp_perc <- pbtn_sens |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Plots with significant changes only
significant_tide_perc <- pbtn_sens |>
  inner_join(temp_perc, by = c("transect", "plot")) |>
  select(-ptmp)

# Graphing just the plots with significant changes in percent cover
significant_tide_perc |>
  ggplot(aes(x = sea_lvl, y = percent_change, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"), scales = "free")







# Notes comparing sea lvl to growth in trans-plot
# 22-23 huge increase in sea level
# G1 2 - 16, 19, G2 7, 8, 9, G3 3, 4, 5 (all they show up in)  - decreases in SPAA, DISP
# G1 6, 9, 12, 16, G2 8, 9: small decreases in SCAM
# G1 13, 17, 18, 19 : either small or huge increases in SCAM
# G1 3, 7, 13, G3 3, 4: decrease in iva. 
# G1 2, 6, 8, G2 7 increase in iva
# G1 1, 11, G2 1, G3 1, 2, 3 increase in frag
# G2 2, 3, 4, 5, 6, 10, 11 decrease in frag



# Notes comparing annual sea lvl to growth, collapsed transect:
# G2: 2020 dip in sea level -> huge jump in frag; 2021 dip in sea lvl, increase in frag,
# ***2022-2023 huge increase in sea level -> huge decrease in frag***. s23-24, small increase in
# sea lvl, huge increase in frag
# G2: 22-23: huge increase in sea level -> huge decrease in sppa, disp. small decrease in scam.
# increase in IVA