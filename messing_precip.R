# Loading packagaes
library(tidyverse)
library(trend)
library(stats)

# Reading in data
meteo_raw <- read_csv("open_meteo_data.csv") |>
  janitor::clean_names() 

biomass_raw <- read_csv("01b_processed_data/all_biomass.csv")

# Cleaning data 
meteo <- meteo_raw |>
  mutate(year = year(time), month = month(time)) |>
  select(year, month, precip_sum, rain_sum) |>
  filter(month %in% c(3, 4, 5)) |>
  group_by(year) |>
  summarize(precip = mean(precip_sum), rain = mean(rain_sum))

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
  arrange(transect, plot, species_code, year)

# Combining biomass and precipitation data sets
biomet <- meteo |>
  full_join(biomass, by = "year") |>
  # Creating a lag of precipitation
  group_by(transect, plot, species_code) |>
  mutate(previous = lag(precip), twoprev = lag(previous)) |>
  ungroup() 

# Graphing precip over time
ggplot(meteo, aes(x = year, y = precip)) + 
  geom_point() +
  geom_line()

# Does precip increase over time? No. 
tst <- meteo |>
  drop_na(precip) |>
  mutate(trend = round(sens.slope(precip)$estimates[1], 3),
         p = round(sens.slope(precip)$p.value[1], 3))

sum <- biomass |>
  group_by(year, species_code) |>
  summarize(total_biomass = sum(total_biomass)) |>
  ungroup() |>
  full_join(meteo, by = "year") |>
  group_by(species_code) |>
  mutate(previous = lag(precip), twoprev = lag(previous)) |>
  ungroup() 

# # Calculating sens slope of precip and biomass
# sens <- biomet |>
#   drop_na(precip) |>
#   group_by(species_code) |>
#   arrange(precip) |>
#   mutate(trend = round(sens.slope(total_biomass)$estimates[1], 3),
#          p = round(sens.slope(total_biomass)$p.value[1], 3)) |>
#   filter(p < 0.05)
# 
# # Finding the plots with at least one significant relationship (convoluted)
# tmp <- sens |>
#   mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
#   group_by(transect, plot) |>
#   summarize(ptmp = sum(p)) |>
#   filter(ptmp >= 10)
# 
# # Plots with significant changes only
# sig <- sens |>
#   inner_join(tmp, by = c("transect", "plot")) |>
#   select(-ptmp)
# 
# 
# # Graphing just the plots with significant changes
# sig |>
#   ggplot(aes(x = precip, y = total_biomass, color = species_code)) +
#   geom_point() + 
#   geom_line() +
#   facet_wrap(c("transect", "plot"), scales = "free")

# SPEARMANS / KENDALL

spearken <- sum |>
  filter(species_code == "SCAM") |>
  select(precip, total_biomass) |>
  drop_na() 

spearken2 <- biomet |>
  filter(species_code == "PHAU", year == 2015) |>
  select(precip, total_biomass) |>
  drop_na()


  cor(spearken, method = "spearman") 
  cor(spearken, method = "kendall")
  
  cor.test(spearken$precip, spearken$total_biomass, method = "spearman")
  cor.test(spearken$precip, spearken$total_biomass, method = "kendall")
  
 lm <- lm(total_biomass ~ twoprev, data = biomet)
 summary(lm)






