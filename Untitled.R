# Loading packagaes ------------------------------------------------------------
library(randomForest)
library(tidyverse)
library(lme4)

# Reading in data --------------------------------------------------------------
load("processed_data/interest.RData") 
load("processed_data/master.RData") 

vars <- interest |>
  ungroup() |>
  dplyr::select(loag_scam, loag_phau, fld_dur_spr) |>
  names()

rfdata <- interest |>
  ungroup() |>
  group_by(transect, plot) |>
  filter(species_code == "PHAU") |>
  # Removing plots where the plant never shows up
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0) |>
  ungroup() |>
  mutate(tp = paste0(transect, ": ", plot)) |>
  # Only selecting variables of interest
  drop_na() |>
  group_by(tp, species_code) |>
  mutate(change_log = log_bio - loag_phau,
         change_loag = lag(change_log)) |>
  select(year, change_log, change_loag, log_bio, total_biomass, loag_scam, vars) |>
  filter(!change_log == 0)

lm <- lm(data = rfdata %>% filter(!change_log == 0), 
         change_log ~ loag_phau + loag_scam + fld_dur_spr)
summary(lm)

rfdata <- interest |>
  ungroup() |>
  group_by(transect, plot) |>
  filter(species_code == "IVFR") |>
  # Removing plots where the plant never shows up
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0) |>
  ungroup() |>
  mutate(tp = paste0(transect, ": ", plot)) |>
  # Only selecting variables of interest
  drop_na() |>
  group_by(tp, species_code) |>
  mutate(change_log = log_bio - loag_ivfr,
         change_loag = lag(change_log)) |>
  select(year, change_log, change_loag, log_bio, total_biomass, loag_scam, vars, loag_ivfr) |>
  filter(!change_log == 0)

lm <- lm(data = rfdata %>% filter(!change_log == 0), 
         change_log ~ loag_scam + fld_dur_spr + loag_phau + loag_ivfr)
summary(lm)

rfdata %>% filter(!change_log == 0) %>%
  ggplot(aes(x = loag_phau, y = change_log, color = fld_dur_spr))+
  geom_point()

rfdata <- interest |>
  ungroup() |>
  group_by(transect, plot) |>
  filter(species_code == "IVFR") |>
  # Removing plots where the plant never shows up
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0) |>
  ungroup() |>
  mutate(tp = paste0(transect, ": ", plot)) |>
  # Only selecting variables of interest
  drop_na() |>
  group_by(tp, species_code) |>
  mutate(change_log = log_bio - loag_ivfr,
         change_loag = lag(change_log)) |>
  select(year, change_log, change_loag, log_bio, total_biomass, loag_scam, vars) |>
  filter(!change_log == 0)

lm <- lm(data = rfdata, 
         change_log ~ loag_phau + loag_scam + fld_dur_spr + change_loag)
summary(lm)


phau_mod <- lm(change_log ~  lag_change_phau , 
               data = rfdata)
summary(phau_mod)

interest %>%
  ggplot(aes(x = lag_change_phau, y = change_log_phau))+
  geom_point()+
  geom_smooth(method = "lm")

interest %>%
  ggplot(aes(x = lag_change_scam, y = change_scam))+
  geom_point()+
  geom_smooth(method = "lm")

tmp <- interest |>
  mutate(change_log = log_bio - lag(log_bio),
         change_loag = lag(log_bio) - lag(lag(log_bio))) 

  ggplot(data = rfdata, aes(x = change_loag)) +
  geom_histogram()+
  facet_wrap(~species_code, scales = "free")

interest %>%
  ggplot(aes(x = change_phau))+
  geom_density()+
  facet_wrap(~species_code, scales = "free")

interest %>%
  ggplot(aes(x = lag_change_ivfr, y = change_ivfr))+
  geom_point()+
  geom_smooth(method = "lm")

interest %>%
  ggplot(aes(x = lag_change_scam, y = change_scam))+
  geom_point()+
  geom_smooth(method = "lm")

interest %>%
  ggplot(aes(x = lag_change_ivfr, y = change_ivfr))+
  geom_point()+
  geom_smooth(method = "lm")
