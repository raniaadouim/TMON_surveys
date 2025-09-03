# Loading packagaes ------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(MASS) # step()

# Reading in data --------------------------------------------------------------
load("processed_data/interest.RData")

# Creating species subsets -----------------------------------------------------

subsets <- interest |>
  dplyr::select(-c(year, transect, plot, total_biomass, log_bio, 
            fld_depth_spr))

phau <- subsets |>
  filter(species_code == "PHAU") |>
  dplyr::select(change_scam, change_ivfr, salinity, 
                fld_dur_spr, fld_frq_spr, msl_spr, precip_spr, 
                lag_phau, lag_scam, lag_ivfr, lag_change_phau, change_phau)

phau_na <- phau |>
  drop_na()

scam <- subsets |>
  filter(species_code == "SCAM") |>
  dplyr::select(change_scam, change_ivfr, salinity, 
                fld_dur_spr, fld_frq_spr, msl_spr, precip_spr, mean_temp,
                lag_phau, lag_scam, lag_ivfr, lag_change_scam, change_phau)  

scam_na <- scam |>
  drop_na()

ivfr <- subsets |>
  filter(species_code == "IVFR") |>
  dplyr::select(change_scam, change_ivfr, salinity, 
                fld_dur_spr, fld_frq_spr, msl_spr, precip_spr, mean_temp,
                lag_phau, lag_scam, lag_ivfr, lag_change_ivfr, change_phau)  

ivfr_na <- ivfr |>
  drop_na()

set.seed(1019)

# AIC Fitting ------------------------------------------------------------------

# PHAU +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
phau_aic <- lm(change_phau ~ ., data = phau_na)
stepAIC(phau_aic, direction = "both")

phau_mod <- lm(change_phau ~ loag_phau + lag_change_phau + lag_scam + fld_dur_spr, 
               data = phau)
summary(phau_mod)

# Nested F-Test - no important info is missing
anova(phau_mod, phau_aic) # Have to drop_nas in mod to run

# Calculating VIFs to test for multicollinearity - no issues
car::vif(phau_mod)


# SCAM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
scam_aic <- lm(change_scam ~ ., data = scam_na)
stepAIC(scam_aic, direction = "both")

scam_mod <- lm(change_scam ~ lag_scam + lag_change_scam + change_phau +
                 lag_phau + msl_spr + fld_dur_spr, data = scam)
summary(scam_mod)

# Nested F-Test - no important info is missing
anova(scam_mod, scam_aic)

# Calculating VIFs to test for multicollinearity - no issues
car::vif(scam_mod)


# IVFR +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ivfr_aic <- lm(change_ivfr ~ ., data = ivfr_na)
stepAIC(ivfr_aic, direction = "both")

ivfr_mod <- lm(change_ivfr ~ msl_spr + precip_spr + lag_ivfr + lag_phau, 
               data = ivfr)
summary(ivfr_mod)

# Only lag_ivfr is significant - this actually increases adj. R^2
ivfr_mod2 <- lm(change_ivfr ~ lag_ivfr, data = ivfr)
summary(ivfr_mod2)

# Nested F-Test - no important info is missing
anova(ivfr_mod2, ivfr_aic)
