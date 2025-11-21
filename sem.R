# Loading packagaes ------------------------------------------------------------
library(tidyverse)
library(lavaan)
library(lavaanPlot)
load("processed_data/master.Rdata")

# PHAU -------------------------------------------------------------------------

phau <- master |>
  group_by(transect, plot) |>
  filter(species_code == "PHAU") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_phau != 0) |>
  ungroup() |>
  # Limiting the number of variables considered
  select(change_log_phau, loag_phau, fld_dur_spr)

phau_sc <- cov(phau)
phau_sc  
phau_reg <- 'change_log_phau ~ fld_dur_spr + loag_phau
             fld_dur_spr ~~ 0*loag_phau'
phau_fit <- sem(phau_reg, data = phau)
summary(phau_fit, fit.measures = TRUE)
lavaanPlot(name = "phau_fit", phau_fit, coefs = TRUE)

# SCAM -------------------------------------------------------------------------
scam <- master |>
  group_by(transect, plot) |>
  filter(species_code == "SCAM") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_scam != 0) |>
  ungroup() |>
  # Limiting the number of variables considered
  select(change_log_scam, loag_scam, loag_phau, fld_dur_spr) |>
  drop_na()

scam_sc <- cov(scam)
scam_sc  
scam_mod <- 'change_log_scam ~ fld_dur_spr + loag_scam + loag_phau
             loag_phau ~~ loag_scam' 
scam_fit <- sem(scam_mod, data = scam)
summary(scam_fit, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(name = "scam_fit", scam_fit, coefs = TRUE)

scam_scaled <- apply(scam, MARGIN = 2, scale)
scam_fit_scaled <- sem(scam_mod, data = scam_scaled)
summary(scam_fit_scaled, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(name = "scam_fit_scaled", scam_fit_scaled, coefs = TRUE)

scam_mod2 <- 'change_log_scam ~ fld_dur_spr + loag_phau'
scam_fit_scaled2 <- sem(scam_mod2, data = scam_scaled)
summary(scam_fit_scaled2, fit.measures = TRUE, standardized = TRUE)

AIC(scam_fit, scam_fit_scaled, scam_fit_scaled2)

# IVFR -------------------------------------------------------------------------
ivfr <- master |>
  group_by(transect, plot) |>
  filter(species_code == "IVFR") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_ivfr != 0) |>
  ungroup() |>
  # Limiting the number of variables considered
  select(change_log_ivfr, loag_scam, loag_phau, loag_ivfr, fld_dur_spr) |>
  drop_na()

ivfr_sc <- cov(ivfr)
ivfr_sc  
ivfr_reg <- 'change_log_ivfr ~ fld_dur_spr + loag_scam + loag_phau + loag_ivfr
             loag_phau ~~ loag_scam
             loag_phau ~~ loag_ivfr
            loag_scam ~~ loag_ivfr' 
ivfr_fit <- sem(ivfr_reg, data = ivfr)
summary(ivfr_fit, fit.measures = TRUE)
lavaanPlot(name = "ivfr_fit", ivfr_fit, coefs = TRUE)
