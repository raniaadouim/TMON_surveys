# Loading packagaes ------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(GGally)
library(leaps)
require(leaps)
library(HH)
library(stringr)

# Reading in data --------------------------------------------------------------
load("processed_data/interest.RData")

# Initial ggpairs plots --------------------------------------------------------
  
# ggpairs of the non-biomass predictor variables
interest |>
  dplyr::select(-c(species_code, year, transect, plot, total_biomass, log_bio, 
            starts_with("change"), starts_with("lag"), starts_with("loag"))) |>
  ggpairs() 
# Some correlations: fld_dur / fld_depth, mean_temp / soil temp

# ggpairs of the biomass predictors
interest |>
  dplyr::select(starts_with("lag"), starts_with("change"), starts_with("loag")) |>
  ggpairs()

# PHAU subset
phau <- interest |>
  filter(species_code == "PHAU") |>
  dplyr::select(-c(year, transect, plot, species_code, total_biomass, log_bio, 
            fld_depth_spr, st7_28))
# SCAM 
scam <- ggpairs_tmp |>
  filter(species_code == "SCAM") |>
  dplyr::select(-c(year, transect, plot, species_code, total_biomass, log_bio, 
            fld_depth_spr, st7_28))

# IVFR
ivfr <- ggpairs_tmp |>
  filter(species_code == "IVFR") |>
  dplyr::select(-c(year, transect, plot, species_code, total_biomass, log_bio, 
            fld_depth_spr, st7_28))


# Best Subsets Algorithm -------------------------------------------------------

# SIMPLER MODELS (NO INTERACTIONS, LIMITED SPECIES) ||||||||||||||||||||||||||||
# PHAU =========================================================================
# Finding predictors of phau biomass, using the best subsets algorithm
bestphau <- regsubsets(total_biomass ~ ., data = phau, 
                   nbest = 1, nvmax = 12)
summaryHH(bestphau)

# The models with 4-7 predictors have the best adjusted R^2 and CP values
pm4 <- lm(total_biomass ~ fld_dur_yr + fld_frq_yr + lag_phau +
            precip_yr, data = phau)
pm5 <- lm(total_biomass ~ elevation + fld_frq_yr + lag_phau +
            precip_yr + msl_spr, data = phau)
pm6 <- lm(total_biomass ~ salinity + elevation + fld_frq_yr + fld_frq_spr +
            lag_phau + msl_spr, data = phau)
pm7 <- lm(total_biomass ~ salinity + elevation + fld_frq_yr + fld_frq_spr +
            lag_phau + msl_spr + precip_yr, data = phau)
pmfull <- lm(total_biomass ~ ., data = phau)

summary(pm4)
summary(pm5)
summary(pm6)
summary(pm7)
summary(pmfull)

# pm5 has all significant variables, but pm6 has a better adjusted R^2
anova(pm5, pmfull) # Can't run because of dataset sizing issues
anova(pm6, pmfull)


# Calculating VIFs to test for multicollinearity
car::vif(pm5)
car::vif(pm6)

# Added Variable Plots
car::avPlot(pm5, "elevation")
car::avPlot(pm5, "fld_frq_yr")
# elevation has a better AV plot (smaller x-residual margins)
pmtest <- lm(total_biomass ~ elevation + lag_phau +
              precip_yr + msl_spr, data = phau)
summary(pmtry)

car::avPlot(pm6, "fld_frq_spr")
car::avPlot(pm6, "fld_frq_yr")
# fld_frq_spr has a better AV plot (smaller x-residual margins)
pmtest1 <- lm(total_biomass ~ salinity + elevation + fld_frq_spr +
           lag_phau + msl_spr, data = phau)
summary(pmtest1)

car::vif(pmtest1)

# pmtest1 has a better adjusted R^2, and has more significant variables than 
# pmtest

pm1 <- lm(total_biomass ~ lag_phau, data = phau)
summary(pm1) # ~48% explained by just lag_phau

# SCAM =========================================================================
# Finding predictors of scam biomass, using the best subsets algorithm
bestscam <- regsubsets(total_biomass ~ ., data = scam, 
                   nbest = 1, nvmax = 12)
summaryHH(bestscam)

# The models with 5-8 predictors have the best adjusted R^2 and CP values
sm5 <- lm(total_biomass ~ salinity + fld_frq_spr + fld_dur_yr + lag_phau +
            lag_scam, data = scam)
sm6 <- lm(total_biomass ~ salinity + fld_frq_spr + fld_dur_yr + lag_phau +
            lag_scam + fld_dur_spr, data = scam)
sm7 <- lm(total_biomass ~ salinity + fld_frq_spr + fld_dur_yr + lag_phau +
            lag_scam + fld_dur_spr + msl_spr, data = scam)
sm8 <- lm(total_biomass ~ salinity + fld_frq_spr + fld_dur_yr + lag_phau +
            lag_scam + fld_dur_spr + msl_spr + msl_yr, data = scam)
smfull <- lm(total_biomass ~ ., data = scam)

summary(sm5)
summary(sm6)
summary(sm7)
summary(sm8)
summary(smfull)

anova(sm5, sm6, sm7, sm8, smfull)
# sm5 is not missing any important info and has mostly significant variables

# Calculating VIFs to test for multicollinearity
car::vif(sm5)

# Added Variable Plots
car::avPlot(sm5, "fld_frq_spr")
car::avPlot(sm5, "fld_dur_yr")

smtest <- lm(total_biomass ~ salinity + lag_phau +
               lag_scam + fld_dur_yr, data = scam)
summary(smtest)

smtest2 <- lm(total_biomass ~ salinity + lag_phau +
               lag_scam + fld_frq_spr, data = scam)
summary(smtest2)

# smtest2 has more significant variables and a better R^2

sm1 <- lm(total_biomass ~ lag_scam, data = scam)
summary(sm1) # ~56% explained by just lag_scam

# IVFR =========================================================================
# Finding predictors of scam biomass, using the best subsets algorithm
bestivfr <- regsubsets(total_biomass ~ ., data = ivfr, 
                       nbest = 1, nvmax = 12)
summaryHH(bestivfr)

# The models with 3-4 predictors have the best adjusted R^2 and CP values
im3 <- lm(total_biomass ~ lag_ivfr + precip_spr + fld_dur_yr, data = ivfr)
im4 <- lm(total_biomass ~ lag_ivfr + precip_spr + fld_dur_yr + lag_phau,
          data = ivfr)
imfull <- lm(total_biomass ~ ., data = ivfr)

summary(im3)
summary(im4)
summary(imfull)

# im3 has the best adjusted R^2
im1 <- lm(total_biomass ~ lag_ivfr, data = ivfr)
summary(im1) #~44% explained by just lag_ivfr

# SPPA =========================================================================
# Finding predictors of scam biomass, using the best subsets algorithm
bestsppa <- regsubsets(total_biomass ~ ., data = sppa, 
                       nbest = 1, nvmax = 12)
summaryHH(bestsppa)

# The model with 7 predictors has the best adjusted R^2 and CP values (w/ no 
# spr/yr repeats)
spm7 <- lm(total_biomass ~ elevation + fld_frq_spr + fld_dur_yr + msl_spr +
             precip_spr + lag_phau + lag_sppa, data = sppa)
spmfull <- lm(total_biomass ~ ., data = sppa)

summary(spm7)
summary(spmfull)

# Dropping NAs to perform a nested F-test
spm7_x <- spm7 <- lm(total_biomass ~ elevation + fld_frq_spr + fld_dur_yr + 
                       msl_spr + precip_spr + lag_phau + lag_sppa, 
                     data = sppa |> drop_na())
anova(spm7_x, spmfull)

# Calculating VIFs to test for multicollinearity
car::vif(spm7) # Really bad multicollinearity issues

# Added Variable Plots
car::avPlot(spm7, "fld_frq_spr") # Best slope
car::avPlot(spm7, "fld_dur_yr") # Bad
car::avPlot(spm7, "elevation") # Bad
car::avPlot(spm7, "msl_spr")

spmtest <- spm7 <- lm(total_biomass ~ fld_frq_spr + msl_spr +
                        precip_spr + lag_phau + lag_sppa, data = sppa)
summary(spmtest)
spmtest2 <- spm7 <- lm(total_biomass ~ fld_frq_spr +
                        precip_spr + lag_phau + lag_sppa, data = sppa)
summary(spmtest2)

# spmtest2 has a better adjusted R^2 and more significant variables

spm1 <- lm(total_biomass ~ lag_sppa, data = sppa)
summary(spm1) # ~14% explained by just lag_sppa

# MORE COMPLEX MODELS ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

phau2 <- ggpairs_tmp |>
  filter(species_code == "PHAU") |>
  dplyr::select(-c(species_code, change)) 

scam2 <- ggpairs_tmp |>
  filter(species_code == "SCAM") |>
  dplyr::select(-c(species_code, change)) 

sppa2 <- ggpairs_tmp |>
  filter(species_code == "SPPA") |>
  dplyr::select(-c(species_code, change)) 

ivfr2 <- ggpairs_tmp |>
  filter(species_code == "IVFR") |>
  dplyr::select(-c(species_code, change)) 

# PHAU =========================================================================

# Finding predictors of phau biomass, using the best subsets algorithm
bestphau2 <- regsubsets(total_biomass ~ ., data = phau2, 
                       nbest = 1, nvmax = 14)
summaryHH(bestphau2)

# The models with 6-11 predictors have the best adjusted R^2 and CP values
p2m6 <- lm(total_biomass ~ salinity*elevation +  fld_frq_spr + lag_phau +
            precip_yr, data = phau2)
p2m7 <- lm(total_biomass ~ salinity*elevation +  fld_frq_spr + lag_phau +
             precip_yr + lag_scam, data = phau2)
p2m8 <- lm(total_biomass ~ salinity*elevation + fld_frq_spr + lag_phau +
             precip_yr + lag_scam + lag_sppa, data = phau2)
p2m9 <- lm(total_biomass ~ salinity*elevation  + fld_frq_spr + lag_phau +
             precip_yr + lag_scam + lag_sppa + lag_ivfr, data = phau2)
p2m10 <- lm(total_biomass ~ salinity*elevation + fld_frq_yr + lag_phau +
               precip_spr + lag_scam + lag_sppa + fld_dur_spr + msl_spr,
            data = phau2)
p2m11 <- lm(total_biomass ~ salinity*elevation + fld_frq_yr + lag_phau +
              precip_spr + lag_scam + lag_sppa + fld_dur_spr + msl_spr +
              lag_ivfr, data = phau2)
p2mfull <- lm(total_biomass ~ ., data = phau2)

summary(p2m6)
summary(p2m7)
summary(p2m8)
summary(p2m9)
summary(p2m10)
summary(p2m11)
summary(p2mfull)

# p2m8 has the best adjusted R^2

# Dropping NAs to compare models
p2m6_x <- lm(total_biomass ~ salinity*elevation + + fld_frq_spr + lag_phau +
             precip_yr, data = phau2 |> drop_na())
p2m8_x <- lm(total_biomass ~ salinity*elevation + + fld_frq_spr + lag_phau +
               precip_yr + lag_scam + lag_sppa, data = phau2 |> drop_na())
anova(p2m6_x, p2m8_x, p2mfull) 
# p2m6 is not missing any important information, but p2m8 has a better adjusted 
# R^2 and a higher residual standard error. p2m7 has the best residual standard 
# error, medium adjusted R^2

# Calculating VIFs to test for multicollinearity
car::vif(p2m7)

# Added Variable Plots
car::avPlot(p2m7, "elevation")
car::avPlot(p2m7, "salinity:elevation")
# elevation has a better AV plot (smaller x-residual margins)
p2mtest <- lm(total_biomass ~ salinity + elevation + fld_frq_spr + lag_phau +
                precip_yr + lag_scam, data = phau2)
summary(p2mtest)

p2mtest <- lm(total_biomass ~ salinity + se+ fld_frq_spr + lag_phau +
                precip_yr + lag_scam, data = phau2)
summary(p2mtest1)

# p2mtest1 has a better adjusted R^2 and rse, and has more significant variables

# SCAM =========================================================================
# Finding predictors of scam biomass, using the best subsets algorithm
bestscam2 <- regsubsets(total_biomass ~ ., data = scam2, 
                       nbest = 1, nvmax = 12)
summaryHH(bestscam2)

# The models with 4-8 predictors have the best adjusted R^2 and CP values
s2m4 <- lm(total_biomass ~ fld_frq_spr + lag_sppa + lag_phau +
             lag_scam, data = scam2)
s2m5 <- lm(total_biomass ~ fld_frq_spr + precip_yr + lag_sppa + lag_phau +
            lag_scam, data = scam2)
s2m6 <- lm(total_biomass ~ fld_dur_spr + fld_dur_yr + fld_frq_yr + lag_sppa + 
             lag_phau + lag_scam, data = scam2)
s2m7 <- lm(total_biomass ~ fld_dur_yr + fld_dur_spr + fld_frq_yr + precip_yr +
             lag_phau + lag_scam + lag_sppa, data = scam2)
s2m8 <- lm(total_biomass ~ elevation + fld_dur_spr + fld_frq_yr + precip_yr +
             lag_phau + lag_scam + lag_sppa + msl_spr, data = scam2)
s2mfull <- lm(total_biomass ~ ., data = scam2)

summary(s2m4)
summary(s2m5)
summary(s2m6)
summary(s2m7)
summary(s2m8)
summary(s2mfull)

anova(s2m4, s2m5)
# the larger models have few significant variables. in that case, better to 
# stick to s2m4

# Removing lag_sppa increases rse by ~20 and decreases adjusted R^2 by ~ 5%
s2m3 <- lm(total_biomass ~ fld_frq_spr + lag_phau +
             lag_scam, data = scam2)
summary(s2m3)

# Calculating VIFs to test for multicollinearity
car::vif(s2m4)

# move forward with s2m4

# IVFR =========================================================================
# Finding predictors of scam biomass, using the best subsets algorithm
bestivfr2 <- regsubsets(total_biomass ~ ., data = ivfr2, 
                       nbest = 1, nvmax = 12)
summaryHH(bestivfr2)

# The models with 2-4 predictors have the best adjusted R^2 values
i2m2 <- lm(total_biomass ~ lag_ivfr + se, data = ivfr2)
i2m3 <- lm(total_biomass ~ lag_ivfr + lag_scam + se, data = ivfr2)
i2m4 <- lm(total_biomass ~ lag_ivfr + lag_scam + se + msl_spr, data = ivfr2)
i2mfull <- lm(total_biomass ~ ., data = ivfr2)

summary(i2m2)
summary(i2m3)
summary(i2m4)
summary(i2mfull)

# In all these models, only lag_ivfr is a significant predictor

# WEIRD: full model is ~8% better than any other one. Model with just 1 
# predictor is better. Model with just lag_ivfr is basically as good as the full 
# model

# move forward with im1

# SPPA =========================================================================
# Finding predictors of scam biomass, using the best subsets algorithm
bestsppa2 <- regsubsets(total_biomass ~ ., data = sppa2, 
                       nbest = 1, nvmax = 12)
summaryHH(bestsppa2)

# The modesl with 7-8 predictors have the best adjusted R^2 and CP values (w/ no 
# spr/yr repeats)
s2pm7 <- lm(total_biomass ~ elevation + lag_scam + fld_dur_yr + msl_spr +
             precip_spr + lag_phau + lag_sppa, data = sppa2)
s2pm8 <- lm(total_biomass ~ elevation + lag_scam + fld_dur_yr + msl_spr +
              precip_spr + lag_phau + lag_sppa + fld_frq_spr, data = sppa2)
s2pmfull <- lm(total_biomass ~ ., data = sppa2)

summary(s2pm7)
summary(s2pm8)
summary(s2pmfull)

# s2pm8 has the best adjusted R^2 and good variable significance

# Calculating VIFs to test for multicollinearity
car::vif(s2pm8) # Really bad multicollinearity issues

# Added Variable Plots
car::avPlot(s2pm8, "fld_frq_spr") # Best slope
car::avPlot(s2pm8, "fld_dur_yr") # 2nd best
car::avPlot(s2pm8, "elevation") 
car::avPlot(s2pm8, "msl_spr")

s2pmtest <- lm(total_biomass ~ lag_scam + fld_dur_yr + precip_spr + lag_phau + 
                 lag_sppa + fld_frq_spr, data = sppa2)
summary(s2pmtest)

# Removing high VIF variables makes the R^2 go down by about 7%

# CHANGE IN BIOMASS ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

change <- interest |>
  group_by(transect, plot, species_code) |>
  mutate(lag_change = lag(change)) |>
  ungroup() |>
  dplyr::select(-c(transect, plot, year, flag_biomass))

phauc <- change |>
  filter(species_code == "PHAU") |>
  dplyr::select(-c(species_code, total_biomass)) 

scamc <- change |>
  filter(species_code == "SCAM") |>
  dplyr::select(-c(species_code, total_biomass)) 

ivfrc <- change |>
  filter(species_code == "IVFR") |>
  dplyr::select(-c(species_code, total_biomass)) 
# PHAU =========================================================================



# Finding predictors of phau biomass, using the best subsets algorithm
bestphauc <- regsubsets(change ~ ., data = phauc, 
                        nbest = 1, nvmax = 14)
summaryHH(bestphauc)

