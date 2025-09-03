# Loading packagaes ------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(MASS)
load("processed_data/interest.Rdata")

# AIC Fitting ------------------------------------------------------------------

phau <- interest |>
  group_by(transect, plot) |>
  filter(species_code == "PHAU") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_phau != 0) |>
  ungroup() |>
  dplyr::select(change_log_phau, loag_phau, loag_scam, loag_ivfr, 
                fld_dur_spr, year) |>
  drop_na() |>
  mutate(fld_dur_spr = 100 * fld_dur_spr)

scam <- interest |>
  group_by(transect, plot) |>
  filter(species_code == "SCAM") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_scam != 0) |>
  ungroup() |>
  dplyr::select(change_log_scam, loag_phau, loag_scam, loag_ivfr, 
                fld_dur_spr, year) |>
  drop_na()

ivfr <- interest |>
  group_by(transect, plot) |>
  filter(species_code == "IVFR") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_ivfr != 0) |>
  ungroup() |>
  dplyr::select(change_log_ivfr, loag_phau, loag_scam, loag_ivfr, 
                fld_dur_spr, year) |>
  drop_na()

# PHAU +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
phau_aic <- lm(change_log_phau ~ ., data = phau)
stepAIC(phau_aic, direction = "both")

phau_mod <- lm(change_log_phau ~ loag_phau + loag_scam + loag_ivfr +
                 fld_dur_spr, data = phau)
summary(phau_mod)

# Removing insignificant variables
phau_mod2 <- lm(change_log_phau ~ loag_phau + fld_dur_spr, data = phau)
summary(phau_mod2)

# Nested F-Test - no important info is missing
anova(phau_mod2, phau_mod, phau_aic) 

# Calculating VIFs to test for multicollinearity - no issues
car::vif(phau_mod2)

# Plotting PHAU ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Redoing to drop fewer NAs 
phau2 <- interest |>
  group_by(transect, plot) |>
  filter(species_code == "PHAU") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_phau != 0) |>
  ungroup() |>
  dplyr::select(change_log_phau, loag_phau, fld_dur_spr, elevation) |>
  mutate(categ = case_when(elevation < 0.18 ~ "low", 
                           elevation > 0.271 ~ "high", 
                           TRUE ~ "mid")) |>
  drop_na() |>
  mutate(fld_dur_spr = 100 * fld_dur_spr) 
  
fld <- interest |>
  ungroup() |>
  dplyr::select(fld_dur_spr) |>
  summarize(med = median(fld_dur_spr, na.rm = T))


phau2 |>
  ggplot(aes(x = loag_phau, y = change_log_phau)) +
  geom_point(size = 3) + 
  egg::theme_article() +
  labs(x = expression(paste("Previous Year's PHAU Biomass (",
                            log[10], " g)")), 
       y = expression(paste("Change in PHAU Biomass (", log[10], " g)")),  
       color = "Spring\nFlooding\nDuration (%)",
       title = "P. australis") +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.7) +
  theme(
    plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    legend.position = "right",
    axis.title.x = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    strip.text = element_text(size = 14, face = "bold.italic"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.height = unit(0.8, "cm")) +
  scale_color_gradient2(low = "purple", mid = "darkgray", high = "green", 
                        midpoint = 33.12) 


  
# 450, 600




# SCAM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
scam_aic <- lm(change_log_scam ~ ., data = scam)
stepAIC(scam_aic, direction = "both")

scam_mod <- lm(change_log_scam ~ loag_phau + loag_scam + fld_dur_spr, data = scam)
summary(scam_mod)

# Removing insignificant variables
scam_mod2 <- lm(change_log_scam ~ loag_phau + loag_scam, data = scam)
summary(scam_mod2)

# Nested F-Test - no important info is missing in scam_mod2
anova(scam_mod2, scam_mod, scam_aic) 

# Calculating VIFs to test for multicollinearity - no issues
car::vif(scam_mod2)

# Redoing to drop fewer NAs 
scam2 <- interest |>
  group_by(transect, plot) |>
  filter(species_code == "SCAM") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_scam != 0) |>
  ungroup() |>
  dplyr::select(change_log_scam, loag_phau, loag_scam, elevation) |>
  mutate(categ = case_when(elevation < 0.18 ~ "low", 
                           elevation > 0.271 ~ "high", 
                           TRUE ~ "mid")) |>
  drop_na() 

loagp <- interest |>
  ungroup() |>
  dplyr::select(loag_phau) |>
  summarize(mean = mean(loag_phau, na.rm = T))


scam2 |>
  ggplot(aes(x = loag_scam, y = change_log_scam)) +
  geom_point(size = 3, alpha = 0) + 
  egg::theme_article() +
  labs(x = expression(paste("Previous Year's SCAM Biomass (",
                            log[10], " g)")), 
       y = expression(paste("Change in SCAM Biomass (", log[10], " g)")),
       color = expression(paste("Prev. Yr's\nPHAU Biomass\n(log    g)")),
       title = "S. americanus") +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.7) +
  theme(
    plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    legend.position = "right",
    axis.title.x = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    axis.title.y = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold.italic"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.height = unit(0.8, "cm")) +
  scale_color_gradient2(low = "purple", mid = "darkgray", high = "green", 
                        midpoint = 2.42) 

# IVFR +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ivfr_aic <- lm(change_log_ivfr ~ ., data = ivfr)
stepAIC(ivfr_aic, direction = "both")

ivfr_mod <- lm(change_log_ivfr ~ loag_phau + loag_ivfr, data = ivfr)
summary(ivfr_mod)

# Nested F-Test - no important info is missing in ivfr_mod
anova(ivfr_mod, ivfr_aic) 

ivfr2 <- interest |>
  group_by(transect, plot) |>
  filter(species_code == "IVFR") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, change_log_ivfr != 0) |>
  ungroup() |>
  dplyr::select(change_log_ivfr, loag_phau, loag_ivfr, elevation) |>
  drop_na()

loagp <- interest |>
  ungroup() |>
  dplyr::select(loag_phau) |>
  summarize(mean = mean(loag_phau, na.rm = T))


ivfr2 |>
  ggplot(aes(x = loag_ivfr, y = change_log_ivfr, color = loag_phau)) +
  geom_point(size=3) + 
  egg::theme_article() +
  labs(x = expression(paste("Previous Year's IVFR Biomass (",
                            log[10], " g)")), 
       y = expression(paste("Change in IVFR Biomass (", log[10], " g)")),  
       color = expression(paste("Prev. Yr's\nPHAU Biomass\n(log   g)")),
       title = "I. frutescens") +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.7) +
  theme(
    plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    axis.title.x = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold.italic"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.height = unit(0.8, "cm")) +
  scale_color_gradient2(low = "purple", mid = "darkgray", high = "green", 
                        midpoint = 2.42) 
