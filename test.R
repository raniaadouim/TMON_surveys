
# Loading packages -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggpubr)
library(lubridate)
library(gridExtra)
library(VulnToolkit)
library(leaflet)
library(htmltools)

# Reading in data --------------------------------------------------------------
load("processed_data/master.RData")
load("processed_data/interest.RData")
coords_raw <- read_csv("raw_data/site_coordinates.csv")

elev2 <- master |>
  dplyr::select(year, transect, plot, elevation, species_code, total_biomass,
                change, fld_dur_spr) |>
  mutate(log_elevation = log(elevation)) |>
  filter(!is.na(elevation)) |>
  arrange(desc(elevation)) |>
  mutate(rank = as.factor(case_when(elevation < 0.189 ~ "low", elevation > 0.274 ~ "high",
                          TRUE ~ "med"))) |>
  select(year, transect, plot, rank) 
  

flooding <- master |>
  select(year, species_code, fld_dur_spr, transect, plot, elevation) |>
  group_by(year) |>
  distinct(fld_dur_spr, .keep_all = TRUE) |>
  drop_na(fld_dur_spr) |>
  mutate(tp = paste0(transect, ": ", plot))

ggplot(flooding, aes(x = year, y = fld_dur_spr, group = tp)) +
  geom_point() +
  geom_line(aes(group = tp)) +
  labs(x = "Year", y = "Spring Flooding Duration (%)") +
  egg::theme_article() +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 


phau2 <- master |>
  group_by(transect, plot) |>
  filter(species_code == "SCAM") |>
  # Removing plots where the plant never shows up, and years where biomass goes 
  # from 0 to 0 (aka has not yet appeared)
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  mutate(tp = paste0(transect, ": ", plot)) %>%
  filter(sum > 0, change_log_scam != 0 ) |>
  ungroup() |>
  dplyr::select(year, transect, plot, tp, msl_spr, lag_scam,total_biomass, fractional_cover, change, lag_phau, elevation, loag_phau, loag_scam, change_log_scam, fld_dur_spr) |>
  mutate(categ = case_when(elevation < 0.18 ~ "low", 
                           elevation > 0.271 ~ "high", 
                           TRUE ~ "mid")) |>
  group_by(tp) %>%
  do({
    model <- lm(total_biomass ~ year, data = .)
    slope <- coef(model)[["year"]]
    mutate(., slope = slope)
  }) %>%
  ungroup() |>
 # keep only negative slopes 
left_join(elev2) |>
  filter(species_code == "IVFR") 

testts <- aov(total_biomass ~ rank, data = phau2)
summary(testts)

test1 <- lm(total_biomass ~ elevation + lag_scam, data = phau2)
summary(test1)


fld <- interest |>
  ungroup() |>
  dplyr::select(fld_dur_spr) |>
  summarize(med = median(fld_dur_spr, na.rm = T))


phau2 |>
  ggplot(aes(x = loag_phau, y = change_log_phau, color = rank)) +
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




mod <- aov(total_biomass ~ rank, data = phau2)
summary(mod)

elev_neg <- master %>%
  select(year, transect, plot, elevation, species_code, total_biomass) %>%
  mutate(tp = paste0(transect, ": ", plot)) %>%
  group_by(tp, species_code) %>%
  mutate(sum = sum(total_biomass, na.rm = TRUE)) %>%
  filter(sum != 0) %>%
  ungroup() %>%
  mutate(log_elevation = log(elevation), 
         species_code = factor(species_code, levels = c("PHAU", "SCAM", "IVFR")),
         year = as.numeric(year)) %>%
  group_by(tp, species_code) %>%
  do({
    model <- lm(total_biomass ~ year, data = .)
    slope <- coef(model)[["year"]]
    mutate(., slope = slope)
  }) %>%
  ungroup() %>%
  filter(slope > 0 )  # keep only negative slopes




ggplot(elev_neg |> mutate(year= as.numeric(year)), aes(x = year, y = total_biomass, 
                                                   color = elevation)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(low = "red", mid = "darkgray", high = "blue",
                        midpoint = 0.20555)+
  geom_smooth(method ='lm', se = F, aes(group = tp), alpha = 0.5) +
  facet_wrap(.~species_code, scales = "free", 
             labeller = as_labeller(c("PHAU" = "P. australis",
                                      "SCAM" = "S. americanus",
                                      "IVFR" = "I. frutescens"))) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
  labs(# title = "Change in Plant Biomass Over Time (2015-2024), by Elevation", 
    # subtitle = "Trend lines are fitted by plot",
    y = "Total Biomass (g)\n", x = "\nYear", 
    color = "Elevation\n(m, NAVD88)") +
  egg::theme_article() +
  theme(# plot.title = element_text(size = 16, hjust = 0.5),
    # plot.subtitle = element_text(size = 13, hjust = 0.5),
    strip.text = element_text(size = 15, face = "bold.italic"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.position = "right",
    plot.margin = unit(c(1, 2, 1, 1), "cm"),
    legend.key.height = unit(0.8, "cm"),
    panel.grid.major = element_line(color = "grey90", size = 0.5))
