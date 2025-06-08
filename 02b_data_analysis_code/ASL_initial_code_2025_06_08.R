## Initial data analysis

#Set up repo
#This folder does not get pushed to GitHub
if(!file.exists("03a_figures")){dir.create("03a_figures")} 

#Load packages
library(tidyverse)
library(ggh4x) #To customize plots
library(trend) #for Sen's slope

#Load data
cover <- read_csv("01b_processed_data/plant_species_cover.csv")
other <- read_csv("01b_processed_data/other_environmental_cover.csv") %>%
  pivot_longer(cols = bare_ground:wrack,
               names_to = "species_code", values_to = "fractional_cover"
               )
bio <- read_csv("01b_processed_data/all_biomass.csv")

#Combine plant cover with other data
cover <- cover %>%
  bind_rows(other)

#Plot
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SPPA", "SCAM", "PHAU", "IVFR", "DISP", "SPCY"),
                               species_code,
                               "OTHER")) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/sum) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("PHAU")
         ) %>%
  ggplot(aes(x = year, y = fractional_cover, color = species_code)) +
  geom_line()+
  ggh4x::facet_nested_wrap(~transect+plot)+
  theme(axis.title.x = element_blank())

strips <- strip_nested(
  background_x = list(element_rect(fill = "white", color = c("grey20")),
                      element_blank()),
  text_x = elem_list_text(color = c("black", "grey50"),
                          size = c(NA, 7),
                          face = c("italic", NA),
                          margin = list(ggplot2::margin(t = 2, b = 1,0,0, unit = "pt"), NULL)),
  by_layer_x = TRUE,
  size = "variable")

jpeg("03a_figures/SPPA.jpg", res = 300, width = 6, height = 7.5, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SPPA"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("SPPA"),
         !is.na(fractional_cover)) %>%
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  mutate(trend = trend::sens.slope(fractional_cover)$estimates[1],
         p = trend::sens.slope(fractional_cover)$p.value[1],
         color = ifelse(trend >= 0, 
                        "Increasing",
                        "Decreasing"),
         color = ifelse(p > 0.05, 
                        "Insignificant",
                        color)) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = color))+
  geom_point(size = 0.5, aes(color = color)) +
  ylab(expression(italic("Spartina patens")~"percent cover"))+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  scale_color_manual(values = c("Increasing" = "green4",
                                "Decreasing" = "darkred",
                                "Insignificant" = "grey"),
                     name = "Trend")+
  ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank(),
        legend.position = c(.8, .05))+
  force_panelsizes(rows = unit(.75, "in"),
                   cols = unit(1, "in"))
dev.off()

jpeg("03a_figures/Rise of PHAU.jpg", res = 300, width = 6, height = 5, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SCAM", "PHAU", "IVFR", "DISP", "SPCY"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("PHAU")) %>%
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  mutate(trend = trend::sens.slope(fractional_cover)$estimates[1],
         p = trend::sens.slope(fractional_cover)$p.value[1],
         color = ifelse(trend >= 0, 
                        "Increasing",
                        "Decreasing"),
         color = ifelse(p > 0.05, 
                        "Insignificant",
                        color)) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = color))+
  geom_point(size = 0.5, aes(color = color)) +
  ylab(expression(italic("Phragmites australis")~"percent cover"))+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  scale_color_manual(values = c("Increasing" = "green4",
                                "Decreasing" = "darkred",
                                "Insignificant" = "grey"),
                     name = "Trend")+
  ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank(),
        legend.position = c(.92,0.1))+
  force_panelsizes(rows = unit(.75, "in"),
                   cols = unit(1, "in"))
dev.off()

jpeg("03a_figures/IVFR.jpg", res = 300, width = 6, height = 6.5, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SCAM", "PHAU", "IVFR", "DISP", "SPCY"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("IVFR")) %>%
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  mutate(trend = trend::sens.slope(fractional_cover)$estimates[1],
         p = trend::sens.slope(fractional_cover)$p.value[1],
         color = ifelse(trend > 0, 
                        "Increasing",
                        "Decreasing"),
         color = ifelse(p > 0.05, 
                        "Insignificant",
                        color)) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = color))+
  geom_point(size = 0.5, aes(color = color)) +
  ylab(expression(italic("Iva frutescens")~"percent cover"))+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  scale_color_manual(values = c("Increasing" = "green4",
                                "Decreasing" = "darkred",
                                "Insignificant" = "grey"),
                     name = "Trend")+
  ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank(),
        legend.position = c(.8, .05))+
  force_panelsizes(rows = unit(.75, "in"),
                   cols = unit(1, "in"))
dev.off()

jpeg("03a_figures/SCAM.jpg", res = 300, width = 6, height = 7, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SCAM", "PHAU", "IVFR", "DISP", "SPCY"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("SCAM")) %>%
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  mutate(trend = trend::sens.slope(fractional_cover)$estimates[1],
         p = trend::sens.slope(fractional_cover)$p.value[1],
         color = ifelse(trend > 0, 
                        "Increasing",
                        "Decreasing"),
         color = ifelse(p > 0.05, 
                        "Insignificant",
                        color)) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = color))+
  geom_point(size = 0.5, aes(color = color)) +
  ylab(expression(italic("Schoenoplectus americanus")~"percent cover"))+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  scale_color_manual(values = c("Increasing" = "green4",
                                 "Decreasing" = "darkred",
                                 "Insignificant" = "grey"),
                     name = "Trend")+
  ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank(),
        legend.position = c(.8, 0.05))+
  force_panelsizes(rows = unit(.75, "in"),
                   cols = unit(1, "in"))
dev.off()

jpeg("03a_figures/SPCY.jpg", res = 300, width = 6, height = 3, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SCAM", "PHAU", "IVFR", "DISP", "SPCY"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("SPCY")) %>%
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  mutate(trend = trend::sens.slope(fractional_cover)$estimates[1],
         p = trend::sens.slope(fractional_cover)$p.value[1],
         color = ifelse(trend > 0, 
                        "Increasing",
                        "Decreasing"),
         color = ifelse(p > 0.05, 
                        "Insignificant",
                        color)) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = color))+
  geom_point(size = 0.5, aes(color = color)) +
  ylab(expression(italic("SPCY")~"percent cover"))+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  scale_color_manual(values = c("Increasing" = "green4",
                                "Decreasing" = "darkred",
                                "Insignificant" = "grey"),
                     name = "Trend")+
  ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank(),
        legend.position = "bottom")+
  force_panelsizes(rows = unit(.75, "in"),
                   cols = unit(1, "in"))
dev.off()

jpeg("03a_figures/All.jpg", res = 300, width = 8, height = 10, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SCAM", "PHAU", "IVFR"),
                               "SCAM/PHAU/IVFR",
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/sum) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("SCAM/PHAU/IVFR")) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = species_code))+
  geom_point(size = 0.5, aes(color = species_code))+
  ylab(expression("Percent cover"))+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  ggh4x::facet_nested_wrap(~transect+plot, strip = strips, ncol = 5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank())+
  force_panelsizes(rows = unit(.75, "in"),
                   cols = unit(1, "in"))
dev.off()

jpeg("03a_figures/Mult.jpg", res = 300, width = 5, height = 6, units = "in")
cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SPPA", "SCAM", "PHAU", "IVFR", "DISP", "SPCY", "open_water"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/sum) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         !species_code %in% c("OTHER")
         ) %>%
  ggplot(aes(x = year, y = fractional_cover*100)) +
  geom_line(aes(color = plot))+
  geom_point(size = 0.5, aes(color = plot))+
  ylab("Percent cover")+
  scale_x_continuous(breaks = seq(2017, 2022, 4),
                     minor_breaks = seq(2015, 2024, 1))+
  facet_grid(species_code~transect)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        axis.title.x = element_blank())
dev.off()

length(unique(cover$species_code))

bio %>%
  filter(method == "allometry",
    species_code %in% c("SCAM"), 
         transect %in% c("G1", "G2", "G3")
    ) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-", day))) %>%
  ggplot(aes(x = Date, y = total_biomass, color = species_code)) +
  geom_line()+
  facet_grid(transect~plot)

bio %>%
  filter(plot %in% 1:10,
         method == "allometry",
         transect %in% c("G1", "G2", "G3")) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(total_biomass, na.rm = T),
         species_code = ifelse(species_code %in% c("SPPA", "SCAM", "PHAU", "IVFR", "SOSE"),
                               species_code,
                               "OTHER")
        ) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fract_biomass = sum(total_biomass, na.rm = T)/unique(sum)) %>%
  ggplot(aes(x = year, y = fract_biomass, fill = species_code)) +
  geom_col()+
  facet_grid(transect~plot)

unique(bio$year)

cover %>%
  group_by(plot, transect) %>%
  complete(species_code, year, fill = list(fractional_cover = 0)) %>%
  mutate(species_code = ifelse(species_code %in% c("SCAM", "IVFR", "PHAU"),
                               species_code,
                               "OTHER"),
         plot = paste0("Plot ", plot),
         plot = factor(plot, levels = paste0("Plot ", 1:20))) %>%
  group_by(year, transect, plot) %>%
  mutate(sum = sum(fractional_cover, na.rm = T)) %>%
  group_by(year, transect, plot, species_code) %>%
  summarize(fractional_cover = sum(fractional_cover)/unique(sum)) %>%
  filter(transect %in% c("G1", "G2", "G3"),
         species_code %in% c("SCAM", "IVFR", "PHAU"),
         !is.na(fractional_cover)) %>%
  group_by(transect, plot, species_code) %>%
  arrange(year) %>%
  summarize(trend = trend::sens.slope(fractional_cover)$estimates[1],
            p = trend::sens.slope(fractional_cover)$p.value[1]) %>%
  filter(p < 0.05,
         species_code != "SCAM" | trend > 0) %>%
  group_by(species_code) %>%
  summarize(trend = mean(trend),
            n = n())
