# Load packages
library(tidyverse)
library(ggplot2)
library(stringr)
library(leaflet)

# Load data
elev_raw1 <- read_csv("raw_data/nov21.csv")
elev_raw2 <- read_csv("raw_data/nov20.csv")
salinity_raw <- readxl::read_xlsx("raw_data/Salinity survey 2025.xlsx")
elev2015_raw <- read_csv("raw_data/gcrew_elevation.csv")
load("processed_data/master.RData")
coords_raw <- read_csv("raw_data/site_coordinates.csv")

biomass <- master |>
  select(year, transect, plot, total_biomass, species_code) |>
  rename(Transect = transect, Plot = plot) |>
  unique() |>
  pivot_wider(names_from = c(species_code, year),
              names_sep = "_", values_from = total_biomass) |>
  mutate(IVFR_change = IVFR_2024 - IVFR_2015,
         SCAM_change = SCAM_2024 - SCAM_2015,
         PHAU_change = PHAU_2024 - PHAU_2015,
         Transect = tolower(Transect))
  
elev <- elev_raw1 |>
  full_join(elev_raw2) |>
  filter(Category %in% c("G1", "G2", "g3")) |>
  mutate(Transect = str_split_i(Name, "-", 1), 
         Plot = str_split_i(Name, "-", 2),
         Note = case_when(Plot == "11c" ~ "replicate",
                          Plot == "13outside" ~ "outside plot",
                          Plot == "10r" ~ "replicate",
                          TRUE ~ NA),
         Plot = as.numeric(case_when(Plot %in% c("11b", "11c") ~ "11",
                                     Plot == "13outside" ~ "13",
                                     Plot == "10r" ~ "10",
                                     TRUE ~ Plot))) |>
  select(Transect, Plot, Elevation, Note) |>
  group_by(Transect, Plot) |>
  summarize(Elevation = mean(Elevation))

salinity <- salinity_raw |>
  mutate(Transect = tolower(Transect)) |>
  select(Transect, Plot, Salinity_PSU)

elev2015 <- elev2015_raw |>
  filter(measurement_type == "TMON vegetation plot") |>
  rename(Transect = transect_id, Plot = plot_id) |>
  group_by(Transect, Plot) |>
  summarize(Elevation2015 = mean(elevation)) |>
  full_join(elev) |>
  mutate(Elev_change = Elevation - Elevation2015) 

coords <- coords_raw |>
  # Keeping only plots of interest
  filter(site == "GCREW", plot != 20, !(plot == 6 & transect == "G3"))  |>
  mutate(transect = tolower(transect)) |>
  rename(Transect = transect, Plot = plot)

all <- elev2015 |>
  full_join(salinity) |>
  full_join(biomass) |>
  full_join(coords)

biomass_bin <- master |>
  select(year, transect, plot, total_biomass, species_code) |>
  rename(Transect = transect, Plot = plot) |>
  unique() |> 
  filter(year == 2015) |>
  mutate(present_2015 = case_when(total_biomass > 0 ~ "Y",
                                  total_biomass == 0 ~ "N",
                                  TRUE ~ NA),
         Transect = tolower(Transect)) |>
  ungroup() |>
  select(!c(year, total_biomass)) |>
  left_join(elev2015)

biomass_bin24 <- master |>
  select(year, transect, plot, total_biomass, species_code) |>
  rename(Transect = transect, Plot = plot) |>
  unique() |> 
  filter(year == 2024) |>
  mutate(present_2024 = case_when(total_biomass > 0 ~ "Y",
                                  total_biomass == 0 ~ "N",
                                  TRUE ~ NA),
         Transect = tolower(Transect)) |>
  ungroup() |>
  select(!c(year, total_biomass)) |>
  left_join(elev2015)

ggplot(aes(x = Salinity_PSU), data = all) +
  geom_density()

ggplot(aes(x = Elevation), data = all) + 
  geom_density()

ggplot(aes(x = Elevation, y = Salinity_PSU, color = Transect), data = all) +
  geom_point() 

ggplot(aes(x = Elevation2015, y = Elevation, color = Salinity_PSU), data = all) +
  geom_point() +
  scale_color_viridis_c() +
  geom_abline() +
  egg::theme_article()

ggplot(aes(x = PHAU_change, y = Elev_change, color = Transect), data = all) +
  geom_point()+
  geom_label(aes(label = Plot))

ggplot(aes(x = SCAM_20, y = Elev_change), data = all) +
  geom_point()

ggplot(aes(x = present_2024, y = Elev_change), data = biomass_bin24) +
  #geom_point() +
  geom_boxplot()+
  facet_wrap(~species_code) +
  geom_jitter(width = 0.1)

# MAPS =========================================================================

all_long <- all |>
  pivot_longer(starts_with("IVFR") | starts_with("SCAM") | starts_with("PHAU"),
               names_to = c("species_code", "year"), names_sep = "_", 
               values_to = "total_biomass") |>
  mutate(tp = paste(Transect, ":", Plot))

# Plotting lot year vs. total biomass for each species, colored
# by elevation
mid <- median(elev2015$Elev_change)
ggplot(all_long, aes(x = year, y = total_biomass, color = Elev_change)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method ='lm', se = F, aes(group = tp), alpha = 0.5) +
    facet_wrap(~species_code, scales = "free") +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    labs(y = "Total Biomass (g)\n", x = "\nYear", 
         color = "Elevation Change") +
    scale_color_gradient2(low = "red", mid = "grey", high = "blue", 
                          midpoint = mid)
    
most <- all |>
  filter(!((Transect == "g3" & Plot == 6) | (Plot == 20)))

# Creating color palette for map. Note the colors in this palette are equally 
# spaced, unlike in the previous plot
palette_elev <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                             domain = c(min(most$Elevation), 
                                        max(most$Elevation)))

# Creating a map of measured plots, colored by elevation in 2025
leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(group = "OSM (default)") |>
  setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
  # Plot coloring depends on elevation
  addCircleMarkers(data = most, 
                   lng = ~longitude, lat = ~latitude, weight = 1.5,
                   radius = 7, fillColor = ~palette_elev(Elevation), 
                   fillOpacity = 0.9, color = "black") |>
  addLegend(position = "bottomright",
            pal = palette_elev, values = most$Elevation, 
            title = "Elevation 2025<br>(m, NAVD88)", 
            opacity = 1) |>
  leafletOptions(resolutions = 1500) 

# Creating a map of measured plots, colored by change in Elevation
palette_elev_change <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                             domain = c(min(most$Elev_change), 
                                        max(most$Elev_change)))

leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(group = "OSM (default)") |>
  setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
  # Plot coloring depends on elevation
  addCircleMarkers(data = most, 
                   lng = ~longitude, lat = ~latitude, weight = 1.5,
                   radius = 7, fillColor = ~palette_elev_change(Elev_change), 
                   fillOpacity = 0.9, color = "black") |>
  addLegend(position = "bottomright",
            pal = palette_elev_change, values = most$Elev_change, 
            title = "Elevation Change<br>(m, NAVD88)", 
            opacity = 1) |>
  leafletOptions(resolutions = 1500) 

# Creating a map of measured plots, colored by salinity
palette_sal <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                                    domain = c(min(most$Salinity_PSU, na.rm = T), 
                                               max(most$Salinity_PSU, na.rm = T)))

leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(group = "OSM (default)") |>
  setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
  # Plot coloring depends on elevation
  addCircleMarkers(data = most, 
                   lng = ~longitude, lat = ~latitude, weight = 1.5,
                   radius = 7, fillColor = ~palette_sal(Salinity_PSU), 
                   fillOpacity = 0.9, color = "black") |>
  addLegend(position = "bottomright",
            pal = palette_sal, values = most$Salinity_PSU, 
            title = "Salinity<br>", 
            opacity = 1) |>
  leafletOptions(resolutions = 1500) 

# Creating a map of measured plots, colored by change in PHAU biomass
palette_phau_change <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                                    domain = c(min(most$PHAU_change, na.rm = T), 
                                               max(most$PHAU_change, na.rm = T)))

leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(group = "OSM (default)") |>
  setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
  # Plot coloring depends on elevation
  addCircleMarkers(data = most, 
                   lng = ~longitude, lat = ~latitude, weight = 1.5,
                   radius = 7, fillColor = ~palette_phau_change(PHAU_change), 
                   fillOpacity = 0.9, color = "black") |>
  addLegend(position = "bottomright",
            pal = palette_phau_change, values = most$PHAU_change, 
            title = "PHAU Change<br>", 
            opacity = 1) |>
  leafletOptions(resolutions = 1500) 

pp <- ggplot(aes(x = Elevation2015, y = Elev_change), data = all) +
  geom_point()

plotly::ggplotly(pp)

ggplot(aes(x = PHAU_change, y = PHAU_change), data = all) +
  geom_point()




