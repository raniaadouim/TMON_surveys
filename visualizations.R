# In this R script, I create various visualizations for biomass and predictor 
# data. Some data wrangling occurs to prepare the data for these visualizations.

# Loading packages -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggmap)
library(lubridate)
library(gridExtra)
library(VulnToolkit)
library(leaflet)

# Reading in data --------------------------------------------------------------
load("processed_data/master.RData")
coords_raw <- read_csv("raw_data/site_coordinates.csv")

# Cleaning coordinate data -----------------------------------------------------

coords <- coords_raw |>
  # Keeping only plots of interest
  filter(site == "GCREW", plot != 20, !(plot == 6 & transect == "G3")) 

master <- master |>
  full_join(coords)

# Creating static biomass maps -------------------------------------------------

# Creating a function to map bioass for a given species in a given year
mapping_leaflet <- function(species, yr, consistentcolor) {
  
  species_all <- master |>
    filter(species_code == species)
  species_yr <- species_all |>
    filter(year == yr)
    
  # For a given species, all maps can be colored according to the same color
  # scale, for ease of comparison.
  if(consistentcolor == "Y") {
    # Finding maximum biomass to determine color scale range
    max <- max(species_all$total_biomass, na.rm = T)
  } else {
    max <- max(species_yr$total_biomass, na.rm = T)
  }
 
  # Creating color palette
  palette <- colorNumeric(palette = c("#f8fccc", "red"), 
                          domain = c(0, max))
  
  # Separating different kinds of biomass values
  pos <- species_yr |>  
    filter(total_biomass > 0) 
  na <- species_yr |>
    filter(is.na(total_biomass)) 
  zero <- species_yr |>
    filter(total_biomass == 0) 
  
  product <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
    addTiles(group = "OSM (default)") |>
    setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
    # Adding x's for NA values
    addLabelOnlyMarkers(data = na, lng = ~longitude, lat = ~latitude, 
                        label = "x", labelOptions = labelOptions(
                          noHide = TRUE,
                          direction = 'center',
                          textOnly = TRUE,
                          style = list(
                            "color" = "black",
                            "font-size" = "18px",
                            "font-weight" = "bold"))) |>
    # Adding white circles for 0 values
    addCircleMarkers(data = zero, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = "white", 
                     fillOpacity = 0.9, color = "black") |>
    # Adding colored circles for positive values
    addCircleMarkers(data = pos, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = ~palette(total_biomass), 
                     fillOpacity = 0.9, color = "black") |>
    addLegend(position = "bottomright",
              pal = palette, values = c(0, pos$total_biomass), 
              title = "Total<br>Biomass (g)", opacity = 1) |>
    leafletOptions(resolutions = 1500) 
  return(product)
}

# Creating a function to map biomass change for a given species and year range
mapping_leafletchange <- function(species, yr1, yr2) {
  
  biomass_change <- master |>
    filter(species_code == species, year %in% c(yr1, yr2)) |>
    group_by(transect, plot) |>
    mutate(change_yrs = total_biomass - lag(total_biomass)) |>
    filter(year == yr2) 
  
  # Removing NA values, so they don't show up on the legend
  legend <- biomass_change |>
    filter(!is.na(change_yrs))
  
  # Separating different kinds of change in biomass values
  pos <- biomass_change |>
    filter(change_yrs > 0)
  na <- biomass_change |>
    filter(is.na(change_yrs))
  neg <- biomass_change |>
    filter(change_yrs < 0)
  zero <- biomass_change |>
    filter(change_yrs == 0)
  max <- max(pos$change_yrs, na.rm = T)
  
  # Creating color palettes
  palette <- colorNumeric(palette = c("darkred", "white", "darkgreen"), 
                             domain = c(-max, max))
  
  product <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
    addTiles(group = "OSM (default)") |>
    setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
    # Adding x's for NA values
    addLabelOnlyMarkers(data = na, lng = ~longitude, lat = ~latitude, 
                        label = "x", labelOptions = labelOptions(
                          noHide = TRUE,
                          direction = 'center',
                          textOnly = TRUE,
                          style = list(
                            "color" = "black",
                            "font-size" = "18px",
                            "font-weight" = "bold"))) |>
    # Adding white circles for 0 values
    addCircleMarkers(data = zero, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = "white", 
                     fillOpacity = 0.9, color = "black") |>
    # Adding colored circles
    addCircleMarkers(data = pos, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = ~palette(change_yrs), 
                     fillOpacity = 0.9, color = "black") |>
    addCircleMarkers(data = neg, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = ~palette(change_yrs), 
                     fillOpacity = 0.9, color = "black") |>
    addLegend(position = "bottomright",
              pal = palette, values = legend$change_yrs, 
              title = "Change in<br>Biomass (g)", opacity = 1) |>
    leafletOptions(resolutions = 1500) 
    return(product)
}


# Creating a function to make standard maps for a given species 

standardmaps <- function(species, yr1, yr2, consistentcolor) {
  
  # Calling functions to make individual maps
  map_1 <- mapping_leaflet(species, yr1, consistentcolor)
  map_2 <- mapping_leaflet(species, yr2, consistentcolor)
  map_change <- mapping_leafletchange(species, yr1, yr2)
  
  # Labeling the individual maps
  labeled_1 <- tags$div(
  tags$h3(paste0("", yr1), style = "text-align: center; 
          font-size: 20px; font-family: Helvetica; font-style: normal"), map_1)
  
  labeled_2 <- tags$div(
    tags$h3(paste0("", yr2), style = "text-align: center; 
          font-size: 20px; font-family: Helvetica; font-style: normal"), map_2)
  
  labeled_change <- tags$div(
    tags$h3(paste0("Change from ", yr1, " to ", yr2), style = "text-align: center; 
          font-size: 20px; font-family: Helvetica; font-style: normal"), map_change)
  
  # Combining all the individual maps into one row
  combined <- tags$div(
    # Need the title in a different div for different formatting
    tags$div(style = "width: 100%;",
             # Title
             tags$h2(paste0("", species), 
             style = "text-align: center; font-family: Helvetica;
             font-style: italic; font-size: 18pt; margin-bottom: 18px; ")),
    tags$div(style = "width: 100%; display: flex; flex-direction: row; gap: 45px;
             align-items: center; justify-content: center;",
             tags$div(style = "flex: 1 1 0;", labeled_1),
             tags$div(style = "flex: 1 1 0;", labeled_2),
             tags$div(style = "flex: 1 1 0;", labeled_change)))
  
  return(combined)
}

# Calling functions to make standard maps
phau_maps <- standardmaps("PHAU", 2015, 2024, "Y")
browsable(phau_maps)
ivfr_maps <- standardmaps("IVFR", 2015, 2024, "Y")
browsable(ivfr_maps)
scam_maps <- standardmaps("SCAM", 2015, 2024, "Y")
browsable(scam_maps)

# Creating animated maps with satellite background -----------------------------

# Registering my Google API
register_google(key = "")
# API is left off this script for privacy

# Retrieving satellite image
satellite <- get_googlemap(center = c(lon = -76.546114, lat = 38.875756),
                           zoom = 16, size = c(400, 400), maptype = "satellite")

animated <- function(species) {
  
  tmp <- master |>
    filter(species_code == species) 
  # Finding the maximum biomass valueto create a consistent color scale
  max <- max(tmp$total_biomass, na.rm = T)
  
    # Separating different kinds of biomass values
    pos <- tmp |>  
      filter(total_biomass > 0) 
    na <- tmp |>
      filter(is.na(total_biomass)) 
    zero <- tmp |>
      filter(total_biomass == 0) 
    
    # Plotting map
    static <- ggmap(satellite) +
      geom_point(data = pos, aes(x = longitude, y= latitude, 
                                 fill = total_biomass), size = 2.5, 
                 shape = 21) +
      geom_point(data = na, aes(x = longitude, y = latitude), color = "black",
                 shape = 4, stroke = 1.5) + 
      geom_point(data = zero, aes(x = longitude, y = latitude), 
                 fill = "white", size = 1.5, shape = 21) +
      scale_fill_gradient(low = "#f9ffb8", high = "red", limits = c(0, max),
                          name = "biomass") +
      scale_size_continuous(range = c(2.5, 6.5)) +
      guides(size = "none") + 
      theme(axis.title = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), panel.grid = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            legend.key.size = unit(0.5, "cm"),
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 7)) 
    
    # Adding animation
    animated <- static +
      shadow_mark() + transition_time(master$year) +
      ggtitle('Year: {frame_time')
  
    return(animated)
  
}


# Plotting Environmental Drivers over time -------------------------------------
  
  # Finding historical mean sea level values
  threshold_msl_raw <- noaa(begindate = 19840101, enddate = 20141231, 
                            station = "8575512", datum = "NAVD", 
                            interval = "monthly", time = "LST") |>
    janitor::clean_names() 
  
  threshold_msl_spr <- threshold_msl_raw |>
    filter(month %in% c(3, 4, 5)) |>
    group_by(year) |>
    summarize(msl_spr = mean(msl, na.rm = TRUE)) |>
    ungroup() |>
    summarize(med_msl_spr = median(msl_spr), max_msl_spr = max(msl_spr)) |>
    # For joining with other threshold datasets later
    mutate(join = 1)
  
  # Combining msl thresholds from the spring and adjusted annual periods
  threshold_msl <- threshold_msl_raw |>
    mutate(year = ifelse(month >= 10, year + 1, year)) |>
    filter(year > 1984 & year < 2025) |>
    group_by(year) |>
    summarize(msl_adj = mean(msl, na.rm = TRUE)) |>
    ungroup() |>
    summarize(med_msl_adj = median(msl_adj), max_msl_adj = max(msl_adj)) |>
    mutate(join = 1) |>
    full_join(threshold_msl_spr) 
  
  # Finding historical precipitation values
  threshold_meteo_raw <- read_csv("raw_data/open_meteoyr.csv") |>
    janitor::clean_names()
  
  threshold_meteo_spr <- threshold_meteo_raw |>
    mutate(time = ymd(time), year = year(time), month = month(time)) |>
    filter(month %in% c(3, 4, 5)) |>
    group_by(year) |>
    summarize(precip_spr = sum(precip)) |>
    ungroup() |>
    summarize(med_precip_spr = median(precip_spr), max_precip_spr = max(precip_spr)) |>
    mutate(join = 1) 
  
  # Combining precip. thresholds from the spring and adjusted annual periods
  threshold_meteo <- threshold_meteo_raw |>
    mutate(time = ymd(time), year = year(time), month = month(time)) |>
    filter(year > 1984 & year < 2025) |>
    group_by(year) |>
    summarize(precip_adj = sum(precip)) |>
    ungroup() |>
    summarize(med_precip_adj = median(precip_adj, max_precip_adj = max(precip_adj))) |>
    mutate(join = 1) |>
    full_join(threshold_meteo_spr) 
  
  # Combining all threshold values
  thresholds <- threshold_msl |>
    full_join(threshold_meteo) |>
    select(!join)

drivers <- master |>
  select(year, precip_spr, precip_adj, msl_spr, msl_adj) |>
  distinct(year, .keep_all = TRUE) |>
  pivot_longer(cols = c(precip_spr, precip_adj, msl_spr, msl_adj),
               names_to = c("variable", "season"),
               names_sep = "_",
               values_to = "value") 

drivers_plot <- drivers |>
  filter(season == "adj")|>
  ggplot(aes(x = year, y = value)) +
  geom_point() + 
  geom_line() +
  egg::theme_article() +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
  labs(title = "Annual Environmental Drivers (2015-2024)",
    subtitle = "with historical benchmarks, reflecting 1985–2014 median and 
    maximum annual values\n",
    x = "Year", y = "Mean Sea Level (m, NAVD88)", linetype = "Benchmark") +
  theme(plot.title = element_text(size = 15, hjust = 0.5), 
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        legend.position = "top",
        axis.title.x = element_text(size = 14, hjust = 0.5),
        axis.title.y = element_text(size = 14, hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold.italic"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
  
drivers_plot


# Plotting elevation data ------------------------------------------------------

elev_species <- master |>
  select(year, transect, plot, elevation, species_code, total_biomass,
         longitude, latitude) |>
  mutate(tp = paste0(transect, ": ", plot)) |>
  # For each species, removing plots where the plant doesn't show up at all 
  group_by(tp, species_code) |>
  mutate(sum = sum(total_biomass, na.rm = T)) |>
  filter(sum != 0) |>
  ungroup() |>
  mutate(species_code = factor(species_code, levels = c("PHAU", "SCAM", "IVFR")))
  
elevation <- master |>
  ungroup() |>
  select(transect, plot, elevation, longitude, latitude) |>
  distinct() |>
  drop_na(elevation)

# Plotting elevation density
ggplot(elevation, aes(x = elevation)) +
  geom_density() +
  labs(x = "Elevation (m, NAVD88)", y = "Density") + 
  egg::theme_article() + 
  theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey90", size = 0.5))

# Determining elevation midpoints for color scale
mid <- median(elevation$elevation)

# Creating a function to plot year vs. total biomass for each species, colored
# by elevation
elev_biomass <- function(species) {
  product <- ggplot(elev |> mutate(year= as.numeric(year)) |> 
                      filter(species_code == species), 
                    aes(x = year, y = total_biomass, color = elevation)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method ='lm', se = F, aes(group = tp), alpha = 0.5) +
    facet_wrap(.~species_code, scales = "free", 
               labeller = as_labeller(c("PHAU" = "P. australis",
                                        "SCAM" = "S. americanus",
                                        "IVFR" = "I. frutescens"))) +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
    labs(y = "Total Biomass (g)\n", x = "\nYear", 
         color = "Elevation\n(m, NAVD88)") +
    scale_color_gradient2(low = "red", mid = "grey", high = "blue", 
                          midpoint = mid) +
    egg::theme_article() +
    theme(strip.text = element_text(size = 15, face = "bold.italic"),
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
  return(product)
}


# Creating static elevation map ------------------------------------------------


# Creating color palette for map. Note the colors in this palette are equally 
# spaced, unlike in the previous plot
palette_elev <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                            domain = c(min(elevation$elevation), 
                                       max(elevation$elevation)))

# Creating a map of measured plots, colored by elevation
elev_map_tmp <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(group = "OSM (default)") |>
  setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
  # Plot coloring depends on elevation
  addCircleMarkers(data = elevation, lng = ~longitude, lat = ~latitude, weight = 1.5,
                   radius = 7, fillColor = ~palette_elev(elevation), 
                   fillOpacity = 0.9, color = "black") |>
  addLegend(position = "bottomright",
            pal = palette_elev, values = elevation$elevation, 
            title = "Elevation<br>(m, NAVD88)", 
            opacity = 1) |>
  leafletOptions(resolutions = 1500) 

# Formatting and labelling map
elev_map <- tags$div(
  # Need the title in a different div for different formatting
  tags$div(style = "width: 100%;",
           # Title
           tags$h2("Elevation", style = "text-align: center; font-family: Helvetica;
                   font-size: 18pt; margin-bottom: 18px; ")),
  tags$div(style = "width: 100%; display: flex; flex-direction: row; gap: 45px;
           align-items: center; justify-content: center;",
           tags$div(style = "flex: 1 1 0;", elev_map_tmp)))
browsable(elev_map)

# Visualizing flooding duration ------------------------------------------------ 

flooding <- master |>
  select(year, species_code, fld_dur_spr, transect, plot, elevation) |>
  group_by(year) |>
  distinct(fld_dur_spr, .keep_all = TRUE) |>
  drop_na(fld_dur_spr) |>
  mutate(tp = paste0(transect, ": ", plot))

# Plotting spring flooding duration in each transect-plot, over time
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
        axis.text.y = element_text(size = 12)) +
  geom_text(data = flooding_extreme, aes(label = tp), vjust = -0.65, hjust = 0.8,
            size = 2.5)

# Plotting summarized biomass vs. biomass --------------------------------------

# Calculating yearly average and total biomass for each species
avgbios <- master |>
  select(year, species_code, total_biomass) |>
  filter(!is.na(total_biomass)) |>
  group_by(species_code, year) |>
  summarize(avg_bio = mean(total_biomass),
            sum_bio = sum(total_biomass)) |>
  mutate(species_code = factor(species_code, levels = c("PHAU", "SCAM", "IVFR")))

# Visualizing how total biomass has changed over time
ggplot(avgbios, aes(x = year, y = sum_bio)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species_code, scales = "free") +
  labs(x = "Year", y = "Total Biomass (g)") +
  egg::theme_article() +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey90", size = 0.5)) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024))

# Visualizing how average biomass of different species compare
ggplot(avgbios, aes(x = avg_phau, y = avg_scam)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  egg::theme_article() +
  labs(title = "Annual Avg Biomass of PHAU vs. SCAM", x = "Avg PHAU biomass",
       y = "Avg SCAM biomass")

ggplot(avgbios, aes(x = avg_phau, y = avg_ivfr)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  egg::theme_article() +
  labs(title = "Annual Avg Biomass of PHAU vs. IVFR", x = "Avg PHAU biomass",
       y = "Avg IVFR biomass")

ggplot(avgbios, aes(y = avg_ivfr, x = avg_scam)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  egg::theme_article() +
  labs(title = "Annual Avg Biomass of SCAM vs. IVFR", y = "Avg IVFR biomass",
       x = "Avg SCAM biomass")

# Plotting specific biomass vs biomass -----------------------------------------

# Plotting biomass comparisons only in plots where both species in the pair
# show up

# Creating a function that will find the plots where both species show up
coexisting <- function(spec1, spec2) {
  coexist_tmp <- master |>
  filter(species_code %in% c(spec1, spec2)) |>
  group_by(transect, plot, species_code) |>
  summarize(total_biomass = sum(total_biomass, na.rm = TRUE)) |>
  group_by(transect, plot) |>
    # Finding if both species appear, in any year, in the plot
  mutate(coexist = case_when(min(total_biomass) == 0 ~ "N", 
                             TRUE ~ "Y")) |>
  ungroup() |>
  filter(coexist == "Y") |>
  select(transect, plot, species_code)
  
  coexist <- master |>
    right_join(coexist_tmp, by = c("transect","plot", "species_code")) |>
    mutate(tp = paste0(transect, ": ", plot),
           log_biomass = ifelse(total_biomass > 0, log(total_biomass), 0)) |>
    select(year, transect, plot, tp, species_code, total_biomass, log_biomass)
  
  return(coexist)
}

coexist_phau_scam <- coexisting("PHAU", "SCAM")

# Plotting biomass in the transect-plots where both PHAU and SCAM appear
ggplot(coexist_phau_scam, aes(x = year, y = total_biomass, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(~tp, scales = "free")

# Plotting log biomass in the transect-plots where both PHAU and SCAM appear
ggplot(coexist_phau_scam, aes(x = year, y = log_biomass, color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(~tp, scales = "free")

