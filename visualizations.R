
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

# Cleaning coordinate data -----------------------------------------------------

coords <- coords_raw |>
  filter(site == "GCREW", plot != 20, !(plot == 6 & transect == "G3")) 

master <- master |>
  full_join(coords)

# Static maps w/ satellite -----------------------------------------------------

# Registering my Google API
register_google(key = "")

# Retrieving satellite image
satellite <- get_googlemap(center = c(lon = -76.546114, lat = 38.875756),
                           zoom = 16, size = c(400, 400), maptype = "satellite")

mapping <- function(species, yr1, yr2, change) {
  sp <- master |>
    filter(species_code == species, year %in% c(2015, 2024))
  max <- max(sp$total_biomass, na.rm = T)
 
  if(change == "N") {
    sp <- sp |>
      filter(year == yr1)
    # Separating different kinds of biomass values
    pos <- sp |>  
      filter(total_biomass > 0) 
    na <- sp |>
      filter(is.na(total_biomass)) 
    zero <- sp |>
      filter(total_biomass == 0) 
    
    # Plotting map
    product <- ggmap(satellite) +
      geom_point(data = pos, aes(x = longitude, y= latitude, 
                                 fill = total_biomass), size = 3.5, 
                 shape = 21) +
      geom_point(data = na, aes(x = longitude, y = latitude), color = "black",
                 shape = 4, stroke = 1.5) + 
      geom_point(data = zero, aes(x = longitude, y = latitude), 
                 fill = "white", size = 1.5, shape = 21) +
      scale_fill_gradient(low = "#f9ffb8", high = "red", limits = c(0, max),
                          name = "Biomass (g)") +
      labs(title = paste0(yr1, "")) + 
      guides(size = "none") + 
      theme(axis.title = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), panel.grid = element_blank(),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.key.size = unit(0.8, "cm"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10)) 
    return(product)
    
  } else {
    # Calculating total biomass change from one year to another
    dif <- sp |>  
      filter(year == yr1 | year == yr2) |>
      group_by(transect, plot) |>
      mutate(previous = lag(total_biomass), change = total_biomass - previous) |>
      filter(year == yr2) 
    pos <- dif |>
      filter(change != 0)
    na <- dif |>
      filter(is.na(change))
    zero <- dif |>
      filter(change == 0)
    
    # Plotting map
    product <- ggmap(satellite) +
      geom_point(data = pos, aes(x = longitude, y = latitude, fill = change), 
                 size = 3.5, shape = 24) +
      geom_point(data = na, aes(x = longitude, y = latitude), color = "black",
                 shape = 4, stroke = 1.5) + 
      geom_point(data = zero, aes(x = longitude, y = latitude), 
                 fill = "white", size = 1.5, shape = 24) +
      scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen",
                           midpoint = 0, name = paste0("Change in\nBiomass (g)")) +
      labs(title = paste0("Change from ", yr1, " to ", yr2)) + 
      guides(size = "none") + 
      theme(axis.title = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), panel.grid = element_blank(),
            plot.title = element_text(size = 17, hjust = 0.5),
            legend.key.size = unit(0.8, "cm"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14)) 
    return(product)
    
  }
}

# Note: the way I wrote the function, yr2 must be greater (more recent) than yr1
# when making a change plot

# Creating plots 
# phau
phau_2015 <- mapping("PHAU", 2015, 2015, "N") +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"))
phau_2024 <- mapping("PHAU", 2024, 2024, "N") +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"))
phau_change <- mapping("PHAU", 2015, 2024, "Y") +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"))

# scam
scam_2015 <- mapping("SCAM", 2015, 2015, "N") 
scam_2024 <- mapping("SCAM", 2024, 2024, "N") 
scam_change <- mapping("SCAM", 2015, 2024, "Y") 

# Extracting legends
legend_bio_p <- get_legend(phau_2015, position = "top") 
legend_change_p <- get_legend(phau_change, position = "top") 
legend_bio_s <- get_legend(scam_2015, position = "top") 
legend_change_s <- get_legend(scam_change, position = "top") 

# Arranging maps


phau_left <- ggarrange(
                       phau_2015 + theme(legend.position = "none"),
                       phau_2024 + theme(legend.position = "none"),
                       ncol = 1,
                       widths = c(1, 1))
phau_right <- ggarrange(legend_bio_p,
                        phau_change + theme(legend.position = "none"),
                        legend_change_p,
                        ncol = 1,
                        widths = c(0.1, 1, 0.1),
                        heights = c(0.1, 1, 0.1))
phau_right
phau_maps <- ggarrange(
                       phau_left, phau_right, 
                       ncol = 2, 
                       widths = c(1, 1))
phau_maps
                      
phau_map <- annotate_figure(phau_maps, 
                            top = textGrob("P. australis", 
                                           gp = gpar(fontsize = 17, 
                                                     fontface = "bold.italic")))
phau_map

scam_left <- ggarrange(
  scam_2015 + theme(legend.position = "none"),
  scam_2024 + theme(legend.position = "none"),
  ncol = 1,
  widths = c(1, 1))
scam_right <- ggarrange(legend_bio_s,
                        scam_change + theme(legend.position = "none"),
                        legend_change_s,
                        ncol = 1,
                        widths = c(0.1, 1, 0.1),
                        heights = c(0.1, 1, 0.1))
scam_maps <- ggarrange(
  scam_left, scam_right, 
  ncol = 2, 
  widths = c(1, 1))

scam_map <- annotate_figure(scam_maps, 
                            top = textGrob("S. americanus", 
                                           gp = gpar(fontsize = 17, 
                                                     fontface = "bold.italic")))
scam_map

# 850, 650

# Static maps no satellite -----------------------------------------------------

# Function to map biomass in a given year
mapping_leaflet <- function(species, yr) {
  tmp <- master |>
    filter(species_code == species, year %in% c(2015, 2024))
  max <- max(tmp$total_biomass, na.rm = T)
  
  sp <- tmp |>
    filter(year == yr)
  # Separating different kinds of biomass values
  pos <- sp |>  
    filter(total_biomass > 0) 
  na <- sp |>
    filter(is.na(total_biomass)) 
  zero <- sp |>
    filter(total_biomass == 0) 
  
  # Creating color palette
  palette <- colorNumeric(palette = c("white", "#f9ffb8", "red"), 
                             domain = c(-max + 10, max))
  
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
    addCircleMarkers(data = zero, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = "white", 
                     fillOpacity = 0.9, color = "black") |>
    addCircleMarkers(data = pos, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = ~palette(total_biomass), 
                     fillOpacity = 0.9, color = "black") |>
    addLegend(position = "bottomright",
              pal = palette, values = c(0, pos$total_biomass), 
              title = "Total<br>Biomass (g)", opacity = 1) |>
    leafletOptions(resolutions = 1500) 
  return(product)
}

# Function to map change in biomass from 2015 to 2024
mapping_leafletchange <- function(species) {
  
  dif <- master |>
    filter(species_code == species, year %in% c(2015, 2024)) |>
    group_by(transect, plot) |>
    mutate(since2015 = lag(total_biomass), changeover = total_biomass - since2015) |>
    filter(year == 2024) 
  difleg <- dif |>
    filter(!is.na(changeover))
  pos <- dif |>
    filter(changeover > 0)
  na <- dif |>
    filter(is.na(changeover))
  neg <- dif |>
    filter(changeover < 0)
  zero <- dif |>
    filter(changeover == 0)
  max <- max(pos$changeover, na.rm = T)
  
  # Creating color palette for pos values
  palettepos <- colorNumeric(palette = c("#e6ffe6", "darkgreen"), 
                             domain = c(0, max))
  paletteneg <- colorNumeric(palette = c("darkred", "#fff0f0"), 
                             domain = c(-max, 0))
  
  paletteleg <- colorNumeric(palette = c("darkred", "white", "darkgreen"), 
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
    addCircleMarkers(data = zero, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = "white", 
                     fillOpacity = 0.9, color = "black") |>
    addCircleMarkers(data = pos, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = ~palettepos(changeover), 
                     fillOpacity = 0.9, color = "black") |>
    addCircleMarkers(data = neg, lng = ~longitude, lat = ~latitude, weight = 1.5,
                     radius = 7, fillColor = ~paletteneg(changeover), 
                     fillOpacity = 0.9, color = "black") |>
    addLegend(position = "bottomright",
              pal = paletteleg, values = difleg$changeover, 
              title = "Change in<br>Biomass (g)", opacity = 1) |>
    leafletOptions(resolutions = 1500) 
    return(product)
}


# Call fxns to create maps
phau_2015l <- mapping_leaflet("PHAU", 2015)
phau_2024l <- mapping_leaflet("PHAU", 2024)
phau_changel <- mapping_leafletchange("PHAU")

phau_2015l2 <- tags$div(
  # Label map
  tags$h3("2015", style = "text-align: center; 
  font-size: 20px; font-family: Helvetica; font-style: normal"),
  phau_2015l
)
browsable(phau_2015l2)

phau_2024l2 <- tags$div(
  # Label map
  tags$h3("2024", style = "text-align: center; font-size: 20px;
            font-family: Helvetica; font-style: normal"),
  phau_2024l
)
browsable(phau_2024l2)

phau_changel2 <- tags$div(
  # Label map
  tags$h3("Change from 2015 to 2024", style = "text-align: center; 
  font-size: 20px; font-family: Helvetica; font-style: normal"),
  phau_changel
)
browsable(phau_changel2)


# # # Using HTML CSS to combine the maps 
# temp <- tags$div(
#   # Need the title in a different div for different formatting
#   tags$div(style = "width: 100%;",
#     # Title
#     tags$h2("P. australis", style = "text-align: center; font-family: Helvetica;
#     font-style: italic; font-size: 16pt; margin-bottom: 20px;")),
#   # All maps
#   tags$div(
#     # Centers right column vertically, decides a gap between maps, and flexibly
#     # determines size, for all the maps within this larger div.
#     style = "display: flex; align-items: center; justify-content: center;
#     gap: 22px;",
#     # Stack 2015 and 2024 maps in the left column
#     tags$div(
#       # Sets map sizing / arrangement for all maps within this  div
#       style = "width: 40%; display: flex; flex-direction: column; gap: 22px;",
#       tags$div(
#         # Label map
#         tags$h3("2015", style = "text-align: center; font-family: Helvetica;
#               font-style: normal"),
#         phau_2015l),
#       tags$div(
#         tags$h3("2024", style = "text-align: center; font-family: Helvetica;
#               font-style: normal"),
#         phau_2024l)
#     ),
#     # Places change map in the right column
#     tags$div(
#       # Increase size
#       style = "width: 60%; height: 75%",
#       # Label map
#       tags$h3("Change from 2015 to 2024", style = "text-align: center;
#             font-family: Helvetica; font-style: normal"),
#      phau_changel
#     )
#   )
# )
# browsable(temp)

phau_leaf <- tags$div(
  # Need the title in a different div for different formatting
  tags$div(style = "width: 100%;",
           # Title
           tags$h2("P. australis", style = "text-align: center; font-family: Helvetica;
    font-style: italic; font-size: 18pt; margin-bottom: 18px; ")),
  tags$div(style = "width: 100%; display: flex; flex-direction: row; gap: 45px;
           align-items: center; justify-content: center;",
           tags$div(style = "flex: 1 1 0;", phau_2015l2),
           tags$div(style = "flex: 1 1 0;", phau_2024l2),
           tags$div(style = "flex: 1 1 0;", phau_changel2)))
browsable(phau_leaf)

elev <- tags$div(
  # Need the title in a different div for different formatting
  tags$div(style = "width: 100%;",
           # Title
           tags$h2("Elevation", style = "text-align: center; font-family: Helvetica;
                   font-size: 18pt; margin-bottom: 18px; ")),
  tags$div(style = "width: 100%; display: flex; flex-direction: row; gap: 45px;
           align-items: center; justify-content: center;",
           tags$div(style = "flex: 1 1 0;", elev_map)))
browsable(elev)



scam_2015l <- mapping_leaflet("SCAM", 2015)
scam_2024l <- mapping_leaflet("SCAM", 2024)
scam_changel <- mapping_leafletchange("SCAM")

scam_2015l2 <- tags$div(
  # Label map
  tags$h3("2015", style = "text-align: center; font-size: 20px;
            font-family: Helvetica; font-style: normal"),
  scam_2015l
)
browsable(scam_2015l2)

scam_2024l2 <- tags$div(
  # Label map
  tags$h3("2024", style = "text-align: center; font-size: 20px;
            font-family: Helvetica; font-style: normal"),
  scam_2024l
)
browsable(scam_2024l2)

scam_changel2 <- tags$div(
  # Label map
  tags$h3("Change from 2015 to 2024", style = "text-align: center; 
  font-size: 20px;font-family: Helvetica; font-style: normal"),
  scam_changel
)
browsable(scam_changel2)

scam_leaf <- tags$div(
  # Need the title in a different div for different formatting
  tags$div(style = "width: 100%;",
           # Title
           tags$h2("S. americanus", style = "text-align: center; font-family: Helvetica;
    font-style: italic; font-size: 18pt; margin-bottom: 18px; ")),
  tags$div(style = "width: 100%; display: flex; flex-direction: row; gap: 18px;
           align-items: center; justify-content: center;",
           tags$div(style = "flex: 1 1 0;", scam_2015l2),
           tags$div(style = "flex: 1 1 0;", scam_2024l2),
           tags$div(style = "flex: 1 1 0;", scam_changel2)))
browsable(scam_leaf)

ivfr_2015l <- mapping_leaflet("IVFR", 2015)
ivfr_2024l <- mapping_leaflet("IVFR", 2024)
ivfr_changel <- mapping_leafletchange("IVFR")

ivfr_2015l2 <- tags$div(
  # Label map
  tags$h3("2015", style = "text-align: center; font-size: 20px;
            font-family: Helvetica; font-style: normal"),
  ivfr_2015l
)

ivfr_2024l2 <- tags$div(
  # Label map
  tags$h3("2024", style = "text-align: center; font-size: 20px;
            font-family: Helvetica; font-style: normal"),
  ivfr_2024l
)

ivfr_changel2 <- tags$div(
  # Label map
  tags$h3("Change from 2015 to 2024", style = "text-align: center; 
  font-size: 20px; font-family: Helvetica; font-style: normal"),
  ivfr_changel
)

ivfr_leaf <- tags$div(
  # Need the title in a different div for different formatting
  tags$div(style = "width: 100%;",
           # Title
           tags$h2("I. frutescens", style = "text-align: center; font-family: Helvetica;
    font-style: italic; font-size: 18pt; margin-bottom: 18px; ")),
  tags$div(style = "width: 100%; display: flex; flex-direction: row; gap: 18px;
           align-items: center; justify-content: center;",
           tags$div(style = "flex: 1 1 0;", ivfr_2015l2),
           tags$div(style = "flex: 1 1 0;", ivfr_2024l2),
           tags$div(style = "flex: 1 1 0;", ivfr_changel2)))
browsable(ivfr_leaf)


# 1300, 500


# Animated Maps ----------------------------------------------------------------


animated <- function(species, change) {
  sp <- master |>
    filter(species_code == species) 
  max <- max(sp$total_biomass, na.rm = T)
  
  if(change == "N") {

    # Separating different kinds of biomass values
    pos <- sp |>  
      filter(total_biomass > 0) 
    na <- sp |>
      filter(is.na(total_biomass)) 
    zero <- sp |>
      filter(total_biomass == 0) 
    
    # Plotting map
    tmp <- ggmap(satellite) +
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
    
    # Add animation
    animated <- tmp +
      shadow_mark() + transition_time(master$year) +
      ggtitle('Year: {frame_time')
  
    return(animated)
    
  
  } else {
    return(master)
  }
}



 # ==========  ====================  ====================  ==========
  # Separating different kinds of biomass values


  # Add animation
  sp <- master |>
    filter(species_code == "IVFR", total_biomass > 0 ) |>
    select(year, transect, plot, total_biomass, latitude, longitude) |>
    mutate(tp = paste0(transect, ": ", plot), year = as.factor(year)) 

na <- master |>
  filter(species_code == "IVFR", is.na(total_biomass)) |>
  mutate(year = as.factor(year))

  max <- max(sp$total_biomass, na.rm = T)
  try <- ggmap(satellite) + 
    geom_point(data = sp, aes(group = year,
                              x = longitude, y = latitude, fill = total_biomass),
               shape = 21, size = 6) +
    geom_point(data = na, aes(group = year, x = longitude, y = latitude), color = "white",
               shape = 4, stroke = 2) +
    scale_fill_gradient(low = "#f9ffb8", high = "red", limits = c(0, max),
                         name = "biomass") +
    transition_states(year, state_length = 2, wrap = TRUE) +
    labs(title = "IVFR Biomass Across the Marsh", 
         subtitle = "Year: {closest_state}") +
    theme(axis.ticks = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    ease_aes()

  gganimate::animate(try)
  anim_save("ivfr-anim.gif")
  
# Environmental Drivers Over Time ----------------------------------------------
  
  # Threshold Values
  threshold_msl_raw <- noaa(begindate = 19850101, enddate = 20141231, 
                            station = "8575512", datum = "NAVD", 
                            interval = "monthly", time = "LST") |>
    janitor::clean_names() 
  
  threshold_msl <- threshold_msl_raw |>
    group_by(year) |>
    summarize(msl = mean(msl, na.rm = TRUE)) |>
    ungroup() |>
    summarize(med_msl = median(msl), max_msl = max(msl)) |>
    mutate(join = 1)
  
  threshold_meteo_raw <- read_csv("raw_data/open_meteoyr.csv") |>
    janitor::clean_names()
  
  threshold_meteo <- threshold_meteo_raw |>
    mutate(time = ymd(time), year = year(time)) |>
    group_by(year) |>
    summarize(precip = sum(precip), 
              temp = mean(temp)) |>
    ungroup() |>
    summarize(med_temp = median(temp), max_temp = max(temp), 
              med_precip = median(precip), max_precip = max(precip)) |>
    mutate(join = 1) |>
    full_join(threshold_msl) |>
    select(!join)
  
  # All threshold values (I got too tired to do it smarter)
  thresholds <- data.frame(
    variable = c("msl", "msl", "meantemp", "meantemp", "precip", "precip"),
    type = c("Median", "Maximum", "Median", "Maximum", "Median", "Maximum"),
    value = c(-0.006833333, 0.1189167, 13.86699, 15.3582, 	
              1085.65, 1494.8)
  ) |> 
    filter(variable == "msl")
  
avgbios <- master |>
    select(year, species_code, total_biomass) |>
    filter(!is.na(total_biomass)) |>
    group_by(species_code, year) |>
    summarize(avg_bio = mean(total_biomass)) |>
    pivot_wider(names_from = species_code, values_from = avg_bio) |>
    rename(avg_ivfr = IVFR, avg_phau = PHAU, avg_scam = SCAM) |>
    pivot_longer(!year, names_to = "variable", values_to = "value") 

# Adding error bars to msl and mean temp
# Error bars looked horrible, left them off
errors <- master |>
  select(year, sd_temp, sd_msl) |>
  distinct(year, .keep_all = TRUE) |>
  pivot_longer(cols = c(sd_temp, sd_msl), names_to = "variable",
               values_to = "sd") |>
  mutate(season = "yr", variable = ifelse(variable == "sd_temp", "meantemp", "msl"))

drivers <- master |>
  select(year, precip_spr, precip_yr, msl_spr, msl_yr, mean_temp_spr, 
         mean_temp_yr) |>
  distinct(year, .keep_all = TRUE) |>
  rename(meantemp_spr = mean_temp_spr, meantemp_yr = mean_temp_yr) |>
  pivot_longer(cols = c(precip_spr, precip_yr, msl_spr, msl_yr, meantemp_spr,
                        meantemp_yr),
               names_to = c("variable", "season"),
               names_sep = "_",
               values_to = "value") |>
  filter(year != 2025, year > 2014) |>
  full_join(errors)


drivers_plot <- drivers |>
  filter(season == "yr", year < 2025, variable == "msl")|>
  ggplot(aes(x = year, y = value)) +
  geom_point() + 
  geom_line() +
  egg::theme_article() +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
  labs(# title = "Annual Environmental Drivers (2015-2024)",
    # subtitle = "with historical benchmarks, reflecting 1985–2014 median and maximum annual values\n"
    x = "Year", y = "Mean Sea Level (m, NAVD88)", linetype = "Benchmark") +
  theme(# plot.title = element_text(size = 15, hjust = 0.5), 
        # plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        legend.position = "top",
        axis.title.x = element_text(size = 14, hjust = 0.5),
        axis.title.y = element_text(size = 14, hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold.italic"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
  
drivers_plot

# Biomass vs. Biomass ----------------------------------------------------------

avgbios <- master |>
  select(year, species_code, total_biomass) |>
  filter(!is.na(total_biomass)) |>
  group_by(species_code, year) |>
  summarize(avg_bio = mean(total_biomass),
            sum_bio = sum(total_biomass)) |>
  mutate(species_code = factor(species_code, levels = c("PHAU", "SCAM", "IVFR")))
ggplot(avgbios, aes(x = year, y = sum_bio)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species_code, scales = "free") +
  labs(x = "Year", y = "Total Biomass (g)") +
  egg::theme_article() +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey90", size = 0.5)) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024))





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
  



  

# Elevation --------------------------------------------------------------------

elev <- master |>
  dplyr::select(year, transect, plot, elevation, species_code, total_biomass,
         longitude, latitude) |>
  mutate(tp = paste0(transect, ": ", plot)) |>
  # Remove plots where the plant doesn't show up at all 
  group_by(tp, species_code) |>
  mutate(sum = sum(total_biomass, na.rm = T)) |>
  filter(sum != 0) |>
  ungroup() |>
  mutate(log_elevation = log(elevation), 
         species_code = factor(species_code, levels = c("PHAU", "SCAM", "IVFR")))
  
  
# Plotting elevation density
ggplot(elev, aes(x = elevation)) +
  geom_density() +
  labs(x = "Elevation (m, NAVD88)", y = "Density") + 
  egg::theme_article() + 
  theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey90", size = 0.5))

ggplot(elev, aes(x = elevation)) +
  geom_density() +
  labs(x = "Elevation (m, relative to NAVD88)", y = "Density") + 
  egg::theme_article() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_line(color = "grey90", size = 0.5))

# Determining elevation midpoints for color scale
median(elev$elevation)
median(elev$log_elevation)

# Plotting year vs. total_biomass, continuous
ggplot(elev |> mutate(year= as.numeric(year)) |> filter(species_code == "SCAM"), 
       aes(x = year, y = total_biomass)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(alpha = 0.5) +
  geom_smooth(method ='lm', se = F, aes(group = tp), alpha = 0.5, color = "black") +
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
# 900, 500



# Elevation map ----------------------------------------------------------------

elev2 <- master |>
  dplyr::select(year, transect, plot, elevation, species_code, total_biomass,
         longitude, latitude, change, fld_dur_spr) |>
  mutate(log_elevation = log(elevation)) |>
  filter(!is.na(elevation)) |>
  arrange(desc(elevation)) |>
  mutate(rank = case_when(elevation < 0.189 ~ "low", elevation > 0.274 ~ "high",
                          TRUE ~ "med"))

min <- min(elev2$elevation)
max <- max(elev2$elevation)
median <- median(elev2$elevation)
rad <- max - median

# Total biomass is statistically different across categorical
scam <- elev2 |>
  filter(species_code == "IVFR") 

test <- lm(total_biomass ~ fld_dur_spr, data = scam)
summary(test)

mod <- aov(change ~ rank, data = scam)
summary(mod)

max# Creating color palette
paletteelev <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                            domain = c(median - rad, median+rad))
pal_rev <- colorNumeric(palette = c("red", "darkgray", "blue"), 
                       domain = c(median - rad, median+rad), reverse = T)

# Function to reverse legend order (from https://github.com/rstudio/leaflet/issues/256#issuecomment-440290201)
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

elev_map<- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(group = "OSM (default)") |>
  setView(lng = -76.545776, lat = 38.875361, zoom = 16) |>
  addCircleMarkers(data = elev2, lng = ~longitude, lat = ~latitude, weight = 1.5,
                   radius = 7, fillColor = ~paletteelev(elevation), 
                   fillOpacity = 0.9, color = "black") |>
  addLegend_decreasing(position = "bottomright",
            pal = paletteelev, values = elev2$elevation, 
            decreasing = TRUE, title = "Elevation<br>(m, NAVD88)", 
            opacity = 1) |>
  leafletOptions(resolutions = 1500) 
elev_map

# 350 x 350

# Flooding Duration ------------------------------------------------------------

flooding <- master |>
  select(year, species_code, fld_dur_spr, transect, plot, elevation) |>
  group_by(year) |>
  distinct(fld_dur_spr, .keep_all = TRUE) |>
  drop_na(fld_dur_spr) |>
  mutate(tp = paste0(transect, ": ", plot))

flooding_extreme <- flooding |>
  ungroup() |>
  filter(year == 2024) |>
  filter(fld_dur_spr == max(fld_dur_spr) | fld_dur_spr == min(fld_dur_spr))

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

test <- plots |>
  ungroup() |>
  select(year, transect, plot, elevation, starts_with("fld")) |>
  pivot_longer(cols = starts_with("fld"), names_to = c("var", "season"),
               names_pattern = "(fld_depth|fld_dur)_(yr|spr|sum)", values_to = "value") |>
  mutate(season = case_when(season == "yr" ~ "year",
                            season == "spr" ~ "spring",
                            season == "sum" ~ "summer")) 


# PHAU vs SCAM -----------------------------------------------------------------
# MOST LIKELY NOT USING ========================================================

ggplot(master, aes(x = change_scam, y = change_phau)) +
  geom_point()

# Remove data points for plots where phrag / scam just does not show up
coexist_plots <- master |>
  filter(species_code != "IVFR") |>
  group_by(transect, plot, species_code) |>
  summarize(total_biomass = sum(total_biomass, na.rm = TRUE)) |>
  group_by(transect, plot) |>
  mutate(coexist = case_when(min(total_biomass) == 0 ~ "no", 
                             TRUE ~ "yes")) |>
  ungroup() |>
  select(transect, plot, coexist) |>
  distinct()

coexist <- master |>
  inner_join(coexist_plots) |>
  filter(species_code != "IVFR") |>
  select(transect, plot, total_biomass, species_code, year, coexist) |>
  group_by(transect, plot, year) |>
  mutate(tp = paste0(transect, ": ", plot)) |>
  # Transforming PHAU biomass to match SCAM change 
  mutate(total_biomass = ifelse(species_code == "PHAU", total_biomass / 10, 
                                total_biomass))

# Just for plots where both plants, at any point, appear 
ggplot(coexist |> filter(coexist == "yes"), aes(x = year, y = total_biomass,
                                                color = species_code)) +
  geom_point() +
  geom_line() +
  facet_wrap(~tp, scales = "free")
# Plots not super interesting - other plant mostly shows up in really low quantities

coexist_wide <- coexist |>
  group_by(tp) |>
  pivot_wider(names_from = "species_code", values_from = "total_biomass")

# For all plots
ggplot(coexist_wide, aes(x = PHAU, y = SCAM, color)) +
  geom_point()

# Checking how iva biomass changes overall

iva <- master |>
  filter(species_code == "IVFR", year %in% c(2015, 2024)) |>
  drop_na(total_biomass) |>
  select(year, transect, plot, total_biomass) |>
  group_by(transect, plot) |>
  mutate(change = total_biomass - lag(total_biomass)) |>
  drop_na(change) |>
  ungroup() |>
  # removing plots where iva doesn't change at all
  filter(change != 0) |>
  mutate(overall = mean(change))
# Disappears completely from 6 plots, decreases in 8 total
# increases in 6 plots

# Initial Salinity Measurements ------------------------------------------------
justelev <- master |>
  select(transect, plot, elevation) |>
  distinct()

sal_raw <- read_csv("raw_data/initial_salinity.csv")

salinity <- sal_raw |>
  left_join(justelev, by = c("transect", "plot")) |>
  mutate(plot = factor(plot), salinity = as.numeric(salinity), 
         # When I put as numeric, plot 17 gets NA'd. Fixing that here
         salinity = ifelse(plot == 17, 11.4, salinity))

# Salinity vs. elevation scatterplot
ggplot(salinity, aes(x = elevation, y = salinity, color = plot)) +
  geom_point() 

# Linear model predicting salinity by elevation
salelev <- lm(salinity ~ elevation, data = salinity)
summary(salelev)


iva <- master |>
  group_by(year, species_code) |>
  summarize(sum = sum(total_biomass, na.rm = T)) 

ggplot(iva, aes(x = year, y = sum)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~species_code, scales = "free")

phau <- master |>
  filter(species_code == "IVFR") |>
  drop_na(total_biomass) |>
  group_by(transect, plot) |>
  mutate(sum = sum(total_biomass, na.rm = T)) |>
  filter(sum > 0) 

ggplot(phau, aes(x = year, y = change_ivfr))  +
  geom_point() +
  geom_line()

phauchange <- lm(change_ivfr ~ year, data = phau)
summary(phauchange)
