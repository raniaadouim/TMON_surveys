# Cleaning & merging data sets -------------------------------------------------

library(readr)
library(leaflet)
library(ggplot2)
library(sf)
library(htmltools)

# Finding top 5 species, by total biomass across all plots and years
top5 <- biomass_raw |>
  filter(transect %in% c("G1", "G2", "G3")) |>
  drop_na(total_biomass) |>
  group_by(transect, plot, year, species_code) |>
  # There are some duplicates. Not dealing with that fully now, just averaging
  summarize(total_biomass = mean(total_biomass)) |>
  ungroup() |>
  group_by(species_code) |>
  summarize(total_biomass = sum(total_biomass)) |>
  arrange(desc(total_biomass))

# Cleaning coordinate data
coords <- coords_raw |>
  filter(site == "GCREW") |>
  select(!site)

# Cleaning porewater (salinity) data
porewater <- porewater_raw |>
  filter(co2 == 0 & nitrogen == 0, salinity != -99, year >= 2014) |>
  select(year, month, depth, salinity) |>
  mutate(depth = factor(depth)) |>
  group_by(year, month, depth) |>
  
  biomass <- biomass_raw |>
  select(year:species_code, total_biomass) |>
  filter(species_code %in% c("SPPA", "PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  drop_na(total_biomass) |>
  group_by(year, transect, plot, species_code) |>
  filter(n() == 1) |>
  ungroup() |>
  # Joining with revised duplicate entries
  full_join(dupes, by = c("year", "month", "day", "transect", "plot", 
                          "species_code", "total_biomass")) |>
  mutate(total_biomass = round(total_biomass, 3)) |>
  arrange(transect, plot, species_code, year)


# Combining biomass and tide data
biomass_tide <- tide_gauge |>
  full_join(biomass, by = "year") |>
  relocate(sea_lvl, .after = total_biomass)



# Separating data from each transect
g1 <- coords |> filter(transect == "G1")
g2 <- coords |> filter(transect == "G2")
g3 <- coords |> filter(transect == "G3")

# Creating a function to map species abundance across the marsh
mapping <- function(species, yr, change, coords) {
  
  plant <- biomass |>  
    filter(species_code == species) |>
    group_by(species_code, year) |>
    arrange(transect, plot) 
  
  if(change == "N") {
    plotprep <- plant |>
      filter(year == yr, total_biomass != 0) 
    
    pal <- c("#e2c1f5", "black")
    colorplant <- colorNumeric(palette = pal, domain = plant$total_biomass)
    
    coords <- coords |>
      inner_join(plotprep, by = c("transect", "plot"))
    
    
    plot <-
      leaflet() |>
      addTiles(group = "OSM (default)") |>
      addCircleMarkers(data = coords, lng = ~longitude, lat = ~latitude, weight = 1,
                       radius = 10, color = ~colorplant(plotprep$total_biomass), 
                       fillOpacity = 50, label = ~paste0(coords$transect, "_", 
                                                         coords$plot, ": ", 
                                                         plotprep$total_biomass)) |>
      addPolylines(data = g1, lng = ~longitude, lat = ~latitude, group = g1, 
                   weight = 1, label = ~transect) |>
      addPolylines(data = g2, lng = ~longitude, lat = ~latitude, group = g2, 
                   weight = 1, label = ~transect) |>
      addPolylines(data = g3, lng = ~longitude, lat = ~latitude, group = g3, 
                   weight = 1, label = ~transect) |>
      leafletOptions(resolutions = 1500)
    
    return(plot)
    
  } else {
    
    plant <- plant |>
      group_by(species_code, year) |>
      complete(transect = "G1", plot = 1:20, fill = list(total_biomass = 0)) |>
      complete(transect = "G2", plot = 1:11, fill = list(total_biomass = 0)) |>
      complete(transect = "G3", plot = 1:6, fill = list(total_biomass = 0)) |>
      arrange(transect, plot, year)
    
    change <- plant |>
      filter(year == 2015 | year == 2024) |>
      group_by(transect, plot) |>
      mutate(previous = lag(total_biomass), change = total_biomass - previous) |>
      drop_na() |>
      filter(change != 0)
    
    neg <- change |>
      filter(change < 0)
    
    pos <- change |>
      filter(change > 0)
    
    palneg <- c("darkred", "white")
    palpos <- c("white", "darkgreen")
    
    maxpos <- max(pos$change)
    maxneg <- abs(min(neg$change))
    if(maxpos > maxneg) {
      max <- maxpos
    } else {
      max <- maxneg
    }
    
    colorneg <- colorNumeric(palette = palneg, domain = (max*-1 - 1):0)
    colorpos <- colorNumeric(palette = palpos, domain = 0:max+1)
    
    negcoords <- coords |>
      inner_join(neg, by = c("transect", "plot")) 
    
    poscoords <- coords |>
      inner_join(pos, by = c("transect", "plot"))
    
    plot <-
      leaflet() |>
      addTiles(group = "OSM (default)") |>
      addCircleMarkers(data = negcoords, 
                       lng = ~longitude, lat = ~latitude, weight = 0.5, 
                       color = ~colorneg(neg$change), fillOpacity = 50, 
                       label = ~paste0(negcoords$transect, "_", negcoords$plot, ": ", 
                                       neg$change)) |>
      addCircleMarkers(data = poscoords, 
                       lng = ~longitude, lat = ~latitude, weight = 0.5, 
                       color = ~colorpos(pos$change), fillOpacity = 50, 
                       label = ~paste0(poscoords$transect, "_", poscoords$plot, ": ", 
                                       pos$change)) |>
      addPolylines(data = g1, lng = ~longitude, lat = ~latitude, group = g1, weight = 1,
                   label = ~transect,) |>
      addPolylines(data = g2, lng = ~longitude, lat = ~latitude, group = g2, weight = 1,
                   label = ~transect) |>
      addPolylines(data = g3, lng = ~longitude, lat = ~latitude, group = g3, weight = 1,
                   label = ~transect) |>
      leafletOptions(resolutions = 1500)
    
    return(plot)
  }
}

# Creating plots for all of our spcies-year-change combos

# phau
phau_2015 <- mapping("PHAU", 2015, "N", coords)
phau_2024 <- mapping("PHAU", 2024, "N", coords)
phau_change <- mapping("PHAU", 2015, "Y", coords)


# scam
scam_2015 <- mapping("SCAM", 2015, "N", coords)
scam_2024 <- mapping("SCAM", 2024, "N", coords)
scam_change <- mapping("SCAM", 2015, "Y", coords)

# IVFR
ivfr_2015 <- mapping("IVFR", 2015, "N", coords)
ivfr_2024 <- mapping("IVFR", 2024, "N", coords)
ivfr_change <- mapping("IVFR", 2015, "Y", coords)

# SPPA
sppa_2015 <- mapping("SPPA", 2015, "N", coords)
sppa_2024 <- mapping("SPPA", 2024, "N", coords)
sppa_change <- mapping("SPPA", 2015, "Y", coords)

# DISP
disp_2015 <- mapping("DISP", 2015, "N", coords)
disp_2024 <- mapping("DISP", 2024, "N", coords)
disp_change <- mapping("DISP", 2015, "Y", coords)


# Faceting 
leaflet_grid <- 
  tagList(
    tags$table(width = "100%",
               tags$tr(
                 tags$td(scam_2015),
                 tags$td(scam_2024),
                 tags$td(scam_change)
               )
    )
  )
browsable(leaflet_grid)




# # Creating triangle icons - to get them colored differently, you have to make 
# # one icon per row
# triangle_pos <- makeSymbolIcons(
#   shape = rep("triangle", nrow(pos)),
#   width = rep(18, nrow(pos)),
#   fillColor = palettepos(pos$changeover),
#   color = "black",
#   weight = 1.5,
#   opacity = rep(1, nrow(pos))
# )
# 
# triangle_neg <- makeSymbolIcons(
#   shape = rep("triangle", nrow(neg)),
#   width = rep(18, nrow(neg)),
#   fillColor = paletteneg(neg$changeover),
#   color = "black",
#   weight = 1.5,
#   opacity = rep(1, nrow(neg))
# )
# 
# triangle_zero <- makeSymbolIcons(
#   shape = rep("triangle", nrow(neg)),
#   width = rep(18, nrow(neg)),
#   fillColor = "white",
#   color = "black",
#   weight = 1.5,
#   opacity = rep(1, nrow(neg))
# )
# 
# 
# 
# 
# 
# 
# 
# 
