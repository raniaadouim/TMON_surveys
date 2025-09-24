# Static maps w/ satellite -----------------------------------------------------



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
