# Loading in packages
library(tidyverse)
library(ggplot2)
library(mclust)
library(ggrepel)
library(broom)
library(GGally)

# Readinggplot2# Reading in data
biomass_raw <- read_csv("raw_data/all_biomass.csv") 
elevation_raw <- read_csv("raw_data/gcrew_elevation.csv")

# Cleaning up elevation data
elevation <- elevation_raw |>
  filter(survey_id == "June 2016 TMON Survey", measurement_type != "test") |>
  select(transect_id, plot_id, corner, replicate_id, elevation) |>
  # Repeated measurements in a corner will be averaged
  group_by(transect_id, plot_id, corner) |>
  summarize(elevation = mean(elevation)) |>
  # Corner measurements will be averaged to get elevation for an entire plot
  group_by(transect_id, plot_id) |>
  summarize(elevation = mean(elevation)) |>
  rename(transect = transect_id, plot = plot_id) |>
  mutate(transect = toupper(transect))

# 5 number summary of elevation
summary(elevation$elevation)

elevation_2 <- elevation |>
  # Categorical rank of "high" & "low" based on mean values
  # Doesn't matter if you use mean or midpoint of min and max
  mutate(rank = case_when(elevation > 0.226 ~ "high", TRUE ~ "low")) 

elevation_3 <- elevation |>
  # Rank based on Q1 and Q3 values
  mutate(rank = case_when(elevation > 0.28344 ~ "high", elevation < 0.16279 ~ "low",
                          TRUE ~ "med"))

elevation_3e <- elevation |>
  # Rank based on dividing plots roughly equally
  # Low has 13, med/high has 12. Bc the dif between the low/med cutoff was smaller than
  # between the med/high cutoff
  mutate(rank = case_when(elevation < 0.189 ~ "low", elevation > 0.274 ~ "high",
                          TRUE ~ "med"))

# Handling duplicate allometry/harvest measurements
dupes <- biomass_raw |>
  select(year, transect:species_code, total_biomass, method) |>
  filter(species_code %in% c("SPPA", "DISP", "PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  drop_na() |>
  filter(n() > 1,
         (species_code %in% c("DISP", "SPPA") & method == "harvest") |
           species_code %in% c("PHAU", "IVFR", "SCAM") & method == "allometry") |>
  select(!method)

# Cleaning biomass data 
biomass <- biomass_raw |>
  select(year, transect:species_code, total_biomass) |>
  filter(species_code %in% c("SPPA", "DISP", "PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  drop_na() |>
  filter(n() == 1) |>
  full_join(dupes, by = c("year", "transect", "plot", "species_code", 
                          "total_biomass")) |>
  mutate(total_biomass = round(total_biomass, 3)) |>
  group_by(transect, plot, species_code) |>
  complete(year = 2015:2024,
           fill = list(total_biomass = 0)) |>
  drop_na(species_code) |>
  group_by(species_code, year) |>
  complete(transect = "G1", plot = 1:20, fill = list(total_biomass = 0)) |>
  complete(transect = "G2", plot = 1:11, fill = list(total_biomass = 0)) |>
  complete(transect = "G3", plot = 1:6, fill = list(total_biomass = 0)) |>
  arrange(transect, plot, species_code, year)

# Combining biomass and elevation data 
bio_elev_2 <- biomass |>
  full_join(elevation_2, by = c("transect", "plot")) 
bio_elev_3 <- biomass |>
  full_join(elevation_3, by = c("transect", "plot")) |>
  mutate(rank = factor(rank, levels = c("low", "med", "high")))
bio_elev_3e <- biomass |>
  full_join(elevation_3e, by = c("transect", "plot")) |>
  mutate(rank = factor(rank, levels = c("low", "med", "high")))

bio_elev <- biomass |>
  full_join(elevation, by = c("transect", "plot")) 

# Faceted scatterplot of biomass over time and elevation rank
ggplot(bio_elev_3e,
       aes(x = year, y = total_biomass, color = rank)) +
  geom_point() +
  facet_wrap("species_code", scales = "free")

# Count # of observations for a species by rank
bio_elev_3e |>
  filter(species_code == "PHAU", total_biomass != 0) |>
  group_by(rank) |>
  count()

# Constructing an elbow plot to determine the number of clusters I want

clusterprep <- bio_elev |>
  filter(species_code == "PHAU") |>
  ungroup() |>
  mutate(across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x),
                .names = "{.col}_z"))

clusterprep_phau <- clusterprep |>
  select(total_biomass_z, elevation_z) 

set.seed(52)
elbow_plot <-
  tibble(k = 1:5) |>
  mutate(kmeans_results = purrr::map(k, ~kmeans(clusterprep_phau, .x)),
         glanced = purrr::map(kmeans_results, glance)) |>
  unnest(cols = c(glanced))

# Construct elbow plot with all clusters considered
ggplot(elbow_plot, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = scales::breaks_width(5)) +
  labs(x = "Number of clusters (k)",
       y = expression("Total within-cluster sum of squares")) +
  theme_light()


# Plotting elevation vs. biomass, faceted by species
ggplot(bio_elev_3e, aes(x = elevation, y = total_biomass)) +
  geom_point(aes(color = rank, shape = rank)) +
  facet_wrap("species_code", scales = "free")

# Plotting year vs. biomass, colored by rank
ggplot(bio_elev_3e |> filter(species_code %in% c("PHAU", "SCAM", "IVFR")),
       aes(x = year, y = total_biomass)) +
  geom_point(aes(color = rank, shape = rank)) +
  facet_wrap("species_code", scales = "free")

# Calculating total biomass change in each plot and species from 2015-2024
elev_change <- bio_elev_3e |>
  filter(year == 2015 | year == 2024) |>
  relocate(species_code, year, .after = c(transect, plot)) |>
  arrange(transect, plot, species_code, year) |>
  group_by(transect, plot, species_code) |>
  mutate(previous = lag(total_biomass), change = total_biomass - previous) |>
  drop_na() |>
  ungroup() |>
  select(species_code, rank, change)

# Plotting change in biomass vs elevation rank
ggplot(elev_change |> filter(species_code %in% c("PHAU", "SCAM", "IVFR")), 
       aes(x = rank, y = change)) +
  geom_point(aes(color = rank)) +
  facet_wrap("species_code", scales = "free")



# # Clustering

#  
#  set.seed(618)
#  kmeans <- clusterprep_phau |>
#    kmeans(centers = 3, nstart = 20)
# 
# clusterprep_phau <- augment(kmeans, clusterprep_phau) 
# 
# clusters <- clusterprep |>
#   full_join(clusterprep_phau, by = c("total_biomass_z", "elevation_z"),
#             relationship = "many-to-many") 
# 
# # Plot clusters, faceted by years
#  ggplot(clusters, aes(x = elevation, y = total_biomass)) +
#    geom_point(aes(color = .cluster, shape = .cluster)) +
#    facet_wrap("year")




  



