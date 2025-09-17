# In this R script, I perform QAQC and some basic data wrangling on the raw 
# biomass dataset. I then save the final version as a .Rdata file 

# Loading packages -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)

# Reading in data --------------------------------------------------------------
biomass_raw <- read_csv("raw_data/all_biomass.csv") 
cover_raw <- read_csv("raw_data/plant_species_cover.csv")

# Handling Duplicates ----------------------------------------------------------

# Filtering biomass data for only the plants and plots of interest
biomass_filtered <- biomass_raw |>
  filter(species_code %in% c("PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  drop_na(total_biomass)

# Finding & handling duplicate biomass measurements
dupes <- biomass_filtered |>
  group_by(year, transect, plot, species_code) |>
  filter(n() > 1) |>
  arrange(year, transect, plot, species_code) |>
  # Creating a numerical marker of what measurement method was used
  mutate(method_num = ifelse(method == "allometry", 0, 1),
         # Creating a numerical marker of whether the duplicate measurements
         # are from the same measurement method. 0 means both are allometry, 
         # 1 means mixed method, 2 means both are harvest.
         method_match = sum(method_num), 
         # 2017 G1 12: Based on the original data sheets, SCAM data from G1 13 was 
         # mislabeled as coming from G1 12. 
         plot = ifelse(total_biomass == 157.005 & species_code == "SCAM",
                              13, plot), 
         # Averaging duplicate values from the same method
         total_biomass = ifelse(method_match != 1, mean(total_biomass), 
                                total_biomass)) |>
  # If duplicates are from different methods, keep the allometry measurements
  filter(!(method_match == 1 & method == "harvest")) |>
  distinct() 
  
# Creating a new dataset with duplicate measurements resolved
biomass_tmp <- biomass_filtered |>
  group_by(year, transect, plot, species_code) |>
  filter(n() == 1) |>
  full_join(dupes) |>
  select(year, transect, plot, species_code, total_biomass)
  
# Handling Missing Biomass -----------------------------------------------------

# Filtering percent cover data for plants and plots of interest
cover <- cover_raw |>
  select(year, transect, plot, species_code, fractional_cover) |>
  filter(species_code %in% c("PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  drop_na(fractional_cover)

# Joining biomass and percent cover data
combined <- biomass_tmp |>
  full_join(cover) |>
  distinct() 

# Cross referencing biomass and cover to find missing biomass data
cross <- combined |>
  filter(is.na(total_biomass)) |>
  # Correcting NA values using physical data sheet entries
  mutate(total_biomass = case_when((species_code == "IVFR" & year == 2019 & 
                                     transect == "G3" & plot == 3) ~ 0,
                                   (species_code == "IVFR" & year == 2022 & 
                                     transect == "G1" & plot == 6) ~ 0,
                                   (species_code == "IVFR" & year == 2017 & 
                                     transect == "G2" & plot == 6) ~ 0,
                                   (species_code == "IVFR" & year == 2017 & 
                                     transect == "G1" & plot == 12) ~ 0,
                                   TRUE ~ total_biomass)) |>
  select(!fractional_cover)

biomass_tmp2 <- biomass_tmp |>
  full_join(cross)

# Missing data was handled in more detail in an excel sheet. I checked scans of 
# physical data sheets, excel sheets, and noted down some issues and data 
# quality flags. Some NA biomass and % cover values were changed. 

# Identifying potentially unusual biomass measurements -------------------------

# Box plot of biomass, faceted by species
ggplot(biomass_tmp2, aes(x = species_code,y = total_biomass)) +
  geom_boxplot() 

# Finding extreme outliers of biomass within each species, using IQR
outliers <- biomass_tmp2 |>
  drop_na(total_biomass) |>
  group_by(species_code) |>
  mutate(iqr = IQR(total_biomass),
            q3 = quantile(total_biomass, 0.75),
            limit = q3 + (3*iqr),
         outlier = ifelse(total_biomass > limit, "Y", "N")) |>
  filter(outlier == "Y")

# Manually compared physical scans to excel data to check that data was entered 
# properly and removed one observation with data entry issues
biomass <- biomass_tmp2 |>
  filter(!(total_biomass == 2684.904 & species_code == "IVFR"))

# Filling in biomasses of 0 ----------------------------------------------------

# I assume that if a plant has neither a biomass nor a fractional cover 
# observation, then that plant was not present in the plot, and the biomass
# should be 0. I manually checked that this was true for ~ 30 entries, looking
# for explicit marks in the hardcopy scans that a plant had no fractional cover.

biomass <- biomass |>
  group_by(year, species_code) |>
  complete(transect = "G1", plot = 1:19, fill = list(total_biomass = 0), 
           explicit = FALSE) |>
  complete(transect = "G2", plot = 1:11, fill = list(total_biomass = 0),
           explicit = FALSE) |>
  complete(transect = "G3", plot = 1:5, fill = list(total_biomass = 0),
           explicit = FALSE) |>
  arrange(species_code, transect, plot, year)

# Saving data set --------------------------------------------------------------

save(complete, file = "processed_data/biomass.RData")
