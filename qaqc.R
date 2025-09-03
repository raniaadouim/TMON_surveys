# In this R script, I perform QAQC and some basic data cleaning on the raw 
# biomass dataset. I save the final version as a .Rdata file 

# Loading packages -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)

# Reading in data --------------------------------------------------------------
biomass_raw <- read_csv("raw_data/all_biomass.csv") 
cover_raw <- read_csv("raw_data/plant_species_cover.csv")
missing_raw <- read_csv("processed_data/qaqc_missing_biomass.csv")

# Handling Duplicates ----------------------------------------------------------

# Finding duplicate biomass measurements
dupes_check <- biomass_raw |>
  filter(species_code %in% c("PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  filter(n() > 1) 

# Pulling out the duplicate measurements we want to keep
dupes <- dupes_check |>
  mutate(total_biomass = round(total_biomass, 3),
         isna = case_when(is.na(total_biomass) ~ "Y", TRUE ~ "N")) |>
  # Some plants are listed twice, once w/ NA for allometry, once w/ a biomass 
  # for harvest. We want to mark that the harvest versions should be kept
  group_by(year, transect, plot, species_code) |>
  mutate(lagisna = lag(isna), 
         biomass_notes = case_when(lagisna == "Y" ~ "keep",
         TRUE ~ biomass_notes)) |>
  # Dropping the NA allometry entries
  drop_na(total_biomass) |>
  # Remove duplicated harvest measurements and keep allometry measurements, w/
  # the exception of the harvest marked as kept
  filter(!(biomass_notes != "keep" & method == "harvest"),
         # 2020 G1 4: original data sheets and % cover (0.5%) indicate trace 
         # amounts of IVFR, so the smaller biomass value is kept
         !(species_code == "IVFR" & year == 2020 & total_biomass > 3),
         # 2018 G1 7: % cover (62.5%), and biomass of other plants in that plot
         # indicate large amounts of IVFR, so the larger value is kept
         !(species_code == "IVFR" & plot == 7 & total_biomass < 10),
         # 2018 G3 3: % cover(87.5%) and biomass of other plants in that plot 
         # indicate large amounts of IVFR, so the larger value is kept
         total_biomass != 5.212,
         # 2022 G1 5: Cannot find in the original data sheets an obs. of SPPA
         # with 131 stems. Keeping the one with 5 stems, which I can find
         (stem_count != 131 | is.na(stem_count))) |>
  # 2017 G1 12: Based on the original data sheets, SCAM data from G1 13 was 
  # mislabeled as coming from G1 12. 
  mutate(plot = ifelse(total_biomass == 157.005, 13, plot)) |>
  group_by(year, transect, plot, species_code) |>
  # 2017 G1 7: Based on the original data sheets, duplicate SCAM entries seem 
  # accurate. The values are very similar to each other anyway, so I'll avg them
  mutate(total_biomass = case_when(species_code == "SCAM" ~ mean(total_biomass),
                                   TRUE ~ total_biomass)) |>
  distinct()
  
biomass <- biomass_raw |>
  filter(species_code %in% c("SCAM", "IVFR", "PHAU"),
         transect %in% c("G1", "G2", "G3")) |>
  group_by(year, transect, plot, species_code) |>
  filter(n() == 1) |>
  # Combining with fixed duplicate data
  full_join(dupes) 
  
# Handling Missing Biomass -----------------------------------------------------

# Cleaning up percent cover data
cover <- cover_raw |>
  select(year, transect, plot, species_code, fractional_cover) |>
  filter(species_code %in% c("PHAU", "SCAM", "IVFR"),
         transect %in% c("G1", "G2", "G3")) 

# Joining biomass and species cover data
all_check <- biomass |>
  select(year:species_code, total_biomass) |>
  full_join(cover) |>
  filter(!(is.na(fractional_cover) & is.na(total_biomass))) |>
  distinct() 

# Cross referencing biomass and cover to find missing data
cross <- all_check |>
  filter(is.na(fractional_cover) | is.na(total_biomass)) 

# Missing biomass data was handled manually in an excel sheet. Checked scans of 
# physical data sheets, excel sheets, and noted down some issues and data 
# quality flags. Some NA biomass and % cover values were changed. 

# Removing duplicate entries that arise after joining
dupes_2 <- missing_raw |>
  filter(species_code != "SPPA") |>
  full_join(all_check) |>
  distinct() |>
  group_by(transect, plot, year, species_code) |>
  filter(n() > 1, !is.na(data_quality_flag)) 

# Creating a new dataset with updated values and data quality flags
all <- missing_raw |>
  filter(species_code != "SPPA") |>
  full_join(all_check) |>
  distinct() |>
  group_by(transect, plot, year, species_code) |>
  filter(n() == 1) |>
  full_join(dupes_2) |>
  filter(!(is.na(total_biomass) & is.na(fractional_cover))) |>
  mutate(flag_biomass = case_when(is.na(total_biomass) ~ data_quality_flag, 
         TRUE ~ NA),
         flag_cover = case_when(is.na(fractional_cover) ~ data_quality_flag,
         TRUE ~ NA)) |>
  select(year:fractional_cover, flag_biomass, flag_cover) 

# Identifying potentially unusual biomass measurements -------------------------

# Scatterplot of biomass vs. fractional cover, faceted by species
ggplot(all, aes(x = fractional_cover, y = total_biomass)) +
  geom_point() +
  facet_wrap("species_code", scales = "free")

# Box plot of biomass, faceted by species
ggplot(all, aes(x = total_biomass)) +
  geom_boxplot() +
  facet_wrap("species_code", scales = "free")

# Finding extreme outliers of biomass within each species, using IQR
outliers <- all |>
  drop_na(total_biomass) |>
  group_by(species_code) |>
  mutate(iqr = IQR(total_biomass),
            q3 = quantile(total_biomass, 0.75),
            limit = q3 + (3*iqr),
         outlier = ifelse(total_biomass > limit, "Y", "N")) |>
  filter(outlier == "Y") |>
# Manually compared hardcopy scans to excel data to check that data was entered 
# properly and flagged an observation with data entry issues
  mutate(total_biomass = round(total_biomass, 3), 
  flag_biomass = case_when(total_biomass == 2684.904 ~ "data entry",
                                TRUE ~ flag_biomass))

# Completing missing biomass ---------------------------------------------------

# I assume that if a plant has neither a biomass nor a fractional cover 
# observation, then that plant was not present in the plot, and the biomass
# should be 0. I manually checked that this was true for ~ 10 entries, looking
# for explicit marks in the hardcopy scans that a plant had no fractional cover.
# I do not change NA values

complete <- all |>
  group_by(year, species_code) |>
  complete(transect = "G1", plot = 1:19, fill = list(total_biomass = 0), 
           explicit = FALSE) |>
  complete(transect = "G2", plot = 1:11, fill = list(total_biomass = 0),
           explicit = FALSE) |>
  complete(transect = "G3", plot = 1:5, fill = list(total_biomass = 0),
           explicit = FALSE) |>
  arrange(species_code, transect, plot, year)

# Saving data set --------------------------------------------------------------

save(complete, file = "processed_data/complete.RData")
