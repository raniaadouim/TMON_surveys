# Messing around with data on plant cover

# Loading packagaes
library(tidyverse)
library(trend)

# Reading in the data
othercover <- read_csv("01b_processed_data/other_environmental_cover.csv")
plantcover <- read_csv("01b_processed_data/plant_species_cover.csv")

# Transforming data on other cover to merge later
othercover <- othercover |>
  select(transect:wrack, -c(month, day)) |>
  pivot_longer(cols = bare_ground:wrack, 
               names_to = "species_code", 
               values_to = "fractional_cover")

# Finding a factor that will rescale fractional cover
factor <- plantcover |>
  select(year, transect, plot, fractional_cover) |>
  bind_rows(othercover) |>
  group_by (year, transect, plot) |>
  summarize(sum = sum(fractional_cover)) 

# Merging data sets and rescaling fractional cover
cover <- plantcover |> 
  select(year, transect:species_code, fractional_cover) |>
  bind_rows(othercover) |>
  left_join(factor, by = c("year", "transect", "plot")) |>
  mutate(fractional_cover = round(fractional_cover / sum, 3)) |>
  select(!sum) |>
  filter(species_code %in% c("IVFR", "PHAU", "SCAM", "DISP", "SPPA"),
         transect %in% c("G1", "G2", "G3"))
  

# Plotting plant cover 

masterplot <- cover |>
  ggplot(aes(x = year, y = fractional_cover, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"))

masterplot

# Notes from master plot:
# G1 1, 10, 11 : In 2019, PHAU randomly starts appearing, than rapidly grows 
# and outcompetes other plants
# G3 3, 4, 5, G2 7: PHAU does not outcompete the other plants. Either no 
# outcompetition at all, or another plant (spartina, iva) outcompetes

# To look at:
# Zoom in on 2019
# Look at growth overall each year, not separated by plot
# summary statistics

# Calculating sen's slope
slope <- cover |>
  complete(species_code, plot, transect, year, 
           fill = list(fractional_cover = 0)) |>
  group_by(transect, plot, species_code) |>
  mutate(trend = round(sens.slope(fractional_cover)$estimates[1], 3),
         p = case_when(fractional_cover != 0 ~ 
                         round(sens.slope(fractional_cover)$p.value[1], 4), 
                       TRUE ~ 1.1),
         direction = case_when(trend > 0 ~ "INC", trend < 0 ~ "DEC", 
                               p > 0.05 | trend == 0 ~ "INSIG")) 

# Filtering out plots with no significant changes (convoluted)
tmp <- slope |>
  mutate(p = case_when(p < 0.05 ~ 10, TRUE ~ 0)) |>
  group_by(transect, plot) |>
  summarize(ptmp = sum(p)) |>
  filter(ptmp >= 10)

# Plots with significant changes only
significant <- slope |>
  inner_join(tmp, by = c("transect", "plot")) |>
  select(-ptmp)

# Graphing just the plots with significant changes in percent cover
sigplot <- significant |>
  ggplot(aes(x = year, y = fractional_cover, color = species_code)) +
  geom_point() + 
  geom_line() +
  facet_wrap(c("transect", "plot"))

sigplot

# Notes from sigplot:
# HUGE jumps in fractional cover for a variety of species across a variety of plots,
# from 2022 to 2023. The ones that jumped either then decreased the next year, or increased
# only very slightly. 
# across almost all plots, sharp increases in DISP and SPPA are immediately followed 
# by sharp decreases (creating lots of "triangles")
# 22 significant plots, more than the biomass plots 
# adds: G1 3, 4, G2 3, 5, 11, G3 4, 5
# missing: G1 6, 8, 19
  


  
