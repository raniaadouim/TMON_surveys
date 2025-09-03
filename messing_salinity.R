# Loading packages
library(tidyverse)

# Reading in data
porewater_raw <- read_csv("raw_data/porewater.csv") |>
  janitor::clean_names()

# Cleaning data
porewater <- porewater_raw |>
  filter(co2 == 0 & nitrogen == 0, year >= 2015, #month == "September",
         salinity != -99) |>
  select(year, depth, salinity, so4, cl) |>
  mutate(depth = factor(depth)) 

porewater <- porewater_raw |>
  filter(co2 == 0 & nitrogen == 0, year >= 2015, 
         salinity != -99) |>
  select(year, month, depth, salinity, so4, cl) |>
  mutate(depth = factor(depth)) 

# Calculating averages by depth and year
porewater_sum <- porewater |>
  group_by(year, depth) |>
  summarize(salinity = mean(salinity), cl = mean(cl), so4 = mean(so4))

# Scatterplot of salinity vs time, faceted by depth
ggplot(porewater_sum, aes(x = year, y = salinity, color = depth)) +
  geom_point() + 
  facet_wrap("depth")

# Line plot of averaged salinity vs time
ggplot(porewater_sum, aes(x = year, y = salinity, color = depth)) +
  geom_point() + 
  geom_line()


# Density plot of salinity
ggplot(porewater |> mutate(salinity = sqrt(salinity)),
       aes(x = salinity, color = depth)) +
  geom_density() 

# Box plot of salinity
ggplot(porewater,
       aes(x = salinity, color = depth)) +
  geom_boxplot() 

# Dot plot of salinity by depth
ggplot(porewater, aes(x = depth, y = salinity, color = depth)) +
  geom_point() 

# Linear model
tmp <- porewater_sum |>
  mutate(depth = case_when(depth == 20 | depth == 40 ~ 40, TRUE ~ 80))
lm <- lm(salinity ~ depth*year, data = tmp)
summary(lm)

# Looking at just change in salinity
change <- porewater_sum |>
  group_by(depth) |>
  mutate(prev = lag(salinity), change = salinity - prev) |>
  select(year, change, depth) |>
  drop_na()

# Scatterplot of change in salinity vs time, faceted by depth
ggplot(change, aes(x = year, y = change, color = depth)) +
  geom_point() + 
  facet_wrap("depth")

# Line plot of change in salinity over time
ggplot(change, aes(x = year, y = change, color = depth)) +
  geom_point() + 
  geom_line()

# Calculating individual changes in salinity, not avg
indiv_change <- porewater_raw |>
  filter(co2 == 0 & nitrogen == 0, year >= 2015, month == "September",
         salinity != -99) |>
  select(year, chamber, depth, salinity) |>
  mutate(depth = factor(depth)) |>
  group_by(chamber, depth) |>
  mutate(prev = lag(salinity), change = salinity - prev) |>
  drop_na()

# Scatterplot of change in salinity vs time, colored by depth
ggplot(indiv_change, aes(x = year, y = change, color = depth)) +
  geom_point() 

# ANOVA of indiv change in salinity vs depth

anova <- aov(change ~ depth*year, data = indiv_change)
  
# QQ-plot
library(car)
qqPlot(anova$residuals,
       id = FALSE # id = FALSE to remove point identification
)
hist(anova$residuals)

summary(anova)
# not significant, conditions met

# ANOVA of salinity vs depth
aov <- aov(salinity ~ depth*year, data = porewater)
qqPlot(aov$residuals,
       id = FALSE # id = FALSE to remove point identification
) # not met
hist(aov$residuals)

summary(aov)
# not significant





library(VulnToolkit)
test <- noaa(20140901, 20230901, "8575512")

library(VulnToolkit)
noaa_data <- noaa("20150101", "20240101", station = 8575512)
  