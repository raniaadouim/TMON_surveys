# Loading packages -------------------------------------------------------------
library(tidyverse)

# Reading in data --------------------------------------------------------------
load("processed_data/interest.Rdata")
meteo_raw <- read_csv("raw_data/open_meteo.csv")

# Some more precipitation ------------------------------------------------------

meteo_sum <- meteo_raw |>
  mutate(time = dmy(time), year = year(time), month = month(time)) |>
  filter(year > 2014, month %in% c(6, 7, 8)) |>
  group_by(year) |>
  summarize(precip_sum = sum(precip_sum), 
            mean_temp_sum = sum(mean_temp))

meteo_aug <- meteo_raw |>
  mutate(time = dmy(time), year = year(time), month = month(time)) |>
  filter(year > 2014, month %in% c(8)) |>
  group_by(year) |>
  summarize(precip_aug = sum(precip_sum), 
            mean_temp_aug = sum(mean_temp)) |>
  full_join(meteo_sum)

interest <- interest |>
  full_join(meteo_aug)


# Initial Linear Regressions ---------------------------------------------------

linear <- interest |>
  group_by(year, species_code) |>
  # Calculating avg biomass just in the plots it appears in
  filter(total_biomass > 0) |>
  mutate(avg_biomass = mean(total_biomass, na.rm = T)) |>
  distinct(avg_biomass, .keep_all = T) |>
  ungroup() |>
  pivot_longer(cols = c(precip_spr, precip_sum, precip_aug, msl_spr, 
                        mean_temp_spr, mean_temp_sum, mean_temp_aug, salinity),
               names_to = c("driver"),
               values_to = "value") |>
  select(year, species_code, avg_biomass, change, driver, species_code, value) 
  
# PHAU =========================================================================
phaul <- linear |>
  filter(species_code == "PHAU") |>
  mutate(log_biomass = log(avg_biomass),
         log_value = log(value))

ggplot(phaul, aes(x = value, y = avg_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 

# Log transformation

ggplot(phaul, aes(x = log_value, y = avg_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 

ggplot(phaul, aes(x = log_value, y = log_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 


phaul_wide <- phaul |>
  select(!starts_with("log")) |>
  pivot_wider(names_from = driver, values_from = value)

phaul_wide2 <- phaul |>
  select(-c(avg_biomass, value)) |>
  pivot_wider(names_from = driver, values_from = log_value)

p1 <- lm(avg_biomass ~ precip_spr , data = phaul_wide)
summary(p1)
p2 <- lm(avg_biomass ~ msl_spr , data = phaul_wide)
summary(p2)
# Salinity seems just unrelated 
p3 <- lm(avg_biomass ~ salinity , data = phaul_wide)
summary(p3)

p1a <- lm(log_biomass ~ precip_spr , data = phaul_wide2)
summary(p1a)
p2a <- lm(log_biomass ~ msl_spr , data = phaul_wide2)
summary(p2a)
# Salinity seems just unrelated 
p3a <- lm(log_biomass ~ salinity , data = phaul_wide2)
summary(p3a)

pall <- lm(avg_biomass ~ precip_spr + msl_spr + salinity, data = phaul_wide)
summary(pall)

ptwoa <- lm(log_biomass ~ precip_spr + msl_spr, data = phaul_wide2)
summary(ptwoa)
anova(p1a, ptwoa)

ptwob <- lm(avg_biomass ~ precip_spr + salinity, data = phaul_wide)
summary(ptwob)

ptwoc <- lm(avg_biomass ~ salinity + msl_spr, data = phaul_wide)
summary(ptwoc)

ptwo <- lm(avg_biomass ~ precip_spr + msl_spr, data = phaul_wide)
summary(ptwo)

anova(p1, ptwo)

# Best model: just spring precip


# SCAM =========================================================================
scaml <- linear |>
  filter(species_code == "SCAM") |>
  mutate(log_biomass = log(avg_biomass),
         log_value = log(value)) 

ggplot(scaml, aes(x = value, y = change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 

ggplot(scaml, aes(x = value, y = log_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 

scaml_wide <- scaml |>
  select(-c(log_value, avg_biomass)) |>
  pivot_wider(names_from = driver, values_from = value)

s1 <- lm(log_biomass ~ precip_aug , data = scaml_wide)
summary(s1)
s2 <- lm(log_biomass ~ msl_spr , data = scaml_wide)
summary(s2)
s3 <- lm(log_biomass ~ precip_sum , data = scaml_wide)
summary(s3)

stwo <- lm(log_biomass ~ precip_sum + msl_spr, data = scaml_wide)
summary(stwo)

anova(s3, stwo)
anova(s2, stwo)

# BEST MODEL: stwo 

# IVFR =========================================================================
ivfrl <- linear |>
  filter(species_code == "IVFR") |>
  mutate(log_biomass = log(avg_biomass),
         log_value = log(value)) 

ggplot(ivfrl, aes(x = value, y = avg_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 

ggplot(ivfrl, aes(x = log_value, y = log_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("driver", scales = "free") 


ivfrl_wide <- ivfrl |>
  pivot_wider(names_from = driver, values_from = log_value) 

# precip has negative R^2
i1 <- lm(log_biomass ~ precip_aug , data = ivfrl_wide)
summary(i1)
i2 <- lm(log_biomass ~ msl_spr , data = ivfrl_wide)
summary(i2)
# Salinity has negative R^2 lmao
i3 <- lm(log_biomass ~ salinity , data = ivfrl_wide)
summary(i3)

ivfrl_wide2 <- ivfrl |>
  select(!starts_with("log")) |>
  pivot_wider(names_from = driver, values_from = value) 

i1 <- lm(avg_biomass ~ precip_aug , data = ivfrl_wide2)
summary(i1)
i2 <- lm(avg_biomass ~ msl_spr , data = ivfrl_wide2)
summary(i2)
# Salinity has negative R^2 lmao
i3 <- lm(avg_biomass ~ salinity , data = ivfrl_wide2)
summary(i3)

# Best model is just using msl_spr no trans.





