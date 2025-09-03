# Loading packagaes ------------------------------------------------------------
library(randomForest)
library(tidyverse)
library(ggplot2)
library(caret)
library(grid)
library(ggtext)

# Reading in data --------------------------------------------------------------
load("processed_data/interest.RData") 

# Random Forest Set-Up----------------------------------------------------------

# Fxn to run random forest on a given species and predictors
rf_fxn <- function(species, vars) {
  set.seed(1017)

  rfdata <- interest |>
    ungroup() |>
    filter(species_code == species) |>
    group_by(transect, plot) |>
    # Removing plots where the plant never shows up
    mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
    filter(sum > 0) |>
    ungroup() |>
    # Only selecting variables of interest
    select(change, vars)
  
  # Running random forest
  rf_change <-  randomForest(change ~ ., data = rfdata, proximity = T, 
                             importance = T, na.action = na.omit, 
                             ntree = 1000)
  rf_change
  
  # Calculating partial correlations for all predictors
  partials <- pdp::partial(rf_change, pred.var = vars[1], train = rfdata) |>
    pivot_longer(-yhat, names_to = "var", values_to = "x") |>
    rename(y = yhat)
  
  for(i in 2:length(vars)){
    new <- pdp::partial(rf_change, pred.var = vars[i],
                        train = as.data.frame(rfdata)) %>%
      pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
      rename(y = yhat)
    partials = rbind(partials, new)
  }
  
  # Creating variable importance plots
  importance <- as.data.frame(importance(rf_change)) 
  importance$var <- row.names(importance)
  
  importance_plot <- importance |>
    mutate(var = fct_reorder(var, .x = `%IncMSE`, .fun = mean, .desc = F)) |>
    ggplot(aes(x = `%IncMSE`, y = var)) +
    geom_col(fill = "#096192", width = 0.7) +
    xlab("Variable Importance (%)") +
    ylab("Variable Name") +
    ggtitle("Variable Importance") +
    scale_y_discrete(labels = c("loag_phau" = "Prev. PHAU\nLog Biomass",
                                "loag_scam" = "Prev. SCAM\nLog Biomass",
                                "fld_dur_spr" = "Spr. Flooding\nDuration")) +
    egg::theme_article() +
    theme(panel.grid.major = element_line(color = "grey90", size = 0.5),
      axis.title.y = element_text(size = 14, family = "Helvetica", 
                                      color = "black"),
          axis.title.x = element_text(size = 14, family = "Helvetica", 
                                      color = "black"),
          axis.text.y = element_text(size = 13, family = "Helvetica"),
          axis.text.x = element_text(size = 13, family = "Helvetica"),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 17, family = "Helvetica", 
                                    color = "black", face = "bold")) +
    scale_x_continuous(n.breaks = 6) 
  
  # Preparing for geom_rug
  rug <- rfdata |>
    pivot_longer(everything(), names_to = "var", values_to = "x") |>
    right_join(importance) |>
    mutate(var = fct_reorder(var, 
                             .x = `%IncMSE`,
                             .fun = mean,
                             .desc = T)) |>
    arrange(var, x) |>
    drop_na(x)
  
  # Plotting partial dependency
  partials_sorted <- partials |>
    left_join(importance) |>
    mutate(var = fct_reorder(var, 
                             .x = `%IncMSE`,
                             .fun = mean,
                             .desc = T)) |>
    arrange(var, x)
  
  labeller <- c("loag_phau" = "Prev. PHAU\nLog Biomass [log(g)]", 
                "loag_scam" = "Prev. SCAM\nLog Biomass [log(g)]",
                "fld_dur_spr" = "Spring Flooding\nDuration (%)")
  
  partial_plot <- partials_sorted |>
    ggplot(aes(x = x)) +
    geom_line(aes(y = y), color = "#096192") +
    geom_rug(color = "#096192", 
             sides = "b", outside = T, 
             data = rug,
             length = unit(0.05, "inch"), alpha = 0.2) +
    coord_cartesian(clip = "off") +
    xlab("Value") +
    ylab(paste0("Change in Biomass (g)")) +
    ggtitle("Partial Dependence") + 
    egg::theme_article() +
    theme(
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      strip.placement = "outside",
      strip.background.x = element_blank(),
      strip.text.x = element_text(margin = unit(c(0,0,0.3,0), units = "cm"),
                                  size = 14),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 13, family = "Helvetica"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
      axis.text.x = element_text(size = 13, family = "Helvetica")) +
    facet_wrap("var", scales = "free", labeller = as_labeller(labeller))
  
  # Confirming that 500 trees is enough
 tree_check <- randomForest(change ~ ., data = rfdata, ntree = 1000,
                             keep.forest = TRUE, na.action = na.omit)

  # Plot error vs. number of trees
   plot(tree_check, main = "OOB Error vs. Number of Trees")
  
  rf_plots_tmp <- ggpubr::ggarrange(
    importance_plot,
    partial_plot, 
    ncol = 2, widths = c(0.6, 1)) 
  

  title1 <- case_when(species == "PHAU" ~ "P. australis",
                      species == "SCAM" ~ "S. americanus",
                      TRUE ~ "I. frutescens")
  title <- textGrob(title1, gp = gpar(fontsize = 20, fontface = "bold.italic"))
  rf_plots <- annotate_figure(rf_plots_tmp, top = title)
  
  return (rf_plots)
}


# Random Forest: Plots ---------------------------------------------------------

# Selecting variables to run in the random forest
vars <- interest |>
  ungroup() |>
  dplyr::select(loag_scam, loag_phau, fld_dur_spr) |>
  names()

# PHAU 
phau_rf <- rf_fxn("PHAU", vars) 
phau_rf

# 1300, 400

# SCAM
scam_rf <- rf_fxn("SCAM", vars) 
scam_rf

vars_ivfr <- interest |>
  ungroup() |>
  dplyr::select(loag_phau, fld_dur_spr) |>
  names()

ivfr_rf <- rf_fxn("IVFR", vars_ivfr)
ivfr_rf

# Checking Flooding duration Plots w/ low PHAU biomass to investigate U-dip
fld_check <- interest |>
  group_by(transect, plot, species_code) |>
  # Removing plots where the plant never shows up
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  ungroup() |>
  filter(sum > 0, fld_dur_spr > 0.25 & fld_dur_spr < 0.55, 
         species_code != "IVFR") |>
  select(year, transect, plot, species_code, total_biomass, fld_dur_spr) |>
  arrange(transect, plot, year)

# Findings:
# There are 18 plots in this U-shaped area - 11 are just PHAU, no SCAM
# and 7 are both PHAU and SCAM. ALL 7 plots where PHAU and SCAM appeared are 
# in this dip
# PHAU and SCAM coexist only in low - mid flooding duration plots
# generally an upward trend where as flooding duration increases, PHAU increases,
# but this is hindered by SCAM presence

# Checking plots with low PHAU log biomass, to investigate a U-shaped dip 
phau_check <- interest |>
  group_by(transect, plot, species_code) |>
  # Removing plots where the plant never shows up
  mutate(sum = sum(total_biomass, na.rm = TRUE)) |>
  filter(sum > 0, loag_phau > 1.7 & loag_phau < 4, species_code != "IVFR") |>
  select(year, transect, plot, species_code, total_biomass, loag_phau) |>
  arrange(transect, plot, year)

# Findings:
# There are only 6 data points in this dip, but they're all from PHAU - SCAM
# coexisting plots

# Checking plots with higher IVFR change and high flooding duration

iva <- master |>
  filter(species_code == "IVFR") |>
  drop_na(total_biomass) |>
  select(year, transect, plot, total_biomass, fld_dur_spr) 

phau <- master |>
  filter(species_code == "PHAU") |>
  drop_na(total_biomass) |>
  select(year, transect, plot, total_biomass, fld_dur_spr)

# Findings: All of the plots above 0.6 have a biomass of 0, with the exception of 
# 2 plots with incredibly high biomass, throwing things off





