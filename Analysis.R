### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

library(tidyverse) # R's dialect used in this script
library(magrittr) # additional pipes (%$%)
library(broom) # tidy the output of statistical tests and models

library(ggtext) # additional plotting package: markdown text
library(hrbrthemes) # additional plotting package: theming
library(scales) # additional plotting package: percent scales

library(esvis) # to compute cohen's d effect sizes

library(syuzhet) # textual sentient analysis

library(kableExtra) # exporting good looking table, 2

library(gtools) # to set up the list of combinations needed to perform pairwise tests over pairs of treatments

#### 0. Preliminaries ####

# getting to the right place


# importing the main task data
df <- read_csv("Data/main_task_data.csv")

# target directory for plots
target_dir_plots <- "Figures/"
target_dir_tables <- "Tables/"

# making some cosmetic changes for plots and tables
df <- df %>%
  mutate(
    treatment = as.factor(treatment),
    treatment = fct_recode(treatment,
      "Click-and-Drag" = "ours",
      "Slider" = "bins",
      "Text" = "number",
      "Distribution" = "metaculus"
    ),
    treatment = fct_relevel(treatment, "Click-and-Drag", "Slider", "Text")
  ) %>%
  mutate(
    time = paste0(time, " seconds"),
    nbins = paste0(nbins, " bins"),
    shape = as.factor(shape),
    shape = fct_relevel(shape, "Symmetric", "Skewed", "Bimodal"),
    nbins = as.factor(nbins),
    nbins = fct_relevel(nbins, "7 bins", "15 bins")
  )


## helper function: detailed pairwise t-test and cohen's d
paired_plus_cohen <- function(data, var1, groupvar) {
  
  formula <- as.formula(paste(var1, "~", groupvar))
  
  grouping <- combinations(n = 4, r = 2, 
                           v = levels(data[[groupvar]]), 
                           repeats.allowed = FALSE) %>% as_tibble()
  
  tests <- map2_df(grouping$V1, grouping$V2, ~tidy(t.test(formula, 
                                                       data = data %>% filter(treatment %in% c(.x,.y)) )))
  tests <- tests %>% bind_cols(grouping) %>% 
    select(group1 = V1, group2 = V2, statistic, parameter, p.value)
  
  
  
  
  cohen <- data %>% 
    coh_d(formula)
  
  tests %>% 
    left_join(cohen, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
    arrange(p.value) %>% 
    mutate(p.value = round(p.value, 3))
}


#### 1. Sample #####

# Table with the demographics and other sample characteristics
source("Scripts/demographic_table.R")

# Testing treatment differences
source("Scripts/demographic_tests.R")

# Slackers by treatment: table and analysis
source("Scripts/slackers_table.R")

# Slackers by treatment: tests
source("Scripts/slackers_tests.R")

#### 2. Final accuracy ####

# final accuracy, one observation per screen per treatment -- detailed table for the appendix
source("Scripts/final_accuracy_detailed_table.R")

# final accuracy, overall and by screen dimesions (time, nbins, shape)
source("Scripts/final_accuracy_overall_table_and_plots.R")

# final accuracy tests
source("Scripts/final_accuracy_tests.R")

# final accuracy robustness: non-slackers only
source("Scripts/final_accuracy_robustness_slackers.R")

# final accuracy tests
source("Scripts/final_accuracy_robustness_slackers_tests.R")


#### 3. Path to final accuracy ####

# path: one subplot by type of screen (24 subplots in 2 plots, 15 and 45 secs)
source("Scripts/path_accuracy_detailed_plot.R")

# path, overall and by screen dimesions (nbins, shape)
source("Scripts/path_accuracy_overall_plot.R")

#### 4. self-repoted assessment ####

# understanding main table (paper) and plot (presentations)
source("Scripts/understanding_plots_and_table.R")

# understanding: tests
source("Scripts/understanding_tests.R")


#### 5. Additional exploratory analyses #####

# sentiment analysis of the commentaries
source("Scripts/sentiment_analysis.R")

# input device table and analysis
source("Scripts/device_analysis.R")


#### 6. temperature prediction analysis ####

source("Scripts/temperature_prediction.R")
