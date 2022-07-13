### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

library(tidyverse)        # R's dialect used in this script
library(magrittr)         # additional pipes (%$%)
library(broom)            # tidy the output of statistical tests and models

library(ggtext)           # additional plotting package: markdown text
library(hrbrthemes)       # additional plotting package: theming
library(scales)           # additional plotting package: percent scales 

library(syuzhet)          # textual sentient analysis

library(kable)            # exporting good looking table, 1
library(kableExtra)       # exporting good looking table, 2

#### 0. Preliminaries ####

# getting to the right place


# importing the main task data
df  <- read_csv("Data/main_task_data.csv")

# target directory for plots
target_dir_plots <- "Figures/"
target_dir_tables <- "Tables/"

# making some cosmetic changes for plots and tables
df <- df %>% 
  mutate(treatment = as.factor(treatment),
         treatment = fct_recode(treatment, "Click-and-drag" = "ours",
                                "Slider"         = "bins",
                                "Text"           = "number",
                                "Distribution"   = "metaculus"),
         treatment = fct_relevel(treatment, "Click-and-drag", "Slider", "Text")) %>% 
  mutate(time = paste0(time, " seconds"), 
         nbins = paste0(nbins, " bins"),
         shape = as.factor(shape), 
         shape = fct_relevel(shape, "Symmetric", "Skewed", "Bimodal"),
         nbins = as.factor(nbins),
         nbins = fct_relevel(nbins, "7 bins", "15 bins"))

#### 1. Sample #####

# Table with the demographics and other sample characteristics
source("demographic_table.R")

# Testing treatment differences
source("demographic_tests.R")

# Slackers by treatment: table and analysis
source("slackers_table.R")

# Slackers by treatment: tests
source("slackers_tests.R")

#### 2. Final accuracy ####

# final accuracy, one observation per screen per treatment -- detailed table for the appendix
source("final_accuracy_detailed_table.R")

# final accuracy, overall and by screen dimesions (time, nbins, shape)
source("final_accuracy_overall_table_and_plots.R")

# final accuracy tests
source("final_accuracy_tests.R")

# final accuracy robustness: non-slackers only
source("final_accuracy_robustness_slackers.R")

# final accuracy tests
source("final_accuracy_robustness_slackers_tests.R")


#### 3. Path to final accuracy ####

# path: one subplot by type of screen (24 subplots in 2 plots, 15 and 45 secs)
source("path_accuracy_detailed_plot.R")

# path, overall and by screen dimesions (nbins, shape)
source("path_accuracy_overall_plot.R")

#### 4. self-repoted assessment ####

# understanding main table (paper) and plot (presentations)
source("understanding_plots_and_table.R")

# understanding: tests
source("understanding_tests.R")


#### 5. Additional exploratory analyses #####

# sentiment analysis of the commentaries
source("sentiment_analysis.R")

# input device table and analysis
source("device_analysis.R")


#### 6. temperature prediction analysis ####

source("temperature_prediction.R")
