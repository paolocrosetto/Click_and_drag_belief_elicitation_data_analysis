### importing and formatting data for the belief horserace paper
###
### author: Paolo Crosetto
###
### this version: June 2022

library(tidyverse)
library(zoo)

#### 0. Preliminaries ####

# getting to the right place
setwd("/home/paolo/Dropbox/Belief Elicitation and Contract Theory/Validation experiment/Data/")

# importing the data
df <- read_csv("all_apps_wide_2022-06-22.csv")

# importing screen characteristics
screens <- read_csv("screens.csv")

# some preliminary name cleaning
df <- df %>% 
  rename(ID = participant.code) 


#### 1. identifying bots ####

# criterion 1: not gotten to screen 81
# criterion 2: earned less than zero
df <- df %>% 
  filter(participant._index_in_pages == 81 & participant.payoff > 0)


#### 3. cleaning all the ancillary files to generate a clean dataset for export #####

### 3a. participant table: payoffs and treatment ####

participants <- df %>% 
  select(ID, 
         treatment = control.1.player.interface,
         final_payoff = participant.payoff)

### 3b. control questions ####

CQ <- df %>% 
  group_by(ID) %>% 
  mutate(CQerrors = sum(control.1.player.failed, control.2.player.failed, control.3.player.failed, na.rm = T)) %>% 
  select(ID, 
         device = control.1.player.device,
         os = control.1.player.os, 
         CQerrors)


#### 3c. main task ####

# isolating the needed varables 
distro <- df %>% 
  select(ID, starts_with("distribution"))

# dropping some unused variables
# and renaming some others
distro <- distro %>%
  # rename(device = distribution.1.player.device,
  #        os = distribution.1.player.os) %>% 
  select(-ends_with("role"),
         -ends_with("group.id_in_subsession"),
         -ends_with("subsession.round_number"),
         -ends_with("player.id_in_group"),
         -ends_with("player.device"),
         -ends_with("player.os")
  ) %>% 
  select(ID,everything()) 

# filtering out 
# - subjects not having visualized the first screen: they did not even start
# - subjects not having visualized the last screen:  they did not finish, closed the tab before the end
distro <- distro %>% 
  filter(!is.na(distribution.1.player.data),
         !is.na(distribution.24.player.data))

# rectangulating
distro <- distro %>% 
  mutate_all(as.character) %>% 
  pivot_longer(starts_with("distr"), 
               names_to = c("key"),
               names_prefix = "distribution.",
               values_to = "val") %>% 
  separate(key, into = c("trial", "dropme", "var")) %>% 
  select(-dropme) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  mutate(payoff = as.numeric(payoff),
         score = as.numeric(score),
         trial = as.numeric(trial))



# some trials have no clicks, so history = "[]"
distro <- distro %>% 
  filter(history != "[]")

# reading in the JSON history structure
distro <- distro %>% 
  group_by(ID, payoff, trial) %>% 
  group_modify(~map_df(.$history, RJSONIO::fromJSON)) %>% 
  arrange(trial)

# one row is created for each of the "data" of bins -- too much
distro <- distro %>% 
  select(-data) %>% 
  distinct()


# generating variables: number of clicks
distro <- distro %>% 
  group_by(ID, trial) %>% 
  # software records one row even if no clicks, so nclicks = nrows - 1
  mutate(nclicks = n()-1)


## merging with screen characteristics
distro <- distro %>% 
  left_join(screens, by = "trial")

## discretizing

# round to the nearest second
distro <- distro %>% 
  mutate(second = ceiling(delay_ms/1000))

# expand to have 1 observation per second
distro <- distro %>% 
  expand(time, second = full_seq(c(1,time), 1)) %>% 
  full_join(distro) 

## sometimes there is more than one choice per second. 
## We keep only the last, that is, the one that was active when the second struck.
distro <- distro %>% 
  ungroup() %>% 
  arrange(ID, trial, second) %>% 
  group_by(ID, trial, second) %>% 
  filter(row_number()==max(row_number()))

## filling the missing elements with a "carry_forward" approach
## since there is ALWAYS a non-missing row at time = 0

distro <- distro %>% 
  ungroup() %>% 
  group_by(ID, trial) %>%
  mutate(payoff = na.locf(payoff, na.rm = F), 
         delay_ms = na.locf(delay_ms, na.rm = F),
         score = na.locf(score, na.rm = F),
         nclicks = na.locf(nclicks, na.rm = F),
         nbins = na.locf(nbins, na.rm = F),
         shape = na.locf(shape, na.rm = F)) 


## some people just did not click anywhere during a trial, dropping those trials
distro <- distro %>% 
  filter(nclicks != 0)

#### 3c bis: slackers ####

# slackers: checking which subjects have "abandoned" at a certain point in time

# this table records the "initial scores"
# in any task by interface
# these are ZERO for click-and-drag, text, slider
# but they are positive for distribution

initial_scores <- distro %>% 
  left_join(participants, by = "ID") %>% 
  ungroup() %>% 
  select(treatment, trial, second, score) %>% 
  filter(second == 0) %>% 
  select(treatment, trial, initial_score = score) %>% 
  distinct()
  
  
slackers <- distro %>% 
  left_join(participants, by = "ID") %>% 
  select(ID, treatment, trial, second, score) %>% 
  left_join(initial_scores, by = c("treatment", "trial")) %>% 
  group_by(ID, treatment, trial) %>% 
  filter(second == max(second)) %>% 
  mutate(not_touched = score - initial_score < 0.05)%>% 
  group_by(ID) %>% 
  mutate(slack = sum(not_touched)) %>% 
  select(ID, slack) %>% 
  distinct() %>% 
  arrange(-slack)


#### 3d. survey ####
surv <- df %>% 
  select(ID, starts_with("survey")) %>% 
  select(-survey.1.player.id_in_group,
         -survey.1.group.id_in_subsession,
         -survey.1.subsession.round_number,
         -survey.1.player.role) %>% 
  rename_all(~stringr::str_replace(.,"^survey.1.player.","")) %>% 
  select(-payoff)

#### 3e. predictions ####

pred <- df %>% 
  select(ID, starts_with("prediction")) %>% 
  select(ID, ends_with("data"), ends_with("history"))

### predictions are sent to a different dataset with a different shape

# 1. rectangulate and keep only needed variables
pred <- pred %>% 
  select(ID, ends_with("data")) %>% 
  rename("prediction_1" = prediction.1.player.data,
         "prediction_2" = prediction.2.player.data) %>% 
  pivot_longer(-ID, names_to = "prediction", names_prefix = "prediction_", values_to = "distribution") 

# 2. unpack the python list
pred <- pred %>% 
  group_by(ID, prediction) %>% 
  mutate(distribution = str_replace(distribution, "^\\[", ""),
         distribution = str_replace(distribution, "\\]", "")) %>% 
  separate(distribution, sep = ",", into = as.character(paste0("bin", seq(1, 31))), convert = T)

# 3. rectangulate again
pred <- pred %>% 
  pivot_longer(starts_with("bin"), names_to = "bin", values_to = "value", names_prefix = "bin")

# adding treatment information
pred <- participants %>% 
  select(ID, treatment) %>% 
  right_join(pred, by = "ID")

pred %>% 
  write_csv("temperature_predictions.csv")

#### 4. Exporting the final dataset ####

final_df <- distro %>% 
  left_join(participants, by = "ID") %>% 
  left_join(CQ, by = "ID") %>% 
  left_join(surv, by = "ID") %>% 
  left_join(slackers, by = "ID")

final_df %>% 
  write_csv("clean_data.csv")