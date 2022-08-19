### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

# counting share of slackers by treatment and by degree of slack

slackers <- df %>%
  select(ID, treatment, slack) %>%
  distinct()

# click vs slider: sign
wilcox.test(
  slackers$slack[slackers$treatment == "Click-and-Drag"],
  slackers$slack[slackers$treatment == "Slider"]
) %>% tidy()

# click vs text: NS
wilcox.test(
  slackers$slack[slackers$treatment == "Click-and-Drag"],
  slackers$slack[slackers$treatment == "Text"]
) %>% tidy()

# click vs distro: NS
wilcox.test(
  slackers$slack[slackers$treatment == "Click-and-Drag"],
  slackers$slack[slackers$treatment == "Distribution"]
) %>% tidy()
