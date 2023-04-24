### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

# counting share of slackers by treatment and by degree of slack

slackers <- df %>%
  select(ID, treatment, slack) %>%
  distinct()

## non parametric
pairwise.wilcox.test(slackers$slack, slackers$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 4))

## parametric 

# test
pairwise.t.test(slackers$slack, slackers$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 4))

# cohen
slackers %>% 
  coh_d(slack ~ treatment)


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
