### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## final scores
final <- df %>%
  group_by(ID, trial) %>%
  filter(second == max(second)) %>%
  filter(slack <= 3)

final %>%
  ungroup() %>%
  select(ID, treatment) %>%
  distinct() %>%
  group_by(treatment) %>%
  summarise(n())


#### overall accuracy difference ####

## non-parametric
kruskal.test(score ~ treatment, data = final) %>% tidy() 

## parametric
anova(lm(score ~ treatment, data = final)) %>% tidy()

# Click-and-Drag vs all others

## non-parametric
pairwise.wilcox.test(final$score, final$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

## parametric

paired_plus_cohen(final, "score", "treatment")


#### loss in performance 45 vs 15 ####

perfloss <- final %>%
  group_by(ID, treatment, shape, nbins, time) %>%
  select(score) %>%
  pivot_wider(names_from = time, values_from = score) %>%
  filter(!is.na(`15 seconds`)) %>%
  filter(!is.na(`45 seconds`)) %>%
  mutate(diff = `45 seconds` - `15 seconds`)

# Click-and-Drag vs all others: non paramteric
pairwise.wilcox.test(perfloss$diff, perfloss$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

# Click-and-Drag vs all others: paramteric
paired_plus_cohen(perfloss, "diff", "treatment")

#### loss in performance in bins: 7 vs 15 vs 30 bins ####

perfloss <- final %>%
  group_by(ID, treatment, shape, nbins, time) %>%
  select(score) %>%
  pivot_wider(names_from = nbins, values_from = score) %>%
  filter(!is.na(`7 bins`)) %>%
  filter(!is.na(`15 bins`)) %>%
  filter(!is.na(`30 bins`)) %>%
  mutate(
    diff_7_15 = `7 bins` - `15 bins`,
    diff_15_30 = `15 bins` - `30 bins`
  )

# Click-and-Drag vs all others: non parametric
# 7 vs 15
pairwise.wilcox.test(perfloss$diff_7_15, perfloss$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

#15 vs 30
paired_plus_cohen(perfloss, "diff_15_30", "treatment")

#7 vs 15
paired_plus_cohen(perfloss, "diff_7_15", "treatment")

##### tests by shape ####


## non parametric
shape_wilcox <- final %>%
  group_by(shape) %>%
  group_modify(~ pairwise.wilcox.test(.$score, .$treatment, p.adjust.method = "none") %>%
                 tidy() %>%
                 mutate(p.value = round(p.value, 2)))

## parametric
final %>%
  group_by(shape) %>%
  group_modify(~ paired_plus_cohen(., "score", "treatment"))

