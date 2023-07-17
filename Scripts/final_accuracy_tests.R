### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## final scores
final <- df %>%
  group_by(ID, trial) %>%
  filter(second == max(second))


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

## parametric, averaging over subjects first to avoid assumption of independence within subjects

correct_test_overall <- final %>% 
  group_by(ID, treatment) %>% 
  summarise(score = mean(score, na.rm = T)) %>% 
  paired_plus_cohen("score", "treatment") %>% 
  filter(group1 == "Click-and-Drag") %>% 
  mutate(var = 'Overall')



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

# same but averaging over subjects
correct_test_45_15 <- perfloss %>% 
  group_by(ID, treatment) %>% 
  summarise(diff = mean(diff, na.rm = T)) %>% 
  paired_plus_cohen("diff", "treatment")%>% 
  filter(group1 == "Click-and-Drag")%>% 
  mutate(var = 'Diff 45 vs 15 sec')

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
pairwise.wilcox.test(perfloss$diff_15_30, perfloss$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

# Click-and-Drag vs all others: parametric
paired_plus_cohen(perfloss, "diff_7_15", "treatment")

## same but with averages by subject
correct_test_7_15 <- perfloss %>% 
  group_by(ID, treatment) %>% 
  summarise(diff_7_15 = mean(diff_7_15, na.rm = T)) %>% 
  paired_plus_cohen("diff_7_15", "treatment")%>% 
  filter(group1 == "Click-and-Drag")%>% 
  mutate(var = 'Diff 7 vs 15 bins')

#15 vs 30
paired_plus_cohen(perfloss, "diff_15_30", "treatment")

## same but with averages by subject
correct_test_15_30 <- perfloss %>% 
  group_by(ID, treatment) %>% 
  summarise(diff_15_30 = mean(diff_15_30, na.rm = T)) %>% 
  paired_plus_cohen("diff_15_30", "treatment")%>% 
  filter(group1 == "Click-and-Drag")%>% 
  mutate(var = 'Diff 15 vs 30 bins')

##### tests by shape ####


## non parametric
shape_wilcox <- final %>%
  group_by(shape) %>%
  group_modify(~ pairwise.wilcox.test(.$score, .$treatment, p.adjust.method = "none") %>%
                 tidy() %>%
                 mutate(p.value = round(p.value, 2)))

## parametric
shape_t_cohen <- final %>%
  group_by(shape) %>%
  group_modify(~ paired_plus_cohen(., "score", "treatment"))


## parametric averaging by subject

correct_test_shape <- final %>% 
  group_by(ID, treatment, shape) %>% 
  summarise(score = mean(score, na.rm = T)) %>% 
  group_by(shape) %>%
  group_modify(~ paired_plus_cohen(., "score", "treatment"))%>% 
  filter(group1 == "Click-and-Drag")%>% 
  mutate(var = paste0('Shape: ', shape)) %>% 
  ungroup() %>% 
  select(-shape)


## exporting a table for the appendix with the tests averaged over subjects

tests_by_subject_table <- 
  bind_rows(correct_test_overall, correct_test_45_15, correct_test_7_15, 
            correct_test_15_30, correct_test_shape) %>% 
  select(var, group2, DoF = parameter, statistic, p.value, coh_d)

# nice table export
tests_by_subject_table %>% 
  select(-var) %>% 
  kbl(booktabs = TRUE, format = "latex", col.names = NULL) %>% 
  pack_rows("Overall", 1, 3) %>% 
  pack_rows("Difference 45 vs 15 seconds", 4, 6) %>% 
  pack_rows("Difference 7 vs 15 bins", 7, 9) %>% 
  pack_rows("Difference 15 vs 30 bins", 10, 12) %>% 
  pack_rows("Shape: Symmetric", 13, 15) %>% 
  pack_rows("Shape: Skewed", 16, 18) %>% 
  pack_rows("Shape: Bimodal", 19, 21) %>% 
  pack_rows("Shape: Random", 22, 24) %>% 
  add_header_above(c(" " = 1, " DoF" = 1, "Stat" = 1, "p" = 1, "d" = 1)) %>% 
  save_kable("Tables/tests_by_subject.pdf")
  
  
  
  
  
