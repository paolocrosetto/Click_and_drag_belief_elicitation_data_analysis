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
acc_test <- pairwise.t.test(final$score, final$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

acc_cohen <- final %>% 
  coh_d(score~treatment)

acc_test %>% 
  left_join(acc_cohen, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
  arrange(p.value) %>% 
  mutate(p.value = round(p.value, 3))


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
loss_test <- pairwise.t.test(perfloss$diff, perfloss$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

loss_cohen <- perfloss %>% 
  coh_d(diff~treatment)

loss_test %>% 
  left_join(loss_cohen, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
  arrange(p.value) %>% 
  mutate(p.value = round(p.value, 3))

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
test_7_15 <- pairwise.t.test(perfloss$diff_7_15, perfloss$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

cohen_7_15 <- perfloss %>% 
  coh_d(diff_7_15~treatment)

test_7_15 %>% 
  left_join(cohen_7_15, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
  arrange(p.value) %>% 
  mutate(p.value = round(p.value, 3))

#15 vs 30
test_15_30 <- pairwise.t.test(perfloss$diff_15_30, perfloss$treatment, p.adjust.method = "none") %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

cohen_15_30 <- perfloss %>% 
  coh_d(diff_15_30~treatment)

test_15_30 %>% 
  left_join(cohen_15_30, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
  arrange(p.value) %>% 
  mutate(p.value = round(p.value, 3))

##### tests by shape ####


## non parametric
shape_wilcox <- final %>%
  group_by(shape) %>%
  group_modify(~ pairwise.wilcox.test(.$score, .$treatment, p.adjust.method = "none") %>%
                 tidy() %>%
                 mutate(p.value = round(p.value, 2)))

## parametric
shape_t <- final %>%
  group_by(shape) %>%
  group_modify(~ pairwise.t.test(.$score, .$treatment, p.adjust.method = "none") %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))

shape_cohen <- final %>%
  group_by(shape) %>%
  group_modify(~ coh_d(data = ., formula = score ~ treatment))
