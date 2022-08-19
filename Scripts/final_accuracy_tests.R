### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## final scores
final <- df %>%
  group_by(ID, trial) %>%
  filter(second == max(second))



# overall accuracy difference
kruskal.test(score ~ treatment, data = final) %>% tidy() ## significant

# Click-and-Drag vs all others
wilcox.test(
  final$score[final$treatment == "Click-and-Drag"],
  final$score[final$treatment == "Slider"]
) %>% tidy()

wilcox.test(
  final$score[final$treatment == "Click-and-Drag"],
  final$score[final$treatment == "Text"]
) %>% tidy()

wilcox.test(
  final$score[final$treatment == "Click-and-Drag"],
  final$score[final$treatment == "Distribution"]
) %>% tidy()

# loss in performance 45 vs 15
perfloss <- final %>%
  group_by(ID, treatment, shape, nbins, time) %>%
  select(score) %>%
  pivot_wider(names_from = time, values_from = score) %>%
  filter(!is.na(`15 seconds`)) %>%
  filter(!is.na(`45 seconds`)) %>%
  mutate(diff = `45 seconds` - `15 seconds`)

# Click-and-Drag vs all others
wilcox.test(
  perfloss$diff[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff[perfloss$treatment == "Slider"]
) %>% tidy()

wilcox.test(
  perfloss$diff[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff[perfloss$treatment == "Text"]
) %>% tidy()

wilcox.test(
  perfloss$diff[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff[perfloss$treatment == "Distribution"]
) %>% tidy()


# loss in perf 7 vs 15

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

# Click-and-Drag vs all others
wilcox.test(
  perfloss$diff_7_15[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff_7_15[perfloss$treatment == "Slider"]
) %>% tidy()

wilcox.test(
  perfloss$diff_7_15[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff_7_15[perfloss$treatment == "Text"]
) %>% tidy()

wilcox.test(
  perfloss$diff_7_15[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff_7_15[perfloss$treatment == "Distribution"]
) %>% tidy()

wilcox.test(
  perfloss$diff_15_30[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff_15_30[perfloss$treatment == "Slider"]
) %>% tidy()

wilcox.test(
  perfloss$diff_15_30[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff_15_30[perfloss$treatment == "Text"]
) %>% tidy()

wilcox.test(
  perfloss$diff_15_30[perfloss$treatment == "Click-and-Drag"],
  perfloss$diff_15_30[perfloss$treatment == "Distribution"]
) %>% tidy()



# tests by shape
final %>%
  group_by(shape) %>%
  filter(treatment %in% c("Click-and-Drag", "Slider")) %>%
  group_modify(~ wilcox.test(score ~ treatment, data = .) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))

final %>%
  group_by(shape) %>%
  filter(treatment %in% c("Click-and-Drag", "Text")) %>%
  group_modify(~ wilcox.test(score ~ treatment, data = .) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))

final %>%
  group_by(shape) %>%
  filter(treatment %in% c("Click-and-Drag", "Distribution")) %>%
  group_modify(~ wilcox.test(score ~ treatment, data = .) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))
