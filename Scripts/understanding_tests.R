### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

understanding_test <- df %>%
  select(ID, treatment, easy, frustrating, understood) %>%
  distinct() %>%
  pivot_longer(-ID & -treatment, names_to = "var", values_to = "value")

understanding_test %>%
  group_by(var) %>%
  filter(treatment %in% c("Click-and-Drag", "Slider")) %>%
  group_modify(~ wilcox.test(value ~ treatment, data = .) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))


understanding_test %>%
  group_by(var) %>%
  filter(treatment %in% c("Click-and-Drag", "Text")) %>%
  group_modify(~ wilcox.test(value ~ treatment, data = .) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))


understanding_test %>%
  group_by(var) %>%
  filter(treatment %in% c("Click-and-Drag", "Distribution")) %>%
  group_modify(~ wilcox.test(value ~ treatment, data = .) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))
