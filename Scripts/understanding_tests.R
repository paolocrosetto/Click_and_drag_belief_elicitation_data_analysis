### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

understanding_test <- df %>%
  select(ID, treatment, easy, frustrating, understood) %>%
  distinct() %>%
  pivot_longer(-ID & -treatment, names_to = "var", values_to = "value")

## non parametric
understanding_test %>%
  group_by(var) %>%
  group_modify(~ pairwise.wilcox.test(.$value, .$treatment, p.adjust.method = "none") %>%
    tidy() %>%
    mutate(p.value = round(p.value, 2)))


## parametric

# test
understanding_test %>%
  group_by(var) %>%
  group_modify(~ pairwise.t.test(.$value, .$treatment, p.adjust.method = "none") %>%
                 tidy() %>%
                 mutate(p.value = round(p.value, 2)))

# cohen
cohen_understand <- understanding_test %>%
  group_by(var) %>%
  group_modify(~ coh_d(data = ., formula = value ~ treatment))
