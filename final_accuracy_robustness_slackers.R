### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## final scores
final <- df %>%
  group_by(ID, trial) %>%
  filter(second == max(second))

# filtering out subjects slacking on more than 3 tasks
final <- final %>%
  filter(slack <= 3)

# overall results table
overall <- final %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  mutate(
    m = round(100 * mean, 2),
    cci = round(100 * ci, 2)
  ) %>%
  mutate(indicator = paste0(m, " [", m - cci, ",", m + cci, "]")) %>%
  select(treatment, indicator) %>%
  mutate(type = "") %>%
  pivot_wider(names_from = treatment, values_from = indicator)

bytime <- final %>%
  group_by(treatment, time) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  mutate(
    m = round(100 * mean, 2),
    cci = round(100 * ci, 2)
  ) %>%
  mutate(indicator = paste0(m, " [", m - cci, ",", m + cci, "]")) %>%
  select(treatment, type = time, indicator) %>%
  pivot_wider(names_from = treatment, values_from = indicator) %>%
  arrange(fct_rev(type))

byshape <- final %>%
  group_by(treatment, shape) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  mutate(
    m = round(100 * mean, 2),
    cci = round(100 * ci, 2)
  ) %>%
  mutate(indicator = paste0(m, " [", m - cci, ",", m + cci, "]")) %>%
  select(treatment, type = shape, indicator) %>%
  pivot_wider(names_from = treatment, values_from = indicator)

bybins <- final %>%
  group_by(treatment, nbins) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  mutate(
    m = round(100 * mean, 2),
    cci = round(100 * ci, 2)
  ) %>%
  mutate(indicator = paste0(m, " [", m - cci, ",", m + cci, "]")) %>%
  select(treatment, type = nbins, indicator) %>%
  pivot_wider(names_from = treatment, values_from = indicator)

overalls <-
  bind_rows(overall, bytime, bybins, byshape)

# export N
final %>%
  ungroup() %>%
  select(treatment, ID) %>%
  distinct() %$%
  table(treatment)

# export table
sink("Tables/overall_results_noslack.tex")
overalls %>%
  kbl(booktabs = TRUE, format = "latex", col.names = NULL, align = c("lccccc")) %>%
  pack_rows("Overall", 1, 1) %>%
  pack_rows("by time constraint", 2, 3) %>%
  pack_rows("by number of bins", 4, 6) %>%
  pack_rows("by shape", 7, 10) %>%
  add_header_above(c(
    " " = 1, "Click-and-drag\n(N = 90)" = 1, "Slider\n(N = 83)" = 1,
    "Text\n(N = 47)" = 1, "Distribution\n(N = 2)" = 1
  ))
sink()
