### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

# counting share of slackers by treatment and by degree of slack

slackers <- df %>%
  select(ID, treatment, slack) %>%
  distinct() %>%
  mutate(slacker = cut(slack,
    breaks = c(-1, 0, 3, 10, 15, 25),
    labels = c(
      "No", "Somewhat (1-3)", "Moderate (4-10)",
      "Serious (11-15)", "Severe (15-24)"
    )
  )) %>%
  group_by(treatment, slacker) %>%
  summarise(n = n()) %>%
  mutate(N = sum(n), share = paste0(round(100 * n / N, 2), "%")) %>%
  select(-n, -N) %>%
  pivot_wider(names_from = slacker, values_from = share, values_fill = "0%")

# add mean slack
meanslack <- df %>%
  select(ID, treatment, slack) %>%
  distinct() %>%
  group_by(treatment) %>%
  summarise(m = mean(slack), s = sd(slack)) %>%
  mutate(mean_slack = paste0(round(m, 2), " (", round(s, 2), ")")) %>%
  select(treatment, mean_slack)

sink("Tables/slacker_table.tex")
slackers %>%
  left_join(meanslack) %>%
  select(treatment, mean_slack, everything()) %>%
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c("lcccccc")) %>%
  # kable_styling(full_width = T) %>%
  add_header_above(c(
    " " = 1, "Mean Slack", "No" = 1, "Somewhat\n(1-3)" = 1,
    "Moderate\n(4-10)" = 1, "Serious\n(11-15)" = 1,
    "Severe\n(15-24)" = 1
  )) %>%
  add_header_above(c(" " = 1, " " = 1, "Distribution of slackers by type" = 5))
sink()
