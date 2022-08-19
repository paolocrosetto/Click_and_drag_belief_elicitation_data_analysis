### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## Sample analysis: N, payoffs, answers to Control Questions

# sample: N, demographics and payoffs
demo_table <- df %>%
  select(ID, treatment, age, gender, final_payoff) %>%
  distinct() %>%
  group_by(treatment) %>%
  mutate(female = if_else(gender == "Female", 1, 0)) %>%
  summarise(
    n = n(),
    avg_pay = mean(final_payoff), sd_pay = sd(final_payoff),
    avg_age = mean(age), sd_age = sd(age),
    share_f = paste0(100 * round(mean(female), 2), "%")
  ) %>%
  mutate(
    payoff = paste0(round(avg_pay, 2), " (", round(sd_pay, 2), ")"),
    age = paste0(round(avg_age, 2), " (", round(sd_age, 2), ")")
  ) %>%
  mutate(N = sum(n)) %>%
  select(treatment, N, n, share_f, age, payoff)

# adding the share of people making it on the first try to the table
# number of errors
demo_table <- df %>%
  select(ID, treatment, CQerrors) %>%
  distinct() %>%
  group_by(treatment, CQerrors) %>%
  summarise(n = n()) %>%
  mutate(N = sum(n), share = n / N) %>%
  filter(CQerrors == 0) %>%
  select(treatment, share_noerror = share) %>%
  mutate(share_noerror = paste0(100 * round(share_noerror, 2), "%")) %>%
  right_join(demo_table)

demo_table %>%
  select(-N) %>%
  select(treatment, n, share_f, age, payoff, share_noerror) %>%
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c("lccccc")) %>%
  # kable_styling(full_width = T) %>%
  add_header_above(c(" " = 1, "N" = 1, "% female" = 1, 
                     "mean age (sd)" = 1, "mean payoff (sd)" = 1, 
                     "% no error in CQ")) %>% 
  save_kable("Tables/demo_table.pdf")
