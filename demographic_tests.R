### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

# testing the demographics and payoffs across treatments

test_demo <- df %>%
  select(ID, treatment, age, gender, final_payoff) %>%
  distinct()

# gender
kruskal.test(gender ~ treatment, data = test_demo) %>% tidy()

# age
kruskal.test(age ~ treatment, data = test_demo) %>% tidy() ## significant

# why is age significant?
# click vs slider: sign
wilcox.test(
  test_demo$age[test_demo$treatment == "Click-and-drag"],
  test_demo$age[test_demo$treatment == "Slider"]
) %>% tidy()

# click vs text: NS
wilcox.test(
  test_demo$age[test_demo$treatment == "Click-and-drag"],
  test_demo$age[test_demo$treatment == "Text"]
) %>% tidy()

# click vs distro: NS
wilcox.test(
  test_demo$age[test_demo$treatment == "Click-and-drag"],
  test_demo$age[test_demo$treatment == "Distribution"]
) %>% tidy()

# slider vs Text: sign
wilcox.test(
  test_demo$age[test_demo$treatment == "Slider"],
  test_demo$age[test_demo$treatment == "Text"]
) %>% tidy()

# slider vs distribution: sign
wilcox.test(
  test_demo$age[test_demo$treatment == "Slider"],
  test_demo$age[test_demo$treatment == "Distribution"]
) %>% tidy()

# distribution vs Text: NS
wilcox.test(
  test_demo$age[test_demo$treatment == "Text"],
  test_demo$age[test_demo$treatment == "Distribution"]
) %>% tidy()

# payoffs
# age
kruskal.test(final_payoff ~ treatment, data = test_demo) %>% tidy() ## significant

# why is age significant?
# click vs slider: sign
wilcox.test(
  test_demo$final_payoff[test_demo$treatment == "Click-and-drag"],
  test_demo$final_payoff[test_demo$treatment == "Slider"]
) %>% tidy()

# click vs text: NS
wilcox.test(
  test_demo$final_payoff[test_demo$treatment == "Click-and-drag"],
  test_demo$final_payoff[test_demo$treatment == "Text"]
) %>% tidy()

# click vs distro: NS
wilcox.test(
  test_demo$final_payoff[test_demo$treatment == "Click-and-drag"],
  test_demo$final_payoff[test_demo$treatment == "Distribution"]
) %>% tidy()

# slider vs Text: sign
wilcox.test(
  test_demo$final_payoff[test_demo$treatment == "Slider"],
  test_demo$final_payoff[test_demo$treatment == "Text"]
) %>% tidy()

# slider vs distribution: sign
wilcox.test(
  test_demo$final_payoff[test_demo$treatment == "Slider"],
  test_demo$final_payoff[test_demo$treatment == "Distribution"]
) %>% tidy()

# distribution vs Text: NS
wilcox.test(
  test_demo$final_payoff[test_demo$treatment == "Text"],
  test_demo$final_payoff[test_demo$treatment == "Distribution"]
) %>% tidy()


# testing control questions
df <- df %>%
  mutate(noerrors = CQerrors == 0)

# not sig
kruskal.test(noerrors ~ treatment, data = df %>% select(ID, treatment, noerrors) %>% distinct()) %>% tidy()
