### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

# testing the demographics and payoffs across treatments

test_demo <- df %>%
  select(ID, treatment, age, gender, final_payoff) %>%
  distinct()

#### gender ####
# non-parametric
kruskal.test(gender ~ treatment, data = test_demo) %>% tidy()

# parametric
chisq.test(test_demo$gender, test_demo$treatment) %>% tidy()

#### age ####

## non-parametric

kruskal.test(age ~ treatment, data = test_demo) %>% tidy() ## significant

# why is age significant?

pairwise.wilcox.test(test_demo$age, test_demo$treatment, p.adjust.method = "none")



## parametric
anova(lm(age ~ treatment, data = test_demo)) %>% tidy() ## significant


# why is age significant?
# click vs slider: sign
age_test <- mypairedt(test_demo, "age", "treatment")


age_cohen <- test_demo %>% 
  coh_d(age~treatment)
  
age_test %>% 
  left_join(age_cohen, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
  arrange(p.value)



#### payoffs ####

## non parametric
kruskal.test(final_payoff ~ treatment, data = test_demo) %>% tidy() ## significant

# why is final payoff significant?
# click vs slider: sign
pairwise.wilcox.test(test_demo$final_payoff, test_demo$treatment, p.adjust.method = "none") %>% tidy()

## parametric
anova(lm(final_payoff ~ treatment, data = test_demo)) %>% tidy() ## significant


# why is payoff difference significant?
# click vs slider: sign
pay_test <- mypairedt(test_demo, "final_payoff", "treatment")

pay_cohen <- test_demo %>% 
  coh_d(final_payoff~treatment)

pay_test %>% 
  left_join(pay_cohen, by = c("group1" = "treatment_ref", "group2" = "treatment_foc")) %>% 
  arrange(p.value) %>% 
  mutate(p.value = round(p.value, 3))

#### control questions ####

## non parametric
df <- df %>%
  mutate(noerrors = CQerrors == 0)

# not sig
kruskal.test(noerrors ~ treatment, data = df %>% select(ID, treatment, noerrors) %>% distinct()) %>% tidy()


## parametric
chisq_data <- df %>% 
  select(ID, treatment, noerrors) %>% 
  distinct()  

chisq.test(chisq_data$noerrors, chisq_data$treatment) %>% tidy()
