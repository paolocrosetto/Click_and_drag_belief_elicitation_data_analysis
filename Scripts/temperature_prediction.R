### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## Analysis of the 2 prediction screens for NYC temperature, July 4th 2022 and 2042

pred <- read_csv("Data/temperature_predictions_data.csv")


#### 0. clean data and generate variables ####

# making stuff look good
pred <- pred %>%
  mutate(
    treatment = as.factor(treatment),
    treatment = fct_recode(treatment,
      "Click-and-drag" = "ours",
      "Slider" = "bins",
      "Text" = "number",
      "Distribution" = "metaculus"
    ),
    treatment = fct_relevel(treatment, "Click-and-drag", "Slider", "Text")
  ) %>%
  mutate(
    prediction = as.factor(prediction),
    prediction = fct_recode(prediction, "4th July 2022" = "1", "4th July 2042" = "2")
  )

# adding the default values for each bin 
# ie what the bin looked like before subjects touched the interface
# this is zero for all interfaces but Distribution
# for Distribution we use an ancillary data file created by running the software without input

default_values <- read_csv("Data/default_value_NYC_Distribution.csv")

# adding needed variables
default_values <- default_values %>% 
  mutate(treatment = "Distribution")

# merging and taking care of NAs
pred <- pred %>% 
  left_join(default_values, by = c("treatment", "bin")) %>% 
  mutate(default_value = if_else(is.na(default_value), 0, default_value))

# write down the bins as seen by the subjects
pred <- pred %>%
  mutate(bin = 60 + (bin - 1) * 2)

# normalize the bins
pred <- pred %>%
  group_by(ID, prediction) %>%
  mutate(sumbins = sum(value)) %>%
  mutate(normalized = value / sumbins)

## identify slackers; all treats but Distribtion

slack_all_but_Distribution <- pred %>% 
  filter(sumbins == 0) %>% 
  ungroup() %>% 
  select(ID, treatment) %>% 
  distinct()

## identify slackers: Distribution
slack_distribution <- pred %>% 
  filter(treatment == "Distribution") %>% 
  mutate(diff = abs(normalized - default_value)) %>% 
  group_by(ID, treatment, prediction) %>% 
  summarise(diff = sum(diff)) %>% 
  filter(diff <= 0.01) %>% 
  select(ID, treatment) %>% 
  distinct()

## all slackers
slackers <- slack_all_but_Distribution %>% 
  bind_rows(slack_distribution) %>% 
  pull(ID)

## cleanup
rm(slack_all_but_Distribution, slack_distribution)



# number of slackers by treatment
pred <- pred %>%
  mutate(slacker = ID %in% slackers)


#### 1. Number of slackers on the NYC temperatue screens by treatment ####

# % share of slackers
pred %>%
  distinct() %>%
  arrange(-slacker) %>%
  group_by(treatment, slacker) %>%
  summarise(n = n()) %>%
  mutate(N = sum(n), share = round(100 * n / N, 2)) %>%
  filter(slacker == TRUE) %>%
  select(treatment, share) %>%
  pivot_wider(names_from = treatment, values_from = share, values_fill = 0)


#### 2. Visually inspecting the distributions for each subject #####

## extra plots to inspect the submitted distributions

# function to generate the visual inspection plots (including slackers)
inspect_plot <- function(treat) {
  plot <- pred %>% 
    filter(treatment == treat) %>% 
    ggplot(aes(bin, normalized, color = prediction))+
    geom_line(linewidth = 1.3)+
    facet_wrap(~ID, scales = "free_y")+
    scale_color_brewer(name = "", palette = "Set1", direction = -1)+
    theme_minimal()+
    labs(title = paste0("Individual temperature predictions -- ", treat))+
    theme(legend.position = "bottom", plot.background = element_rect(colour = "white", fill = "white"), 
          plot.title = element_text(face = "bold", size = 36, hjust = 0.5), 
          plot.title.position = "plot", 
          legend.text = element_text(size = 26))
  
  ggsave(paste0("Extra_Figures/Individual_distributions_", treat, ".png"), width = 36, height = 18, units = "in", dpi = 300)
}

inspect_plot("Click-and-drag")
inspect_plot("Slider")
inspect_plot("Text")
inspect_plot("Distribution")



#### 3. comparing the distributions by treatment ####

### 3.1: plot of the distributions by treatment

# compute the mean prediction by bin and the overall estimate
plotme <- pred %>%
  filter(slacker == F) %>%
  filter(!is.na(normalized)) %>%
  group_by(treatment, prediction, bin) %>%
  summarise(
    value = mean(normalized),
    sd = sd(normalized),
    ci = sd(normalized, na.rm = T) / sqrt(n()) * qt(.95 / 2 + .5, n())
  ) %>%
  mutate(
    wsum = bin * value,
    wsummax = bin * (value + ci),
    wsummin = bin * (value - ci),
    estimate = sum(wsum),
    estimatemax = sum(wsummax),
    estimatemin = sum(wsummin)
  )


# aggregate over bins to get a decent overall picture
plotme %>%
  ggplot(aes(bin, value, color = prediction, fill = prediction)) +
  geom_ribbon(aes(ymin = value - ci, ymax = value + ci), color = NA, alpha = 0.3) +
  geom_line() +
  geom_vline(aes(xintercept = estimate, color = prediction), linetype = "dashed", show.legend = F) +
  # geom_vline(aes(xintercept = 83.6))+
  # geom_vline(aes(xintercept = 82.3))+
  # geom_vline(aes(xintercept = 83.6))+
  facet_grid(treatment ~ .) +
  labs(y = "probability", x = "°F") +
  scale_x_continuous(breaks = seq(60, 120, 10)) +
  scale_color_brewer(palette = "Set1", direction = -1, name = "") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "") +
  guides(fill = "none") +
  theme_ipsum() +
  annotate(geom = "segment", x = 85, y = 0, yend = 0.1, xend = 85)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
ggsave("Figures/NYC_temperature_predictions.png",
       width = 16 / 1.7, height = 12 / 1.7, dpi = 300, units = "in"
)

## 3.2. table of mean and conf_int of the treatment-aggregate distributions

plotme %>% 
  mutate(confint = estimatemax - estimatemin) %>% 
  select(treatment, prediction, confint) %>% 
  distinct()

# mean and CI of predictions
plotme %>%
  select(treatment, prediction, starts_with("estimate")) %>%
  distinct() %>%
  mutate(indicator = paste0(round(estimate, 2), " [", round(estimatemin, 2), ",", round(estimatemax, 2), "]")) %>%
  select(treatment, prediction, indicator) %>%
  pivot_wider(names_from = treatment, values_from = indicator) %>%
  kbl(booktabs = TRUE, format = "latex", col.names = NULL, align = c("lcccc")) %>%
  add_header_above(c(
    " " = 1, "Click-and-drag" = 1, "Distribution" = 1,
    "Slider" = 1, "Text" = 1
  )) %>% 
  save_kable("Tables/NYC_predictions.pdf")

## 3.3 concentration of mass into one bin

## clusters: number of bins with positive mass
pos_mass <- pred %>% 
  filter(slacker == F) %>% 
  mutate(positive_mass = normalized > 0) %>% 
  group_by(ID, prediction, treatment) %>% 
  summarise(Nbins = sum(positive_mass)) %>% 
  arrange(prediction, Nbins)

# which share of subjects concentrated mass on how many bins?
concentration <- pos_mass %>% 
  group_by(Nbins, treatment) %>% 
  tally() %>% 
  group_by(treatment) %>% 
  mutate(share = 100*n / sum(n))

concentration %>% 
  filter(Nbins == 1)
  

## 3.4 computing scorign rules

scoring <- pred %>% 
  ungroup() %>% 
  filter(slacker == F) %>% 
  filter(prediction == "4th July 2022") %>% 
  select(ID, treatment, prediction, bin, prob = normalized) %>% 
  mutate(correct_bin = bin == 84) %>% 
  distinct()

## scoring rules
##
## QSR: 0.5 + prob(correct) - sum(O.5*(prob(incorrect))^2)

QSR <- scoring %>% 
  group_by(ID, treatment) %>% 
  mutate(step1 = (correct_bin)*prob, 
         step2 = 0.5*((!correct_bin)*prob)^2) %>% 
  summarise(QSR = 0.5 + sum(step1) - sum(step2)) %>% 
  distinct()

## spherical
## prob.correct.bin / (sqrt(sum(prob.each.bin squared))

SSR <- scoring %>% 
  group_by(ID, treatment) %>% 
  mutate(step1 = (correct_bin)*prob,
         step2 = prob^2) %>% 
  summarise(SSR = sum(step1)/sqrt(sum(step2)))


df_sr <- QSR %>% 
  left_join(SSR, by = c("ID", "treatment"))


## means and st.dev by treatment of the scoring rules
scores <- df_sr %>% 
  group_by(treatment) %>% 
  summarise(across(ends_with("SR"), ~ paste0(round(mean(.x),3), 
                                             " (", 
                                             round(sd(.x), 3), 
                                             ")")))


scores %>% 
  kbl(booktabs = TRUE, format = "latex", col.names = NULL, align = c("lcc")) %>%
  add_header_above(c(
    " " = 1, "Quadratic" = 1, "Spherical" = 1
  )) %>% 
  save_kable("Tables/scoring_rules.pdf")  

## are those differeces significant?
df_sr %>% 
  pivot_longer(ends_with("SR"), names_to = "rule", values_to = "value") %>% 
  group_by(rule) %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  group_modify(~paired_plus_cohen(., "value", "treatment"))


## mean estimates by treatment

# mean predictions by subject
mean_estimates <- pred %>% 
  filter(slacker == F) %>%
  filter(!is.na(normalized)) %>%
  mutate(wsum = bin * normalized) %>% 
  group_by(ID, treatment, prediction) %>% 
  summarise(estimate = sum(wsum))

# test: was 2022 accurate wrt to reality?
mean_estimates %>% 
  filter(prediction == "4th July 2022") %>% 
  group_by(treatment) %>% 
  group_modify(~tidy(t.test(.$estimate, mu = 85))) %>% 
  mutate(p.value = round(p.value, 5))


## mass on the winning bin
plotme %>% 
  filter(prediction == "4th July 2022") %>% 
  filter(bin == 84) %>% 
  select(treatment, value) %>% 
  mutate(value = round(value, 3)) %>% 
  arrange(-value)


# expected global warming
plotme %>%
  select(treatment, estimate) %>%
  distinct() %>%
  pivot_wider(names_from = prediction, values_from = estimate) %>%
  mutate(warming = `4th July 2042` - `4th July 2022`) %>%
  ungroup() %>%
  summarise(mean_warm = mean(warming))
