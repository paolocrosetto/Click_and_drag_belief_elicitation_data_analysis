### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## Analysis of the 2 prediction screens for NYC temperature, July 4th 2022 and 2042

pred <- read_csv("Data/temperature_predictions_data.csv")

# making stuff look good
pred <- pred %>% 
  mutate(treatment = as.factor(treatment),
         treatment = fct_recode(treatment, "Click-and-drag" = "ours",
                                "Slider"         = "bins",
                                "Text"           = "number",
                                "Distribution"   = "metaculus"),
         treatment = fct_relevel(treatment, "Click-and-drag", "Slider", "Text")) %>% 
  mutate(prediction = as.factor(prediction), 
         prediction = fct_recode(prediction, "4th July 2022" = "1", "4th July 2042" = "2"))

# write down the bins as seen by the subjects
pred <- pred %>% 
  mutate(bin = 60 + (bin-1)*2)

# normalize the bins
pred <- pred %>% 
  group_by(ID, prediction) %>% 
  mutate(sumbins = sum(value)) %>% 
  mutate(normalized = value/sumbins)

# number of slackers by treatment
pred <- pred %>% 
  mutate(slacker = sumbins < 0.05)

pred %>% 
  distinct() %>% 
  arrange(-slacker) %>% 
  group_by(treatment, prediction, slacker) %>% 
  summarise(n = n()) %>% 
  mutate(N = sum(n), share = round(100*n/N,2)) %>% 
  filter(slacker == TRUE) %>% 
  select(treatment, prediction, share) %>% 
  pivot_wider(names_from = treatment, values_from = share, values_fill = 0)

# compute the mean prediction by bin and the overall estimate 
pred <- pred %>% 
  filter(slacker == F) %>% 
  filter(!is.na(normalized)) %>% 
  group_by(treatment, prediction, bin) %>% 
  summarise(value = mean(normalized),
            ci = sd(normalized, na.rm = T)/sqrt(n())*qt(.95/2 + .5, n())) %>% 
  mutate(wsum = bin*value, 
         wsummax = bin*(value+ci), 
         wsummin = bin*(value-ci),
         estimate = sum(wsum),
         estimatemax = sum(wsummax), 
         estimatemin = sum(wsummin)) 


# temp data 
NYC <- tibble(xmin = 82.3, x = 83.6, xmax = 84.9, ymin = 0, ymax = 0.075, yend = 0.075)

# aggregate over bins to get a decent overall picture
pred %>% 
  ggplot(aes(bin, value, color = prediction, fill = prediction))+
  geom_ribbon(aes(ymin = value-ci, ymax = value + ci), color = NA, alpha = 0.3)+
  geom_line()+
  geom_vline(aes(xintercept = estimate, color = prediction), linetype = "dashed", show.legend = F)+
  # geom_vline(aes(xintercept = 83.6))+
  # geom_vline(aes(xintercept = 82.3))+
  # geom_vline(aes(xintercept = 84.9))+
  facet_grid(treatment~.)+
  labs(y = "probability", x = "°F")+
  scale_x_continuous(breaks = seq(60, 120, 10))+
  scale_color_brewer(palette = "Set1", direction = -1, name = "")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "")+
  guides(fill = "none")+
  theme_ipsum()+
  geom_rect(data = NYC, inherit.aes = F, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "black", alpha = 0.2, show.legend = T)+
  # annotate(geom = "rect", xmin = 82.3, xmax = 84.9, ymin = 0, ymax = 0.075, fill = "black", alpha = 0.2)+
  # annotate(geom = "segment", x = 83.6, y = 0, yend = 0.075, xend = 83.6)+
  #annotate(geom = "segment", x = 85, y = 0, yend = 0.075, xend = 85, color = "purple")+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank())
ggsave("Figures/NYC_temperature_predictions.png",
       width = 16/1.7, height = 12/1.7, dpi = 300, units = "in")

# mean and CI of predictions
sink("Tables/NYC_predictions.tex")
pred %>% 
  select(treatment, prediction, starts_with("estimate")) %>% 
  distinct() %>% 
  mutate(indicator = paste0( round(estimate, 2), " [", round(estimatemin, 2),",",round(estimatemax,2),"]")) %>% 
  select(treatment, prediction, indicator) %>% 
  pivot_wider(names_from = treatment, values_from = indicator) %>% 
  kbl(booktabs = TRUE, format = "latex",col.names = NULL, align = c('lcccc')) %>% 
  add_header_above(c(" " = 1, "Click-and-drag" = 1, "Slider" = 1,
                     "Text" = 1, "Distribution" = 1))
sink()


# overshooting of avg temp
pred %>% 
  select(treatment, prediction, starts_with("estimate")) %>% 
  distinct() %>% 
  mutate(mean_historic = 83.6, actual = 85) %>% 
  filter(prediction == "4th July 2022") %>% 
  mutate(overshoot_history = estimate - mean_historic, 
         overshoot_actual = estimate - actual) %>% 
  select(treatment, starts_with("over")) %>% 
  ungroup() %>% 
  summarise(mean_overshoot_h = mean(overshoot_history),
            mean_overshoot_a = mean(overshoot_actual))

# expected global warming
pred %>% 
  select(treatment, estimate) %>% 
  distinct() %>% 
  pivot_wider(names_from = prediction, values_from = estimate) %>% 
  mutate(warming = `4th July 2042` - `4th July 2022`) %>% 
  ungroup() %>% 
  summarise(mean_warm = mean(warming))