### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

comm <- df %>%
  select(treatment, commentary) %>%
  distinct() %>%
  mutate(sentiment = get_sentiment(commentary))

plotme <- comm %>%
  filter(!is.na(commentary)) %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(sentiment, na.rm = TRUE),
    sd = sd(sentiment, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  )

# mean sentiment per treatment
plotme %>%
  ggplot(aes(x = mean, y = fct_rev(treatment), color = treatment)) +
  geom_segment(aes(x = mean + ci, xend = mean - ci, yend = treatment), size = 3, alpha = 0.6, lineend = "round") +
  geom_point(size = 4.5) +
  theme_ipsum_rc() +
  labs(x = "mean sentiment", y = "") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )


ggsave(
  filename = paste0(target_dir_plots, "Presentation_sentiment_analysis.png"),
  width = 16 / 1.7, height = 9 / 1.7,
  dpi = 300, units = "in"
)

# point estimates
comm %>%
  filter(!is.na(commentary)) %>%
  group_by(treatment) %>%
  summarise(m = round(mean(sentiment, na.rm = T), 2), s = round(sd(sentiment, na.rm = T), 2))

# tests
kruskal.test(sentiment ~ treatment, data = comm) %>% tidy() ## significant

# binary tests
# click vs slider: NS
wilcox.test(
  comm$sentiment[comm$treatment == "Click-and-drag"],
  comm$sentiment[comm$treatment == "Slider"]
) %>% tidy()

# click vs text: NS
wilcox.test(
  comm$sentiment[comm$treatment == "Click-and-drag"],
  comm$sentiment[comm$treatment == "Text"]
) %>% tidy()

# click vs distro: NS
wilcox.test(
  comm$sentiment[comm$treatment == "Click-and-drag"],
  comm$sentiment[comm$treatment == "Distribution"]
) %>% tidy()

# slider vs Text: sign
wilcox.test(
  comm$sentiment[comm$treatment == "Slider"],
  comm$sentiment[comm$treatment == "Text"]
) %>% tidy()

# slider vs distribution: sign
wilcox.test(
  comm$sentiment[comm$treatment == "Slider"],
  comm$sentiment[comm$treatment == "Distribution"]
) %>% tidy()

# distribution vs Text: NS
wilcox.test(
  comm$sentiment[comm$treatment == "Text"],
  comm$sentiment[comm$treatment == "Distribution"]
) %>% tidy()
