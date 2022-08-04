### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022


understanding <- df %>%
  select(ID, treatment, easy, frustrating, understood) %>%
  distinct() %>%
  pivot_longer(-ID & -treatment, names_to = "var", values_to = "value") %>%
  group_by(treatment, var) %>%
  summarise(
    mean = mean(value, na.rm = T),
    ci = sd(value, na.rm = T) / sqrt(n()) * qt(.95 / 2 + .5, n())
  )

understanding %>%
  mutate(
    var = as.factor(var),
    var = fct_recode(var,
      "...hard to use?" = "easy",
      "...frustrating?" = "frustrating",
      "...difficult to understand?" = "understood"
    )
  ) %>%
  ggplot(aes(x = mean, y = fct_rev(treatment), color = var, shape = var)) +
  # basic plot
  geom_segment(aes(x = mean - ci, xend = mean + ci, yend = treatment), size = 2.4, alpha = 0.6, lineend = "round") +
  geom_point(size = 4) +
  facet_wrap(~var, nrow = 3) +
  # scales to change color and shape of things
  scale_color_manual(name = "", values = c("#00A08A", "#F98400", "chartreuse3")) +
  scale_shape_manual(name = "", values = c(18, 16, 17)) +
  scale_x_continuous(limits = c(3, 5), breaks = seq(3, 5)) +
  labs(
    y = "", x = "",
    title = "From 1 to 7, did you find the interface..."
  ) +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    strip.text = element_text(size = 18, face = "bold", hjust = 1),
    # strip.background = element_rect(fill = "grey99", color = "white", size = 0.1 ),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_markdown(),
    plot.title.position = "plot",
    legend.position = "none"
  )

ggsave("Figures/Presentation_self_reported.png", width = 16 / 1.5, height = 12 / 1.5, units = "in", dpi = 300)

## table

sink("Tables/understanding.tex")
understanding %>%
  mutate(
    m = round(mean, 2),
    cci = round(ci, 2)
  ) %>%
  mutate(indicator = paste0(m, " [", m - cci, ",", m + cci, "]")) %>%
  mutate(var = case_when(
    var == "easy" ~ "Hard to use",
    var == "frustrating" ~ "Frustrating",
    var == "understood" ~ "Difficult to understand"
  )) %>%
  select(treatment, var, indicator) %>%
  pivot_wider(names_from = treatment, values_from = indicator) %>%
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c("lccccc")) %>%
  add_header_above(c(
    " " = 1, "Click-and-drag" = 1, "Slider" = 1,
    "Text" = 1, "Distribution" = 1
  ))
sink()
