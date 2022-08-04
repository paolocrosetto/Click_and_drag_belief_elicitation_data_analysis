### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022


## overall
plotme <- df %>%
  group_by(treatment, time, second) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  arrange(second)

plotme %>%
  filter(!(time == "15 seconds" & second > 15)) %>%
  filter(!(time == "45 seconds" & second > 45)) %>%
  ggplot(aes(second, mean, color = treatment, fill = treatment)) +
  # geom_errorbar(width = 0.25, aes(ymin=mean-ci, ymax=mean+ci), size = 0.3, alpha = 0.8)+
  geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), color = NA, alpha = 0.3, show.legend = F) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 0.8), labels = percent) +
  # scale_x_continuous(limits = c(0,45))+
  facet_grid(. ~ fct_rev(time), scales = "free_x") +
  labs(x = "Seconds", y = "performance") +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0),
    # strip.background = element_rect(fill = "grey99", color = "white", size = 0.1 ),
    # axis.text.y = element_text(size = 12, face = "bold"),
    panel.border = element_rect(color = "black", size = 0.3, fill = NA),
    plot.title = element_markdown(),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank()
  )

ggsave(paste0(target_dir_plots, "history_overall.png"),
  width = 16 / 1.7, height = 9 / 1.7,
  dpi = 300, units = "in"
)

## by shape

plotme <- df %>%
  group_by(treatment, time, shape, second) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  arrange(second)

plotme %>%
  filter(!(time == "15 seconds" & second > 15)) %>%
  filter(!(time == "45 seconds" & second > 45)) %>%
  ggplot(aes(second, mean, color = treatment, fill = treatment)) +
  # geom_errorbar(width = 0.25, aes(ymin=mean-ci, ymax=mean+ci), size = 0.3, alpha = 0.8)+
  geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), color = NA, alpha = 0.3, show.legend = F) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 0.8), labels = percent) +
  # scale_x_continuous(limits = c(0,45))+
  facet_grid(shape ~ fct_rev(time), scales = "free") +
  labs(x = "Seconds", y = "performance") +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0),
    # strip.background = element_rect(fill = "grey99", color = "white", size = 0.1 ),
    # axis.text.y = element_text(size = 12, face = "bold"),
    panel.border = element_rect(color = "black", size = 0.3, fill = NA),
    plot.title = element_markdown(),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank()
  )

ggsave(paste0(target_dir_plots, "history_shape.png"),
  width = 9 / 1.2, height = 11 / 1.2,
  dpi = 300, units = "in"
)

## by N bins

plotme <- df %>%
  group_by(treatment, time, nbins, second) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n()),
    confintmult = qt(.95 / 2 + .5, n()),
    ci = se * confintmult
  ) %>%
  arrange(second)

plotme %>%
  filter(!(time == "15 seconds" & second > 15)) %>%
  filter(!(time == "45 seconds" & second > 45)) %>%
  ggplot(aes(second, mean, color = treatment, fill = treatment)) +
  # geom_errorbar(width = 0.25, aes(ymin=mean-ci, ymax=mean+ci), size = 0.3, alpha = 0.8)+
  geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), color = NA, alpha = 0.3, show.legend = F) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  # scale_x_continuous(limits = c(0,45))+
  facet_grid(nbins ~ fct_rev(time), scales = "free") +
  labs(x = "Seconds", y = "performance") +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    strip.text = element_text(size = 18, face = "bold", hjust = 0),
    # strip.background = element_rect(fill = "grey99", color = "white", size = 0.1 ),
    # axis.text.y = element_text(size = 12, face = "bold"),
    panel.border = element_rect(color = "black", size = 0.3, fill = NA),
    plot.title = element_markdown(),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank()
  )

ggsave(paste0(target_dir_plots, "history_nbins.png"),
  width = 9 / 1.2, height = 11 / 1.2,
  dpi = 300, units = "in"
)
