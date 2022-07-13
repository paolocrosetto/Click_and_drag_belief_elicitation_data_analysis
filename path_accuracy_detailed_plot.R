### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## path plots
plotme <- df %>%
  group_by(treatment, time, nbins, shape, trial, second) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n()),
            confintmult = qt(.95/2 + .5, n()),
            ci = se*confintmult) %>% 
  arrange(trial, second)

plotme %>% 
  filter(time == "15 seconds") %>% 
  ggplot(aes(second, mean, color = treatment))+
  geom_errorbar(width = 0.25, aes(ymin=mean-ci, ymax=mean+ci), size = 0.3, alpha = 0.8)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0,15))+
  scale_color_brewer(palette = "Set1", name = "")+
  facet_grid(nbins~shape)+
  ggtitle("Performance history of the interface -- 15 seconds screens")+
  theme_ipsum()+
  theme(legend.position = "bottom")

ggsave("Figures/Presentation_history15.png",
       width = 16/1.5, height = 12/1.5, units = "in", dpi = 300)

plotme %>% 
  filter(time == "45 seconds") %>% 
  ggplot(aes(second, mean, color = treatment))+
  geom_errorbar(width = 0.25, aes(ymin=mean-ci, ymax=mean+ci), size = 0.3, alpha = 0.8)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0,45))+
  scale_color_brewer(palette = "Set1", name = "")+
  facet_grid(nbins~shape)+
  ggtitle("Performance history of the interface -- 45 seconds screens")+
  theme_ipsum()+
  theme(legend.position = "bottom")

ggsave("Figures/Presentation_history45.png",
       width = 16/1.5, height = 12/1.5, units = "in", dpi = 300) 
