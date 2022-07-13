### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## final scores 
final <- df %>% 
  group_by(ID, trial) %>% 
  filter(second == max(second))


### here we pack screens according to dimensions: by time (only), by nbins (only), and by shape (only)
overall <- final %>%
  group_by(treatment) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE)/sqrt(n()),
            confintmult = qt(.95/2 + .5, n()),
            ci = se*confintmult) %>% 
  mutate(m = round(100*mean,2),
         cci = round(100*ci,2)) %>% 
  mutate(indicator = paste0(m, " [", m-cci, ",", m+cci, "]")) %>% 
  select(treatment, indicator) %>% 
  mutate(type = "") %>% 
  pivot_wider(names_from = treatment, values_from = indicator)

bytime <- final %>%
  group_by(treatment, time) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE)/sqrt(n()),
            confintmult = qt(.95/2 + .5, n()),
            ci = se*confintmult) %>% 
  mutate(m = round(100*mean,2),
         cci = round(100*ci,2)) %>% 
  mutate(indicator = paste0(m, " [", m-cci, ",", m+cci, "]")) %>% 
  select(treatment, type = time, indicator) %>% 
  pivot_wider(names_from = treatment, values_from = indicator) %>% 
  arrange(fct_rev(type))

byshape <- final %>%
  group_by(treatment, shape) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE)/sqrt(n()),
            confintmult = qt(.95/2 + .5, n()),
            ci = se*confintmult) %>% 
  mutate(m = round(100*mean,2),
         cci = round(100*ci,2)) %>% 
  mutate(indicator = paste0(m, " [", m-cci, ",", m+cci, "]")) %>% 
  select(treatment, type = shape, indicator) %>% 
  pivot_wider(names_from = treatment, values_from = indicator)

bybins <- final %>%
  group_by(treatment, nbins) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE)/sqrt(n()),
            confintmult = qt(.95/2 + .5, n()),
            ci = se*confintmult) %>% 
  mutate(m = round(100*mean,2),
         cci = round(100*ci,2)) %>% 
  mutate(indicator = paste0(m, " [", m-cci, ",", m+cci, "]")) %>% 
  select(treatment, type = nbins, indicator) %>% 
  pivot_wider(names_from = treatment, values_from = indicator)

overalls <- 
  bind_rows(overall, bytime, bybins, byshape)

#export N
final %>% 
  ungroup() %>% 
  select(treatment, ID) %>% 
  distinct() %$%
  table(treatment)

# export table
sink("Tables/overall_results.tex")
overalls %>% 
  kbl(booktabs = TRUE, format = "latex", col.names = NULL, align = c('lccccc')) %>% 
  pack_rows("Overall", 1, 1) %>% 
  pack_rows("by time constraint", 2, 3) %>% 
  pack_rows("by number of bins", 4, 6) %>% 
  pack_rows("by shape", 7, 10) %>% 
  add_header_above(c(" " = 1, "Click-and-drag\n(N = 95)" = 1, "Slider\n(N = 91)" = 1,
                     "Text\n(N = 91)" = 1, "Distribution\n(N = 95)" = 1))
sink()


## same, but with plots
plot_overalls <- overalls %>% 
  # extract raw from formatted data
  mutate(type = if_else(type == "", "Overall", type)) %>% 
  pivot_longer(-type, names_to = "treatment", values_to = "indicator") %>% 
  separate(indicator, into = c("m", "rest"), sep = "\\[", convert = T) %>% 
  separate(rest,      into = c("cil", "rest"), sep = ",", convert = T) %>% 
  separate(rest,      into = c("cih"), sep = "\\]", convert = T) %>% 
  mutate(m= m/100, cil = cil/100, cih = cih/100) %>% 
  # add a manipulation variable
  mutate(manipulation = case_when(type == "Overall" ~ "Overall performance", 
                                  type %in% c("45 seconds", "15 seconds") ~ "Time constraint",
                                  type %in% c("7 bins", "15 bins", "30 bins") ~ "Number of bins",
                                  type %in% c("Symmetric", "Skewed", 
                                              "Bimodal", "Random") ~ "Distribution shape")) %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, "Click-and-drag", "Slider", "Text"))


plot_theme <- theme_ipsum_rc()+
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
        strip.text = element_text(size = 18, face = "bold", hjust = 0),
        #strip.background = element_rect(fill = "grey99", color = "white", size = 0.1 ),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", size = 0.3, fill = NA),
        plot.title = element_markdown(),
        plot.title.position = "plot",
        legend.position = "none")
# overall
plot_overalls %>% 
  filter(manipulation == "Overall performance") %>% 
  ggplot(aes(x = m, y = fct_rev(treatment), color = treatment))+
  geom_segment(aes(x = cil, xend = cih, yend = treatment), size = 3, alpha = 0.6, lineend = "round")+
  geom_point(size = 4.5)+
  scale_x_continuous(labels = percent)+
  facet_grid(.~type)+
  labs(x = "Performance", y = "")+
  plot_theme

ggsave(filename = paste0(target_dir_plots, "Presentation_accuracy_overall.png"),
       width = 16/1.7, height = 9/1.7, 
       dpi = 300, units = "in")

# by time constraint
plot_overalls %>% 
  filter(manipulation == "Time constraint") %>% 
  ggplot(aes(x = m, y = fct_rev(treatment), color = treatment))+
  geom_segment(aes(x = cil, xend = cih, yend = treatment), size = 3, alpha = 0.6, lineend = "round")+
  geom_point(size = 4.5)+
  scale_x_continuous(labels = percent)+
  facet_grid(.~fct_rev(type))+
  labs(x = "Performance", y = "")+
  plot_theme
ggsave(filename = paste0(target_dir_plots, "Presentation_accuracy_bytime.png"),
       width = 16/1.7, height = 9/1.7, 
       dpi = 300, units = "in")

# by number of bins
plot_overalls %>% 
  filter(manipulation == "Number of bins") %>% 
  ggplot(aes(x = m, y = fct_rev(treatment), color = treatment))+
  geom_segment(aes(x = cil, xend = cih, yend = treatment), size = 3, alpha = 0.6, lineend = "round")+
  geom_point(size = 4.5)+
  scale_x_continuous(labels = percent)+
  facet_grid(.~fct_relevel(type, "7 bins", "15 bins"))+
  labs(x = "Performance", y = "")+
  plot_theme
ggsave(filename = paste0(target_dir_plots, "Presentation_accuracy_bybins.png"),
       width = 16/1.7, height = 9/1.7, 
       dpi = 300, units = "in")

# by shape
plot_overalls %>% 
  filter(manipulation == "Distribution shape") %>% 
  ggplot(aes(x = m, y = fct_rev(treatment), color = treatment))+
  geom_segment(aes(x = cil, xend = cih, yend = treatment), size = 3, alpha = 0.6, lineend = "round")+
  geom_point(size = 4.5)+
  scale_x_continuous(labels = percent)+
  facet_grid(.~fct_relevel(type, "Symmetric", "Skewed", "Bimodal"))+
  labs(x = "Performance", y = "")+
  plot_theme
ggsave(filename = paste0(target_dir_plots, "Presentation_accuracy_byshape.png"),
       width = 16/1.7, height = 9/1.7, 
       dpi = 300, units = "in")

