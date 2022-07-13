### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

## final scores 
final <- df %>% 
  group_by(ID, trial) %>% 
  filter(second == max(second))

## means and conf.int. for final scores
plotme <- final %>%
  group_by(treatment, time, nbins, shape) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            se = sd(score, na.rm = TRUE)/sqrt(n()),
            confintmult = qt(.95/2 + .5, n()),
            ci = se*confintmult)

## plot 1: one point + conf.int for each of the 24 screens, for each of 4 interfaces
## this is overkill, but contains ALL information and allows one to do ALL comparisons one wants
plotme %>% 
  ggplot(aes(x = mean, y = fct_rev(nbins), color = time, shape = time))+
  # basic plot
  geom_linerange(aes(xmin = mean-ci, xmax = mean+ci), size = 1.5, alpha = 0.6, position = position_dodge(width = -0.5))+
  geom_point(size = 2.5, position = position_dodge(width = -0.5))+
  facet_grid(treatment~shape)+
  scale_x_continuous(labels = percent)+
  scale_color_manual(name = "", values = c("#00A08A", "#F98400"))+
  scale_shape_manual(name = "", values = c(18, 16))+
  #fake data to force ggplot to use the correct scales for each subplot
  labs(y = "", x = "",
       title = "Performance after <span style = 'color:#00A08A;'>15</span> and <span style = 'color:#F98400;'>45</span> seconds, by interface",
       subtitle = "By number of bins and distribution shape")+
  theme_ipsum_rc()+
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
        strip.text = element_text(size = 9, face = "bold", hjust = 1),
        #strip.background = element_rect(fill = "grey99", color = "white", size = 0.1 ),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_markdown(),
        plot.title.position = "plot",
        legend.position = "none")

ggsave("Figures/Presentation_final_scores_detailed.png", 
       width = 16/1.7, height = 12/1.7, units = "in", dpi = 300)

# Same, but in a table: one mean + conf.int for each screen (24), each interface (4), for a total of 96 items
sink("Tables/main_results.tex")
plotme %>% 
  mutate(m = round(100*mean,2),
         cci = round(100*ci,2)) %>% 
  mutate(indicator = paste0(m, " [", m-cci, ",", m+cci, "]")) %>% 
  select(treatment, time, nbins, shape, indicator) %>% 
  pivot_wider(names_from = treatment, values_from = indicator) %>% 
  arrange(fct_rev(time)) %>% 
  ungroup() %>% 
  select(-time, -nbins) %>% 
  kbl(booktabs = TRUE, format = "latex", col.names = NULL, align = c('lccccc')) %>% 
  pack_rows("45 seconds", 1, 12) %>% 
  pack_rows("15 seconds", 13, 24) %>% 
  pack_rows("7 bins", 1, 4) %>% 
  pack_rows("15 bins", 5, 8) %>% 
  pack_rows("30 bins", 9, 12) %>% 
  pack_rows("7 bins", 13, 16) %>% 
  pack_rows("15 bins", 17, 20) %>% 
  pack_rows("30 bins", 21, 24) %>% 
  add_header_above(c(" " = 1, "Click-and-drag" = 1, "Slider" = 1,
                     "Text" = 1, "Distribution" = 1))
sink()  