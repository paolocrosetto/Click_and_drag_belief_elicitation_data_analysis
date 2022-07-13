### data analysis for the belief interface horserace paper
###
### author: Paolo Crosetto
###
### this version: June/July 2022

devices <- df %>% select(ID, treatment, keyboard, mouse, touchpad, touchscreen, device, os) %>% 
  distinct()

table(devices$os)
table(devices$device)

# table of devices
sink("Tables/devices.tex")
devices %>% 
  select(treatment, keyboard, mouse, touchpad, touchscreen) %>% 
  pivot_longer(-treatment, names_to = "var", values_to = "val") %>% 
  group_by(treatment, var) %>% 
  summarise(v = paste0(100*round(mean(val),4),"%")) %>% 
  pivot_wider(names_from = treatment, values_from = v) %>% 
  kbl(format = "latex", booktabs = T,col.names = NULL, align = c('lccccc')) %>%   
  add_header_above(c(" " = 1, "Click-and-drag" = 1, "Slider" = 1,
                     "Text" = 1, "Distribution" = 1))
sink()
