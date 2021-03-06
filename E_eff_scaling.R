################################
# Energetic efficiency scaling #
################################

library(tidyverse)
library(ggsci)

d_full <- read_csv("Cetacea model output BOUT_EXTANT_final_3.18.19.csv") %>% 
  select(-(X13:X25)) %>% 
  mutate(`MR exponent` = factor(`MR exponent`),
         Group = case_when(Family == "Balaenopteridae" ~ "Rorqual",
                           Family == "Balaenidae" ~ "Balaenid",
                           TRUE ~ "Odontocete"),
         Grouping = case_when(Family == "Balaenopteridae" ~ "Rorqual",
                              Family == "Balaenidae" ~ "Balaenid",
                              Family %in% c("Delphinidae", "Phocoenidae") ~ "Delphinidae and Phocoenidae",
                              Family %in% c("Physeteridae", "Ziphiidae") ~ "Physeteridae and Ziphiidae"))

energy_eff_lm <- d_full %>% 
  group_by(Group, `MR exponent`) %>% 
  group_map(function(group, ...) {
    mod <- lm(log10(E_divesurf_med) ~ log10(`M (kg)`), group, weights = Percent)
    mod_summ <- summary(mod)
    fstat <- mod_summ$fstatistic
    p.value <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) 
    r.squared <- mod_summ$r.squared
    tibble(intercept = coef(mod)[1],
           slope = coef(mod)[2],
           p.value,
           r.squared)
  }) %>% 
  ungroup %>% 
  mutate(a = 10 ^ intercept,
         b = slope)

write_csv(energy_eff_lm, path = "Figure 4 regession values.csv")

# Plot to verify allometric equations fit the original points
# All combinations of group and MR exponent
crossing(Group = unique(d_full$Group),
                         `MR exponent` = unique(d_full$`MR exponent`)) %>% 
  # Join with min and max mass per group
  left_join(d_full %>% 
              group_by(Group) %>% 
              summarize(m_min = min(`M (kg)`),
                        m_max = max(`M (kg)`)),
            by = "Group") %>% 
  # gather min and max mass into a mass variable
  gather(min_max, mass, m_min, m_max) %>% 
  # Join allometric parameters
  left_join(energy_eff_lm,
            by = c("Group", "MR exponent")) %>% 
  # Calculate energy efficiency estimates
  mutate(energy_eff = a * mass ^ b) %>% 
  # plot
  ggplot(aes(color = Group)) +
  # points of original data
  geom_point(aes(`M (kg)`, E_divesurf_med, color = Group),
             d_full) +
  # allometric lines
  geom_line(aes(mass, energy_eff)) +
  # log scales
  scale_x_log10() +
  scale_y_log10() +
  scale_color_aaas() +
  theme_bw()
