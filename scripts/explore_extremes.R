library(dplyr)
library(ggplot2)

rm(list = ls())

load("shiny/NicheDyn_exploration/data/input_TA_unstand.RData")

input_TA$years_since_intro <- as.numeric(input_TA$years_since_intro)

# identify outlier --------------------------------------------------------

out_spp <- input_TA[which(input_TA$expansion %in% boxplot.stats(input_TA$expansion)$out),] %>% 
  select(species, region, expansion, years_since_intro)


# out_spp <- out_spp %>% 
#   filter(!(species == "Celosia argentea" & region == "ate"))
#   

ggplot(out_spp, aes(x = years_since_intro, y = expansion)) +
  geom_point() +
  xlab("Time since introduction (years)") +
  ylab("Expansion (%)") +
  theme_bw()


ggplot(input_TA, aes(x = years_since_intro, y = expansion)) +
  geom_point() +
  xlab("Time since introduction (years)") +
  ylab("Expansion (%)") +
  theme_bw()







t <- input_TA %>%  
  select(species, region, expansion, years_since_intro)


spp_rel <- input_TA %>% 
  arrange(desc(rel_expansion)) %>% 
  slice(1:10) %>% 
  pull(species_region)

spp_exp <- input_TA %>% 
  arrange(desc(expansion)) %>% 
  slice(1:10) %>% 
  pull(species_region)


spp_exp == spp_rel
