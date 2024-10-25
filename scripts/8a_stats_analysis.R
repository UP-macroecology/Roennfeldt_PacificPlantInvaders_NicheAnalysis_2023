#' ---------------------------
#
# Purpose of script: statistical analysis of the main results
# Author: Anna Rönnfeldt
# Date Created: 2024-05
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------


library(dplyr)
library(plotrix)


load("results/ecospat/master_results_AC.RData")


# slect relevant columns and calulate standardised stability, unfilling, and expansion 
master_results_AC <- master_results_AC %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>%
  select(c(species, region, overlap, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering)) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(stand_expansion = rel_expansion / total_esu) %>%
  mutate(stand_stability = rel_stability / total_esu) %>%
  mutate(stand_unfilling = rel_unfilling / total_esu) 
  

# working with the rel. dynamicsfor abandonment and pioneering
m_abandonment <- glm(rel_abandonment ~ region, data = master_results_AC, family = "binomial")
m_pioneering <- glm(rel_pioneering ~ region, data = master_results_AC, family = "binomial")

# working with the standardised ESU values (based on rel. dynamics)
m_expansion <- glm(stand_expansion ~ region, data = master_results_AC, family = "binomial")
m_stability <- glm(stand_stability ~ region, data = master_results_AC, family = "binomial")
m_unfilling <- glm(stand_unfilling ~ region, data = master_results_AC, family = "binomial")



anova(m_expansion)
summary(m_expansion)

anova(m_stability)
summary(m_stability)

anova(m_unfilling)
summary(m_unfilling)

anova(m_abandonment)
summary(m_abandonment)

anova(m_pioneering)
summary(m_pioneering)



