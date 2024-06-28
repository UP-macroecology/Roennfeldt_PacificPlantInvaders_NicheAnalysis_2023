
library(dplyr)

rm(list = ls())

load("results/ecospat/master_results_AC.RData")



master_results <- master_results_AC %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>%
  select(c(species, region, overlap, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering)) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(stand_expansion = rel_expansion / total_esu) %>%
  mutate(stand_stability = rel_stability / total_esu) %>%
  mutate(stand_unfilling = rel_unfilling / total_esu) 
  

# working with the rel. dynamics  
m_abandonment <- glm(rel_abandonment ~ region, data = master_results, family = "binomial")
m_pioneering <- glm(rel_pioneering ~ region, data = master_results, family = "binomial")

# working with the standardised ESU values (based on rel. dynamics)
m_expansion <- glm(stand_expansion ~ region, data = master_results, family = "binomial")
m_stability <- glm(stand_stability ~ region, data = master_results, family = "binomial")
m_unfilling <- glm(stand_unfilling ~ region, data = master_results, family = "binomial")



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



# original ecospat output -------------------------------------------------

m_expansion_orig <- glm(stand_expansion ~ region, data = master_results, family = "binomial")
m_expansion_orig$deviance/m_expansion_orig$df.residual

m_stability_orig <- glm(stand_stability ~ region, data = master_results, family = "binomial")
m_stability_orig$deviance/m_stability_orig$df.residual 

m_unfilling_orig <- glm(stand_unfilling ~ region, data = master_results, family = "binomial")
m_unfilling_orig$deviance/m_unfilling_orig$df.residual


summary(m_expansion_orig)
summary(m_stability_orig)
summary(m_unfilling_orig)



regions <- as.character(unique(master_results$region))

reg <- "sam"

df <- subset(master_results, region == reg)

mean(df$stand_unfilling, na.rm = TRUE)
mean(df$stand_stability, na.rm = TRUE)
mean(df$stand_expansion, na.rm = TRUE)
