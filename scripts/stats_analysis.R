
library(dplyr)

rm(list = ls())

load("results/master_results_new.RData")

master_results <- master_results %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam")))

m_expansion <- glm(rel.expansion ~ region, data = master_results, family = "binomial")
m_expansion$deviance/m_expansion$df.residual # making sure data is not over dispersed (value has to be <1 to be ok)

m_stability <- glm(rel.stability ~ region, data = master_results, family = "binomial")
m_stability$deviance/m_stability$df.residual 

m_unfilling <- glm(rel.unfilling ~ region, data = master_results, family = "binomial")
m_unfilling$deviance/m_unfilling$df.residual

m_abandonment <- glm(rel.abandonment ~ region, data = master_results, family = "binomial")
m_abandonment$deviance/m_abandonment$df.residual

m_pioneering <- glm(rel.pioneering ~ region, data = master_results, family = "binomial")
m_pioneering$deviance/m_pioneering$df.residual



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

m_expansion_orig <- glm(expansion ~ region, data = master_results, family = "binomial")
m_expansion_orig$deviance/m_expansion_orig$df.residual

m_stability_orig <- glm(stability ~ region, data = master_results, family = "binomial")
m_stability_orig$deviance/m_stability_orig$df.residual 

m_unfilling_orig <- glm(unfilling ~ region, data = master_results, family = "binomial")
m_unfilling_orig$deviance/m_unfilling_orig$df.residual


summary(m_expansion_orig)
summary(m_stability_orig)
summary(m_unfilling_orig)
