
library(dplyr)
library(plotrix)

rm(list = ls())

load("results/ecospat/master_results_AC.RData")



master_results_AC <- master_results_AC %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>%
  select(c(species, region, overlap, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering)) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(stand_expansion = rel_expansion / total_esu) %>%
  mutate(stand_stability = rel_stability / total_esu) %>%
  mutate(stand_unfilling = rel_unfilling / total_esu) 
  

# working with the rel. dynamics  
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



# SE niche dynamics -------------------------------------------------------

# goal calculate the species level SE for each metric 
specs <- unique(master_results_AC$species)
metrics <- c("rel_abandonment", "stand_unfilling", "stand_stability", "stand_expansion", "rel_pioneering")
# prep empty df to store results
df_SE <- data.frame(species = rep(specs, each = 5),
                    metric = rep(metrics, length(specs)),
                    SE = as.numeric(NA))


for (spp in specs) {
  
  print (spp)
  
  spp_df <- subset(master_results_AC, species == spp)
  
  for (metric in metrics) {
    print(metric)
    
    # add SE info to results df
    df_SE[df_SE$species == spp & df_SE$metric == metric,"SE"] <- std.error(spp_df[,metric])*100
  } # for loop over metrics
} # end of for loop

rm(spp_df)

# calculate mean SE per metric
df_mean_SE <- data.frame(metric = metrics,
                         mean_SE = NA)

for (m in metrics) {
  
  print(m)
  
  metric_df <- df_SE %>% dplyr::filter(metric == m)
  
  hist(metric_df$SE, main = paste("histogram of", m))
  df_mean_SE[df_mean_SE$metric == m, "mean_SE"] <- mean(metric_df$SE)
} # for loop over metrics


