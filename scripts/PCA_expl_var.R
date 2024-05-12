

library(dplyr)
library(purrr)
library(stringr)

rm(list = ls())

path_data <- "Z:/roennfeldt/projects/PhD_C1/data/PCA/perc_expl/"

files <- list.files(path_data)


perc_expl_df <- data.frame(species = as.character(NULL),
                region = as.character(NULL),
                perc_1 = as.numeric(),
                perc_2 = as.numeric())


for (i in 1:length(files)) {
  
  print(i)
  
  file <- files[i]
  
  load(paste0(path_data,file))
  
  spp <- file %>% 
    str_remove(".RData") %>% 
    str_split(pattern = "_") %>% 
    map(~ .x[[5]]) %>% 
    simplify()
  
    reg <- file %>% 
    str_remove(".RData") %>% 
    str_split(pattern = "_") %>% 
    map(~ .x[[4]]) %>% 
    simplify()
  
  perc_expl_df[i,"species"] <- spp
  perc_expl_df[i,"region"] <- reg
  perc_expl_df[i,"perc_1"] <- perc_expl[1]
  perc_expl_df[i,"perc_2"] <- perc_expl[2]
  
} # end of for loop over files


perc_expl_df <- perc_expl_df %>%
  mutate(perc_total = perc_1 + perc_2)

save(perc_expl_df, file = "results/ecospat/PCA_perc_expl.RData")

# load("results/ecospat/PCA_perc_expl.RData")



mean_perc_1 <- mean(perc_expl_df$perc_1) 
sd_perc_1 <- sd(perc_expl_df$perc_1)
mean_perc_2 <- mean(perc_expl_df$perc_2) 
sd_perc_2 <- sd(perc_expl_df$perc_2)

(mean_total <- mean(perc_expl_df$perc_total))
(sd_total <- sd(perc_expl_df$perc_total))
