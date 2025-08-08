library(dplyr)
library(stringr)
library(purrr)
library(diffdf)


# compile a list of all regions the target species have been introduced to:

# focal species 
load("data/species_selection/spp_buffer.RData")
load("data/species_selection/spp_buffer_2.RData")


path_data <- "X:/roennfeldt/Projects/PhD_C1/data/"




# native species - done ----------------------------------------------------


nat_files <- list.files("data/buffer_sensitivity/coords_final_nat/")


spp_nat <- nat_files %>% 
  str_remove(".RData") %>% 
  str_split(pattern = "_") %>%
  map(~ .x[[5]]) %>%
  simplify() %>%
  unique()


spp_buf_comp <- spp_nat

save(spp_buf_comp, file = "data/species_selection/spp_buf_comp.RData")


v_count <- NULL

df_tasks <- as.data.frame(matrix(nrow = 0, ncol = 2))
names(df_tasks) <- c("species", "region")

# determine number of files that should in theory exist for this species selection

for (spp in spp_buffer_2) {
  
  files <- list.files(paste0(path_data, "regional_occs/criterion_1/introduced/"), pattern = spp)
  
  regions <- files %>% 
    str_remove(".RData") %>% 
      str_split(pattern = "_") %>%
      map(~ .x[[3]]) %>%
      simplify() %>%
      unique()
  
  df_tasks <- rbind(df_tasks,
                    data.frame(species = rep(spp, length(regions)),
                               region = regions))
  
}


v_count <- NULL

for (spp in spp_nat) {
  
  files <- list.files(paste0(path_data, "regional_occs/criterion_1/introduced/"), pattern = spp)
  
  
  v_count <- c(v_count, as.numeric(length(files)))
  
  
}



sum(v_count)


# compare with progress 



files_done <- list.files("data/buffer_sensitivity/coords_final_intr/")

df_done <- as.data.frame(matrix(nrow = 0, ncol = 2))
names(df_done) <- c("species", "region")


for (file in files_done) {
  
  spp <- file %>% 
    str_remove(".RData") %>% 
    str_split(pattern = "_") %>% 
    map(~ .x[[6]]) %>%
    simplify() %>%
    unique()
  
  
  reg <- file %>% 
    str_remove(".RData") %>% 
    str_split(pattern = "_") %>% 
    map(~ .x[[5]]) %>%
    simplify() %>%
    unique()
  
  df_done <- rbind(df_done,
                    data.frame(species = spp,
                               region = reg))
  
} # end of for loop over files


t <- df_done %>% 
  filter(species %in% c(spp_buffer,spp_buffer_2))

difference_dfs <- diffdf(df_tasks, df_done)

missing_rows <- difference_dfs[["ExtRowsBase"]] %>% 
  pull(..ROWNUMBER..)


df_pending <- df_tasks[missing_rows,]
save(df_pending, file = "data/buffer_sensitivity/df_pending.RData")


df_donedone <- df_tasks[1:182,]


t <- df_tasks %>% 
  inner_join()
