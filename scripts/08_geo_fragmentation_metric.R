library(sf)
library(terra)
library(landscapemetrics)
library(ggplot2)

rm(list = ls())



# new test session --------------------------------------------------------

# get a raster as reference for the spatial resolution we're working with
chelsa <- rast("Z:/Arbeit/datashare/data/envidat/biophysical/CHELSA_V2/global/CHELSA_pr_01_1980_V.2.1.tif")
# change values to 1 to decrease size
values(chelsa) <- 1 

pac_islands <- vect("data/spatial_data/pacific_islands.shp") # island shape files

# load in tdwg regions wwe're working with 
tdwg <- st_read("data/tdwg/geojson/level1.geojson")[-c(6,9),] # without antarctic


#df to store results

df_fragmentation <- data.frame(region = rep(tdwg$LEVEL1_NAM, each = 4),
                               metric_name = rep(c("AI","CAI", "CLUMPY", "CORE"), 7),
                               metric_value = NA)
#regions <- tdwg$LEVEL1_NAM

# build a loop:

for (i in 1:nrow(tdwg)) {
  
  reg_name <- tdwg$LEVEL1_NAM[i]
  print(reg_name)
  reg_vec <- tdwg[i,3]
  
  # get extent of vector
  e <- ext(reg_vec)
  
  # increase extent size
  e2 <- e
  e2[1] <- e[1] - 2
  e2[2] <- e[2] + 2
  e2[3] <- e[3] - 2
  e2[4] <- e[4] + 2
  
  # landcsape funcitons require SpatRaster as input
  reg_rast <- rasterize(reg_vec, chelsa, touches = TRUE)
  # cut raster to modified extent of the vector
  reg_rast <- crop(reg_rast, ext(e2))
  
  # change NA values to 2(not all landscape functions work if there is only one class)
  reg_rast[is.na(reg_rast)] <- 2
  
  # get metrics
  reg_ai <- lsm_c_ai(reg_rast)[1,6]
  reg_cai_cv <- lsm_c_cai_cv(reg_rast)[1,6]
  reg_clumpy <- lsm_c_clumpy(reg_rast)[1,6]
  reg_core_cv <- lsm_c_core_cv(reg_rast)[1,6]
  
  # store in results
  df_fragmentation[df_fragmentation$region == reg_name & df_fragmentation$metric_name == "AI" , "metric_value"] <- reg_ai
  df_fragmentation[df_fragmentation$region == reg_name & df_fragmentation$metric_name == "CAI" , "metric_value"] <- reg_cai_cv
  df_fragmentation[df_fragmentation$region == reg_name & df_fragmentation$metric_name == "CLUMPY" , "metric_value"] <- reg_clumpy
  df_fragmentation[df_fragmentation$region == reg_name & df_fragmentation$metric_name == "CORE" , "metric_value"] <- reg_core_cv
}


# prepare metrics or Pacific Islands
# get extent of vector
e <- ext(pac_islands)

# increase extent size
e2 <- e
e2[1] <- e[1] - 2
e2[2] <- e[2] + 2
e2[3] <- e[3] - 2
e2[4] <- e[4] + 2

# landcsape funcitons require SpatRaster as input
pac_rast <- rasterize(pac_islands, chelsa, touches = TRUE)
# cut raster to modified extent of the vector
pac_rast <- crop(pac_rast, ext(e2))

# change NA values to 2 (not all landscape functions work if there is only one class)
pac_rast[is.na(pac_rast)] <- 2

# get metrics
pac_ai <- lsm_c_ai(pac_rast)[1,6]
pac_cai_cv <- lsm_c_cai_cv(pac_rast)[1,6]
pac_clumpy <- lsm_c_clumpy(pac_rast)[1,6]
pac_core_cv <- lsm_c_core_cv(pac_rast)[1,6]

df_pac <- data.frame(region = "PACIFIC",
                     metric_name = c("AI","CAI", "CLUMPY", "CORE"),
                     metric_value = NA)

df_pac[df_pac$metric_name == "AI", "metric_value"] <- pac_ai
df_pac[df_pac$metric_name == "CAI", "metric_value"] <- pac_cai_cv
df_pac[df_pac$metric_name == "CLUMPY", "metric_value"] <- pac_clumpy
df_pac[df_pac$metric_name == "CORE", "metric_value"] <- pac_core_cv

# combine results
df_fragmentation <- rbind(df_fragmentation, df_pac)


save(df_fragmentation, file = "data/trait_analysis/df_fragmentation_metrics.RData")


ggplot(data = subset(df_fragmentation, metric_name == "AI"), aes(x = metric_name, y = metric_value, group = region, color = region)) +
  geom_line() +
  geom_point()

ggplot(data = subset(df_fragmentation, metric_name == "CAI"), aes(x = metric_name, y = metric_value, group = region, color = region)) +
  geom_line() +
  geom_point()

ggplot(data = subset(df_fragmentation, metric_name == "CORE"), aes(x = metric_name, y = metric_value, group = region, color = region)) +
  geom_line() +
  geom_point()

ggplot(data = subset(df_fragmentation, metric_name == "CLUMPY"), aes(x = metric_name, y = metric_value, group = region, color = region)) +
  geom_line() +
  geom_point()
ggplot(data = df_fragmentation, aes(x = metric_name, y = metric_value, group = region, color = region)) +
  geom_line() +
  geom_point()


# -> I'll play around with including CAI or AI (tendency towards CAI)
