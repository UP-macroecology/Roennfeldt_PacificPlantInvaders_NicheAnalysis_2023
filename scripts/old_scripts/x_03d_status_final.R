library(dplyr)


rm(list = ls())

load("data/status_info/occ_status_POWO_GIFT_GloNAF.RData")


# rearrange the table
occ_status_all <- occ_status_POWO_GIFT_GloNAF %>%
  select(!starts_with("GIFT") & !starts_with("powo") & !starts_with("lcvp")) %>% # drop columns holding additional information
  relocate("status_POWO", .before = "status_GIFT")  # all status columns should be at one position in the table 

unique(occ_status_POWO_GIFT_GloNAF$status_POWO)
unique(occ_status_POWO_GIFT_GloNAF$status_GIFT)
unique(occ_status_POWO_GIFT_GloNAF$status_GloNAF)

#' --------------------------

t1 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "native" & is.na(status_GloNAF)) # 15052566
t2 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "native" & status_GloNAF == "naturalized") # 2236902
t3 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "native" & status_GloNAF == "alien") # 3717

t4 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "non-native" & is.na(status_GloNAF)) # 198489
t5 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "non-native" & status_GloNAF == "naturalized") # 122063
t6 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "non-native" & status_GloNAF == "alien") # 458

t7 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "naturalized" & is.na(status_GloNAF)) # 0
t8 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "naturalized" & status_GloNAF == "naturalized") # 217367
t9 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "naturalized" & status_GloNAF == "alien") # 40

t10 <- subset(occ_status_all, status_POWO == "native" & is.na(status_GIFT) & is.na(status_GloNAF)) # 173594
t11 <- subset(occ_status_all, status_POWO == "native" & is.na(status_GIFT) & status_GloNAF == "naturalized") # 25275
t12 <- subset(occ_status_all, status_POWO == "native" & is.na(status_GIFT) & status_GloNAF == "alien") # 26

#' --------------------------

t13 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "native" & is.na(status_GloNAF)) # 615740
t14 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "native" & status_GloNAF == "naturalized") # 712053
t15 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "native" & status_GloNAF == "alien") # 1671

t16 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "non-native" & is.na(status_GloNAF)) # 552001
t17 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "non-native" & status_GloNAF == "naturalized") # 1865378
t18 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "non-native" & status_GloNAF == "alien") # 2984

t19 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "naturalized" & is.na(status_GloNAF)) # 471034
t20 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "naturalized" & status_GloNAF == "naturalized") # 2994661
t21 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "naturalized" & status_GloNAF == "alien") # 398

t22 <- subset(occ_status_all, status_POWO == "introduced" & is.na(status_GIFT) & is.na(status_GloNAF)) # 301544
t23 <- subset(occ_status_all, status_POWO == "introduced" & is.na(status_GIFT) & status_GloNAF == "naturalized") # 337429
t24 <- subset(occ_status_all, status_POWO == "introduced" & is.na(status_GIFT) & status_GloNAF == "alien") # 1299

#' --------------------------

t25 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "native" & is.na(status_GloNAF)) # 468621
t26 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "native" & status_GloNAF == "naturalized") # 60130
t27 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "native" & status_GloNAF == "alien") # 167

t28 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "non-native" & is.na(status_GloNAF)) # 177526
t29 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "non-native" & status_GloNAF == "naturalized") # 200645
t30 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "non-native" & status_GloNAF == "alien") # 1260

t31 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "naturalized" & is.na(status_GloNAF)) # 140444
t32 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "naturalized" & status_GloNAF == "naturalized") # 568833 
t33 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "naturalized" & status_GloNAF == "alien") # 13

t34 <- subset(occ_status_all, status_POWO == "unknown" & is.na(status_GIFT) & is.na(status_GloNAF)) # 250955
t35 <- subset(occ_status_all, status_POWO == "unknown" & is.na(status_GIFT) & status_GloNAF == "naturalized") # 79215
t36 <- subset(occ_status_all, status_POWO == "unknown" & is.na(status_GIFT) & status_GloNAF == "alien") # 535

#' --------------------------

t37 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "unknown" & is.na(status_GloNAF)) # 6477
t38 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "unknown" & status_GloNAF == "naturalized") # 917
t39 <- subset(occ_status_all, status_POWO == "native" & status_GIFT == "unknown" & status_GloNAF == "alien") # 0

t40 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "unknown" & is.na(status_GloNAF)) # 158
t41 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "unknown" & status_GloNAF == "naturalized") # 202
t42 <- subset(occ_status_all, status_POWO == "introduced" & status_GIFT == "unknown" & status_GloNAF == "alien") # 0


t43 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "unknown" & is.na(status_GloNAF)) # 71
t44 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "unknown" & status_GloNAF == "naturalized") # 8
t45 <- subset(occ_status_all, status_POWO == "unknown" & status_GIFT == "unknown" & status_GloNAF == "alien") # 0

t <- t45
n_distinct(t$species)
n_distinct(t$tdwg_l3_name)


# create conflict column --------------------------------------------------

occ_status_all_confl <- occ_status_all %>%
  mutate(conflict_source = case_when(
    
    # Scenario ID 2 (= t2)
    status_POWO == "native" & status_GIFT == "native" & status_GloNAF == "naturalized" ~ "GloNAF_vs_rest",

    # Scenario ID 3 (= t3)
    status_POWO == "native" & status_GIFT == "native" & status_GloNAF == "alien" ~ "GloNAF_vs_rest",
    
    # Scenario ID 4 (= t4)
    status_POWO == "native" & status_GIFT == "non-native" & is.na(status_GloNAF) ~ "POWO_vs_GIFT",
    
    # Scenario ID 5 (= t5)
    status_POWO == "native" & status_GIFT == "non-native" & status_GloNAF == "naturalized" ~ "POWO_vs_rest",
    
    # Scenario ID 6 (= t6)
    status_POWO == "native" & status_GIFT == "non-native" & status_GloNAF == "alien" ~ "POWO_vs_rest",
    
    # Scenario ID 7 (= t7)
    status_POWO == "native" & status_GIFT == "naturalized" & is.na(status_GloNAF) ~ "POWO_vs_GIFT",
    
    # Scenario ID 8 (= t8)
    status_POWO == "native" & status_GIFT == "naturalized" & status_GloNAF == "naturalized" ~ "POWO_vs_rest",
    
    # Scenario ID 9 (= t9)
    status_POWO == "native" & status_GIFT == "naturalized" & status_GloNAF == "alien" ~ "POWO_vs_rest",
    
    # Scenario ID 11 (= t11)
    status_POWO == "native" & is.na(status_GIFT) & status_GloNAF == "naturalized" ~ "POWO_vs_GloNAF",
    
    # Scenario ID 12 (= t12)
    status_POWO == "native" & is.na(status_GIFT) & status_GloNAF == "alien" ~ "POWO_vs_GloNAF",
    
    # Scenario ID 13 (= t13)
    status_POWO == "introduced" & status_GIFT == "native" & is.na(status_GloNAF) ~ "POWO_vs_GIFT",
    
    # Scenario ID 14 (= t14)
    status_POWO == "introduced" & status_GIFT == "native" & status_GloNAF == "naturalized" ~ "GIFT_vs_rest",
    
    # Scenario ID 15 (= t15)
    status_POWO == "introduced" & status_GIFT == "native" & status_GloNAF == "alien" ~ "GIFT_vs_rest",
    
    # Scenario ID 26 (= t26)
    status_POWO == "unknown" & status_GIFT == "native" & status_GloNAF == "naturalized" ~ "GIFT_vs_GloNAF",
    
    # Scenario ID 27 (= t27)
    status_POWO == "unknown" & status_GIFT == "native" & status_GloNAF == "alien" ~ "GIFT_vs_GloNAF",
    
    # Scenario ID 38 (= t38)
    status_POWO == "native" & status_GIFT == "unknown" & status_GloNAF == "naturalized" ~ "POWO_vs_GloNAF",
    
    # Scenario ID 39 (= t39)
    status_POWO == "native" & status_GIFT == "unknown" & status_GloNAF == "alien" ~ "POWO_vs_GloNAF"
  ))


t2 <- subset(occ_status_all_confl, status_POWO == "native" & status_GIFT == "native" & status_GloNAF == "naturalized") # 2236902

save(occ_status_all_confl, file = "data/intermediate/occ_status_all_conflicts.RData")
