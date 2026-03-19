################################################################################
#' @description Qualitative study sampling
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
overall <- readRDS("C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/ACSU5MR/acsu5mr-validation-bd/gen/overall-date-aug.rds")
################################################################################

# Limit to mothers with residency episodes starting more than 5 years prior
# Want to limit to those with uninterrupted residency in past 5 years, but don't have that data yet.
sort(unique(overall$int_date))
as.Date("2024-12-07") - 5*365.25
length(unique(overall$rid_m)) # 848
v_mothers <- unique(subset(overall, mot_in_date <= as.Date("2020-01-01"))$rid_m)
length(v_mothers) # 723

# Identify mothers who have a match in the FPH for all events reported in the HDSS
unique(overall$type)
# Drop HDSS_AB, HDSS_MSC -- no matching attempted
# Drop VS_AB, VS_MSC -- no matching attempted
# Drop VS_NoMatch -- these would be additions from survey. for qualitative sampling, want to focus on omissions
# Keep only VS_Match, HDSS_NoMatch
overall %>%
  filter(rid_m %in% v_mothers) %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>%
  group_by(rid_m, type) %>%
  summarise(n = n()) %>%
  mutate(has_omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  group_by(rid_m) %>%
  summarise(has_omission = max(has_omission)) %>%
  group_by(has_omission) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(per = n/sum(n))

# mothers with no omissions
v_mth_comp <- overall %>%
  filter(rid_m %in% v_mothers) %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>%
  group_by(rid_m, type) %>%
  summarise(n = n()) %>%
  mutate(has_omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  group_by(rid_m) %>%
  summarise(has_omission = max(has_omission)) %>%
  filter(has_omission == 0) %>%
  select(rid_m) %>% pull()

# mothers with any omissions
v_mth_om <- overall %>%
  filter(rid_m %in% v_mothers) %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>%
  group_by(rid_m, type) %>%
  summarise(n = n()) %>%
  mutate(has_omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  group_by(rid_m) %>%
  summarise(has_omission = max(has_omission)) %>%
  filter(has_omission == 1) %>%
  select(rid_m) %>% pull()
length(v_mth_om) # 252

# mothers with no omissions
v_mth_comp <- overall %>%
  filter(rid_m %in% v_mothers) %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>%
  group_by(rid_m, type) %>%
  summarise(n = n()) %>%
  mutate(has_omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  group_by(rid_m) %>%
  summarise(has_omission = max(has_omission)) %>%
  filter(has_omission == 0) %>%
  select(rid_m) %>% pull()
length(v_mth_comp) # 471


# Mothers with no omissions -----------------------------------------------

# age range
tab1full <- overall %>%
  filter(rid_m %in% v_mth_comp) %>%
  pivot_longer(cols = c(b114, matage_cat, paritycat_dss)) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  mutate(value = as.character(value)) %>% 
  mutate(value = replace_na(value, "Missing")) %>%
  group_by(name) %>%
  mutate(total = sum(n), 
         per = sprintf("%.2f", round(n/total*100, 2)),
         nper = paste0(n, " (", per, ")")) %>% 
  mutate(
    name = case_when(
      name == "b114" ~ "Edu. level",
      name == "matage_cat" ~ "Age",
      name == "paritycat_dss" ~ "Parity",
      TRUE ~ name),
    value = case_when(
      value == "Primary" ~ "aPrimary",
      value == "Secondary" ~ "bSecondary",
      value == "Higher secondary" ~ "cHigher secondary",
      value == "Missing" ~ "zMissing",
      TRUE ~ value)
  ) %>%
  arrange(name, value) %>%
  mutate(
    value = case_when(
      value == "aPrimary" ~ "Primary",
      value == "bSecondary" ~ "Secondary",
      value == "cHigher secondary" ~ "Higher secondary",
      value == "zMissing" ~ "Missing",
      TRUE ~ value)
  )
tab1top <- tab1full %>%
  select(name, value, n, per)
tab1tot <- tab1full %>%
  ungroup() %>%
  select(total) %>%
  rename(n = total) %>%
  mutate(name = "Total",
         value = "",
         per = "100.0") %>%
  distinct()
tab1top %>% bind_rows(tab1tot)
  


# Mothers with some omissions ---------------------------------------------




# Misclassification -------------------------------------------------------


# within those...
# ages, edu, parity
