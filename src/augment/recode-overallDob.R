################################################################################
#' @description Recode and categorize variables
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
library(lubridate)
#' Inputs
dat <- readRDS("./gen/augment/overallDob-aug.rds")
################################################################################

# Recode ------------------------------------------------------------------

# pregnancy outcome survey
unique(dat$c223)
dat <- dat %>%
  mutate(c223 = as.character(c223)) %>%
  mutate(c223 = case_when(
    c223 == "Born alive" ~ "Live birth",
    c223 == "Born dead" ~ "Stillbirth",
    c223 == "Miscarriage" ~ "Miscarriage",
    c223 == "Abortion" ~ "Abortion",
    TRUE ~ c223
  ))

# pregnancy outcome dss
unique(dat$pregout_dss)
dat <- dat %>%
  mutate(pregout_dss = as.character(pregout_dss)) %>%
  mutate(pregout_dss = case_when(
    pregout_dss == "Livebirth" ~ "Live birth",
    TRUE ~ pregout_dss
  ))

# Make sure mother's dob and date of first in-migration applies to all her records
# first check that there is only ever one unique non-missing dob and doi per mother
# no mother has missing dob
nrow(subset(dat, is.na(dob_m_dss))) # 0
# no mother has multiple dob
dat %>%
  select(rid_m, dob_m_dss) %>%
  distinct() %>%
  group_by(rid_m) %>%
  mutate(ndob_m = n()) %>%
  filter(ndob_m > 1) %>% nrow() # 0
# 679 mothers have missing doi
nrow(subset(dat, is.na(doi_m_dss))) # 0
# there are never 2 different doi's per mother though
dat %>%
  group_by(rid_m) %>%
  mutate(doi_m_dss_fill = min(doi_m_dss, na.rm = TRUE)) %>%
  filter(!is.na(doi_m_dss) & doi_m_dss != doi_m_dss_fill) %>% nrow() # 0
# ok, proceed
dat <- dat %>%
  group_by(rid_m) %>%
  mutate(doi_m_dss = min(doi_m_dss, na.rm = TRUE)) %>%
  ungroup() 


# Mother-level characteristics --------------------------------------------

# Maternal age at interview 
# note that if an hdss pregnancy outcome/child was not matched to the VS, there will not be an int_date
# need to fill in int_date for all child records for each mother
# first check that there is only ever one unique non-missing int_date per mother
dat %>%
  group_by(rid_m) %>%
  mutate(int_date_fill = max(int_date_sur, na.rm = TRUE)) %>%
  filter(!is.na(int_date_sur) & int_date_sur != int_date_fill) %>% nrow() # 0
# also check that after filling, there are no missing int_dates
dat %>%
  group_by(rid_m) %>%
  mutate(int_date_fill = max(int_date_sur, na.rm = TRUE)) %>%
  filter(is.na(int_date_fill)) %>% nrow() # 0
# ok, proceed
dat <- dat %>%
  group_by(rid_m) %>%
  mutate(int_date_sur = max(int_date_sur, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mage_int = as.numeric(int_date_sur - dob_m_dss)/365.25,
         magecat_int = cut(mage_int,
                           breaks = seq(15,55, 5), 
                           labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")),
         magecat2_int = cut(mage_int,
                             breaks = c(15, seq(25, 45, 5), 55), 
                             labels = c("15-24", "25-29", "30-34", "35-39", "40-44", "45+"))) 
nrow(subset(dat, is.na(mage_int))) # 0
nrow(subset(dat, is.na(magecat_int))) # 0
nrow(subset(dat, is.na(magecat2_int))) # 0
## check breaks
# dat %>%
#   select(mage_int, magecat_int, magecat2_int) %>%
#   distinct() %>% View


# Max parity dss
df_parity <- dat %>%
  group_by(rid_m) %>%
  summarise(paritymax_dss = max(parity_dss, na.rm = TRUE)) %>%
  mutate(paritymaxcat_dss = cut(paritymax_dss, breaks = c(0,1,2,3,10), labels = c("1","2","3", "4+")))
nrow(subset(df_parity, is.na(paritymaxcat_dss))) # 0
nrow(subset(df_parity, paritymax_dss == 0)) # 0
dat <- dat %>%
  left_join(df_parity, by = "rid_m") %>%
  relocate(paritymax_dss, .after = parity_dss) %>%
  relocate(paritymaxcat_dss, .after = paritymax_dss)

# # check breaks
# dat %>%
#   select(paritymax_dss, paritymaxcat_dss) %>%
#   distinct() %>% View

# Max parity sur
df_parity <- dat %>%
  group_by(rid_m) %>%
  summarise(paritymax_sur = max(as.numeric(parity_sur), na.rm = TRUE)) %>%
  mutate(paritymaxcat_sur = cut(paritymax_sur, breaks = c(0,1,2,3,10), labels = c("1","2","3", "4+")))
nrow(subset(df_parity, is.na(paritymaxcat_sur))) # 1
nrow(subset(df_parity, paritymax_sur == 0)) # 0
dat <- dat %>%
  left_join(df_parity, by = "rid_m") %>%
  relocate(paritymax_sur, .after = parity_sur) %>%
  relocate(paritymaxcat_sur, .after = paritymax_sur)

# categorize b114: education level
table(dat$b114, useNA = "always")
# b113 is education yes/no, b114 is level
# b114 is missing when b113 is no. 
table(dat$b113, dat$b114, useNA = "always")
#it is also missing for non-matched hdss records (NA for b113 and b114)
table(dat$b114, dat$type, useNA = "always")
# check that there is no one with multiple different non-na b114 values
# first recode as none when b113 = no
dat %>%
  filter(!is.na(b114)) %>%
  mutate(meducat = ifelse(b113 == "No", "None", b114)) %>%
  select(rid_m, meducat) %>%
  distinct() %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow() # 0
df_meducat <- dat %>%
  mutate(meducat = ifelse(b113 == "No", "None", as.character(b114))) %>%
  mutate(hasmeducat = ifelse(!is.na(meducat), 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(hasmeducat = sum(hasmeducat)) %>%
  mutate(meducat_sur = ifelse(hasmeducat == 0, "Missing", as.character(meducat))) %>%
  #select(rid_m, type, pregout_dss, c244, hasc244, observer_sur) %>% filter(rid_m == "2A00012205") 
  select(rid_m, meducat_sur) %>%
  distinct() %>%
  filter(!is.na(meducat_sur))
# check we have a value for all mothers
length(unique(dat$rid_m)[!(unique(dat$rid_m) %in% df_meducat$rid_m)]) # 0
# and no NA values
nrow(subset(df_meducat, is.na(meducat_sur))) # 0
dat <- dat %>%
  left_join(df_meducat, by = "rid_m") %>%
  relocate(meducat_sur, .after = b114) 

# categorize c244: whether someone was there during interview
table(dat$c244, useNA = "always")
# it is missing for the unmatched records from the HDSS
table(dat$c244, dat$type, useNA = "always")
# it is also missing for a few vs_match, vs_msc, and vs_nomatch records
# those should be recoded as missing.
# otherwise apply the survey value to all records.
# first check that there is no one with multiple different non-na c244 values
dat %>%
  filter(!is.na(c244)) %>%
  select(rid_m, c244) %>%
  distinct() %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow() # 0
# create one c244 per person, recoding as Missing where appropriate
df_int_observer <- dat %>%
  mutate(hasc244 = ifelse(!is.na(c244), 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(hasc244 = sum(hasc244)) %>%
  mutate(observer_sur = ifelse(hasc244 == 0, "Missing", as.character(c244))) %>%
  #select(rid_m, type, pregout_dss, c244, hasc244, observer_sur) %>% filter(rid_m == "2A00012205") 
  select(rid_m, observer_sur) %>%
  distinct() %>%
  filter(!is.na(observer_sur))
# check we have a value for all mothers
length(unique(dat$rid_m)[!(unique(dat$rid_m) %in% df_int_observer$rid_m)]) # 0
# and no NA values
nrow(subset(df_int_observer, is.na(observer_sur))) # 0
dat <- dat %>%
  left_join(df_int_observer, by = "rid_m") %>%
  relocate(observer_sur, .after = c244) 

# household size number
table(dat$a1, useNA = "always")
# it is only missing for the unmatched records from the HDSS
table(dat$a1, dat$type, useNA = "always")
# first check that there is no one with multiple different non-na a1 values
dat %>%
  filter(!is.na(a1)) %>%
  select(rid_m, a1) %>%
  distinct() %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow() # 0
# create one HH_size per person, recoding as Missing where appropriate
df_hhsize <- dat %>%
  mutate(hasHHsize = ifelse(!is.na(a1), 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(hasHHsize = sum(hasHHsize)) %>%
  mutate(hhsize_sur = ifelse(hasHHsize == 0, NA, a1)) %>%
  select(rid_m, hhsize_sur) %>%
  distinct() %>%
  filter(!is.na(hhsize_sur))
# check we have a value for all mothers
length(unique(dat$rid_m)[!(unique(dat$rid_m) %in% df_hhsize$rid_m)]) # 0
# and no NA values
nrow(subset(df_hhsize, is.na(hhsize_sur))) # 0
dat <- dat %>%
  left_join(df_hhsize, by = "rid_m") %>%
  relocate(hhsize_sur, .after = HH_size) 


# household size (small, medium, large)
table(dat$HH_size, useNA = "always")
# it is only missing for the unmatched records from the HDSS
table(dat$HH_size, dat$type, useNA = "always")
# first check that there is no one with multiple different non-na HH_size values
dat %>%
  filter(!is.na(HH_size)) %>%
  select(rid_m, HH_size) %>%
  distinct() %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow() # 0
# create one HH_size per person, recoding as Missing where appropriate
df_hhsizecat <- dat %>%
  mutate(hasHHsize = ifelse(!is.na(HH_size), 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(hasHHsize = sum(hasHHsize)) %>%
  mutate(hhsizecat_sur = ifelse(hasHHsize == 0, "Missing", as.character(HH_size))) %>%
  select(rid_m, hhsizecat_sur) %>%
  distinct() %>%
  filter(!is.na(hhsizecat_sur))
# check we have a value for all mothers
length(unique(dat$rid_m)[!(unique(dat$rid_m) %in% df_hhsizecat$rid_m)]) # 0
# and no NA values
nrow(subset(df_hhsizecat, is.na(hhsizecat_sur))) # 0
dat <- dat %>%
  left_join(df_hhsizecat, by = "rid_m") %>%
  relocate(hhsizecat_sur, .after = hhsize_sur) 


# household assets
table(dat$asset_quintile, useNA = "always")
# it is only missing for the unmatched records from the HDSS
table(dat$asset_quintile, dat$type, useNA = "always")
# first check that there is no one with multiple different non-na asset_quintile values
dat %>%
  filter(!is.na(asset_quintile)) %>%
  select(rid_m, asset_quintile) %>%
  distinct() %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow() # 0
# create one asset_quintile per person, recoding as Missing where appropriate
df_hhassets <- dat %>%
  mutate(hasasset_quintile = ifelse(!is.na(asset_quintile), 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(hasasset_quintile = sum(hasasset_quintile)) %>%
  mutate(hhassets_sur = ifelse(hasasset_quintile == 0, "Missing", as.character(asset_quintile))) %>%
  select(rid_m, hhassets_sur) %>%
  distinct() %>%
  filter(!is.na(hhassets_sur))
# check we have a value for all mothers
length(unique(dat$rid_m)[!(unique(dat$rid_m) %in% df_hhassets$rid_m)]) # 0
# and no NA values
nrow(subset(df_hhassets, is.na(hhassets_sur))) # 0
dat <- dat %>%
  left_join(df_hhassets, by = "rid_m") %>%
  relocate(hhassets_sur, .after = asset_quintile) 


# Create new --------------------------------------------------------------

# combined pregnancy outcome date (deferring to dss)
nrow(subset(dat, is.na(dob_c_dss))) # 679
nrow(subset(dat, is.na(c220))) # 536
dat <- dat %>%
  mutate(dob_c_comb = dplyr::if_else(!is.na(dob_c_dss), dob_c_dss, c220)) %>%
  mutate(dob_c_comb = as.Date(dob_c_comb, format = "%d-%b-%Y"))
nrow(subset(dat, is.na(dob_c_comb))) # 0

# combined date of death (deferring to dss)
nrow(subset(dat, !is.na(dod_c_dss))) # 694
nrow(subset(dat, !is.na(dod_c_sur))) # 716
dat <- dat %>%
  mutate(dod_c_comb = dplyr::if_else(!is.na(dod_c_dss), dod_c_dss, dod_c_sur)) %>%
  mutate(dod_c_comb = as.Date(dod_c_comb, format = "%d-%b-%Y"))

# combined birth order (deferring to dss)
dat <- dat %>%
  mutate(birthorder_comb = coalesce(as.numeric(parity_dss), as.numeric(parity_sur))) %>%
  mutate(birthorder_cat_comb = cut(birthorder_comb, breaks = c(0,1,2,3,100), labels = c("1","2","3", "4+")))
# # check breaks
# dat %>%
#   select(birthorder_comb,birthorder_cat_comb) %>%
#   distinct() %>% View

# combined parity max (deferring to dss)
dat <- dat %>%
  mutate(paritymax_comb = coalesce(paritymax_dss, paritymax_sur)) %>%
  mutate(paritymaxcat_comb = cut(paritymax_comb, breaks = c(0,1,2,3,10), labels = c("1","2","3", "4+")))

# combined cstatus (deferring to dss)
dat <- dat %>%
  mutate(cstatus_comb = coalesce(cstatus_dss, cstatus_sur))

# combined cstatus_agep (deferring to dss)
dat <- dat %>%
  mutate(cstatus_agesp_comb = coalesce(cstatus_agesp_dss, cstatus_agesp_sur))

# birth recency
dat <- dat %>%
  mutate(birthrecency = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dob_c_comb)/365.25,
         birthrecency_dss = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dob_c_dss)/365.25,
         birthrecency_sur = as.numeric(as.Date(max(unique(dat$int_date_sur))) - c220)/365.25,
           ) %>%
  mutate(birthrecency_cat = cut(birthrecency, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+")),
         birthrecency_cat_dss = cut(birthrecency_dss, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+")),
         birthrecency_cat_sur = cut(birthrecency_sur, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+"))
         ) 
  #select(dob_c_comb, birth_recency, birth_recency_cat) %>% filter(birth_recency == 10)
## check breaks
# dat %>%
#   mutate(birthrecency = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dob_c_comb)/365.25,
#          birthrecency_dss = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dob_c_dss)/365.25,
#          birthrecency_sur = as.numeric(as.Date(max(unique(dat$int_date_sur))) - c220)/365.25,
#   ) %>%
#   mutate(birthrecency_cat = cut(birthrecency, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+")),
#          birthrecency_cat_dss = cut(birthrecency_dss, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+")),
#          birthrecency_cat_sur = cut(birthrecency_sur, breaks = c(-1,4,9,14,100), labels = c("0-4","5-9", "10-14", "15+"))
#   ) %>%
#   select(birthrecency, birthrecency_cat, birthrecency_dss, birthrecency_cat_dss, birthrecency_sur, birthrecency_cat_sur)

# death recency
dat <- dat %>%
  mutate(deathrecency = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dod_c_comb)/365.25,
         deathrecency_dss = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dod_c_dss)/365.25,
         deathrecency_sur = as.numeric(as.Date(max(unique(dat$int_date_sur))) - dod_c_sur)/365.25,
  ) %>%
  mutate(deathrecency_cat = cut(deathrecency, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+")),
         deathrecency_cat_dss = cut(deathrecency_dss, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+")),
         deathrecency_cat_sur = cut(deathrecency_sur, breaks = c(-1,5,10,15,100), labels = c("0-4","5-9", "10-14", "15+"))) 
nrow(subset(dat, is.na(deathrecency))) # 2387

# create child strata for just age
dat <- dat %>%
  mutate(cstrata_a = case_when( 
    cstrata_ac == "Surviving" ~ "Surviving",
    cstrata_ac == "Neonatal (other)" ~ "Neonatal",
    cstrata_ac == "Neonatal (birth asphyxia)" ~ "Neonatal",
    cstrata_ac == "Postneonatal (other)" ~ "Postneonatal",
    cstrata_ac ==  "Postneonatal (RI+con)"  ~ "Postneonatal",
    cstrata_ac == "1-4 year (other)" ~ "1-4 year",
    cstrata_ac ==  "1-4 year (drowning)"  ~ "1-4 year",
    TRUE ~ cstrata_ac
  )) 

# create child strata for just cause
dat <- dat %>%
  mutate(cstrata_c = case_when( 
    cstrata_ac == "Surviving" ~ "Surviving",
    cstrata_ac == "Neonatal (other)" ~ "Other",
    cstrata_ac == "Neonatal (birth asphyxia)" ~ "Birth asphyxia",
    cstrata_ac == "Postneonatal (other)" ~ "Other",
    cstrata_ac ==  "Postneonatal (RI+con)"  ~ "RI and congenital",
    cstrata_ac == "1-4 year (other)" ~ "Other",
    cstrata_ac ==  "1-4 year (drowning)"  ~ "Drowning",
    TRUE ~ cstrata_ac
  )) 

# add ordered recnr
dat <- dat %>%
  arrange(rid_m, dob_c_comb) %>%
  mutate(recnr = 1:n()) %>%
  relocate(recnr)

# Check id vars -----------------------------------------------------------

nrow(dat) # 3185

# mother_id
length(unique(dat$rid_m)) # 848
nrow(subset(dat, is.na(rid_m))) # 0

# child id in dss
length(unique(dat$rid_c)) # 2163

# count from 1 to n in survey
length(unique(dat$serial)) # 2649
# serial per respondent
length(unique(dat$serial1)) # 849

# child unique id serial1 + parity in survey
length(unique(dat$uid_c_sur)) # 2649
# child unique id serial1 + parity in dss
length(unique(dat$uid_c_dss)) # 2506

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/augment/overallDob-recode.rds")

