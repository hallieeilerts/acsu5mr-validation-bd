################################################################################
#' @description Prepare hdss data for augmenting overall file
#' (overall file contains all survey obs, with matched hdss obs. need to add unmatched hdss)
#' Drop unnecessary variables
#' Clean variables
#' Standardize variable names
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
## hdss_final_all2: livebirth and stillbirth records from the HDSS
# Contains all updated HDSS data of the mother and child
# Newly added variables: CCOD cause of death up to May 31, 2025; 
# alive (vital status on survey date); date and type of Mother’s first entry to Matlab
dat <- read_dta("./data/20250930/hdss_final_all2.dta")
################################################################################

# Variable examination ----------------------------------------------------

# parity variable investigation
dat %>%
  select(rid_m, parity_n_dss, parity_s_dss) %>%
  arrange(rid_m, parity_n_dss) %>%
  head()
dat %>%
  filter(rid_m == "1A00037540") %>%
  select(rid_m, parity_n_dss, parity_s_dss) %>%
  arrange(rid_m, parity_n_dss) %>%
  head()
dat %>%
  filter(rid_m == "3D90015808")  %>%
  select(rid_m, po, parity_n_dss, parity_s_dss) %>%
  arrange(rid_m, parity_n_dss)
# in the hdss, this person has parity going up to 4 and it does not include abortions.

nrow(dat) # 2505
length(unique(dat$rid_m)) # 848
length(unique(dat$rid_c)) # 2157
head(sort(table(dat$rid_c), decreasing = TRUE)) # 341 blanks, 5 1's and 5 2's
length(unique(dat$serial1)) # 848
length(unique(dat$uid_c)) # 2505 unique
length(unique(dat$uid_c_dss)) # 2505 unique

# these are the same
nrow(subset(dat, uid_c != uid_c_dss)) # 0

# parity variable is a count, not max parity
dat %>%
  filter(rid_m == "3D90015808")  %>%
  select(rid_m, po, parity_n_dss) 

# unique child id variable is uid_c
# rid_c is not unique
nrow(dat) # 2505
length(unique(dat$rid_c)) # 2157
nrow(subset(dat, is.na(rid_c))) # 0
length(unique(dat$uid_c)) # 2505
nrow(subset(dat, is.na(uid_c))) # 0


# Select variables --------------------------------------------------------

# Remove unnecessary variables
dat <- dat %>%
  select(-c(
    serial1,
    uid_c, # # child unique id serial1 + parity, same as uid_c_dss
    alive, # alive on survey date
    parity_s_dss # parity string dss
  ))

# keep variables and order
dat <- dat %>%
  select(
    rid_m, # mothers RID
    rid_c, # children rid
    uid_c_dss, # child unique id DSS serial1+parity
    sd, # interview date
    name_m, # mother name
    dob_m,  # mother dob
    coo_m,  # mother cause of out
    doo_m,  # mother date of out
    mot_in_date, # mothers first in date
    f_intype, # first in type of mother
    parity_n_dss, # parity number dss
    po, # pregnancy outcome type
    name_c, # child name
    sex_c, # child sex
    dob_c, # child dob
    dod_c, # child dod
    CCOD, # child cause of death
    everything()
  )

# Clean ------------------------------------------------------------------

# replace blank spaces with NA
dat <- dat %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .)))

# recode "na"
dat <- dat %>%
  mutate(across(where(is.character), ~ {
    x <- str_trim(.)
    x[str_detect(x, regex("^n\\s*/?\\s*a$", ignore_case = TRUE))] <- NA
    x
  }
  ))

# convert labeled values to factors
dat <- dat %>%
  mutate(across(where(is.labelled), ~as_factor(.)))

# convert to date
dat <- dat %>%
  mutate(mot_in_date = as.Date(mot_in_date, format = "%d-%b-%Y"))

# Rename variables with dss suffix ----------------------------------------

# Rename columns that actually came from the DSS to make that clear
dat <- dat %>%
  rename(dob_m_dss = dob_m,
         coo_m_dss = coo_m,
         doo_m_dss = doo_m,
         doi_m_dss = mot_in_date,
         toi_m_dss = f_intype,
         name_m_dss = name_m,
         pregout_dss = po,
         name_c_dss = name_c,
         sex_c_dss = sex_c,
         dob_c_dss = dob_c,
         dod_c_dss = dod_c,
         cod_c_dss = CCOD,
         parity_dss = parity_n_dss)

# Rename variables with sur suffix ----------------------------------------

# clarify that not from hdss

dat <- dat %>%
  rename(int_date_sur = sd)


# # Recode values -----------------------------------------------------------
# 
# # pregnancy outcome
# dat <- dat %>%
#   mutate(pregout_dss = as.character(pregout_dss)) %>%
#   mutate(pregout_dss = case_when(
#     pregout_dss == "Livebirth" ~ "Live birth",
#     TRUE ~ pregout_dss
#   ))

# Create new variables ----------------------------------------------------

# age at death in days
dat$aadd_dss <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)
dat$aadm_dss <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)/30.5
dat$aady_dss <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)/365.25

# child status variable which combines survival status with adverse pregnancy outcomes
dat <- dat %>%
  mutate(cstatus_dss = case_when( # formerly cstatus
    pregout_dss == "Livebirth" & !is.na(dod_c_dss) ~ "Died",
    pregout_dss == "Livebirth" & is.na(dod_c_dss) ~ "Surviving",
    pregout_dss == "Miscarriage" ~ "Miscarriage",
    pregout_dss == "Abortion" ~ "Abortion",
    TRUE ~ pregout_dss
  ))

# assign child-level strata by age
dat <- dat %>%
  mutate(cstatus_agesp_dss = case_when( # formerly cstrata_a_dss
    cstatus_dss == "Abortion" ~ "Abortion",
    cstatus_dss == "Miscarriage" ~ "Miscarriage",
    cstatus_dss == "Stillbirth" ~ "Stillbirth",
    cstatus_dss == "Surviving" ~ "Surviving",
    cstatus_dss == "Died" & aadd_dss < 28 ~ "Neonatal",
    cstatus_dss == "Died" & aadd_dss >= 28 & aadd_dss < 365 ~ "Postneonatal",
    cstatus_dss == "Died" & aadd_dss >= 365 & aadd_dss < 5*365 ~ "1-4",
    cstatus_dss == "Died" & aadd_dss >= 5*365 & aadd_dss < 10*365 ~ "5-9",
    cstatus_dss == "Died" & aadd_dss >= 10*365 ~ "10+",
    TRUE ~ NA
  )) 


# tidy --------------------------------------------------------------------

# tidy
dat <- dat[order(dat$rid_m, dat$dob_c_dss), ]


# check to see have all desired variables from both survey and hdss
dat %>%
  select(rid_m, # mother id for both survey and dss, date of interview
         #mstrata_a, mstrata_ac, mstrata_c, # mother-level strata drawn from dss for sampling. not provided in this file.
         rid_c, pregout_dss, cstatus_dss, cstatus_agesp_dss, dob_c_dss, dod_c_dss, cod_c_dss # child-level information from dss
         ) %>%
  head()

# dat %>%
#   filter(rid_m %in% "3DX1016507") %>%
#   select(rid_m, # mother id for both survey and dss, date of interview
#          #mstrata_a, mstrata_ac, mstrata_c, # mother-level strata drawn from dss for sampling. not provided in this file.
#          rid_c, pregout_dss, cstatus_dss, cstatus_agesp_dss, dob_c_dss, dod_c_dss, cod_c_dss # child-level information from dss
#   ) %>%
#   View()

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/clean/hdss-clean.rds")

