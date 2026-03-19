################################################################################
#' @description Prepare hdss data
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

# examine labels
labels <- sapply(dat, function(x) attr(x, "label") %||% NA_character_)
df_labels <- tibble(
  variable = names(labels),
  label = labels)
#View(df_labels)

nrow(dat) # 2505
length(unique(dat$rid_m)) # 848
length(unique(dat$rid_c)) # 2157
length(unique(dat$serial1)) # 848
length(unique(dat$uid_c)) # 2505 unique
length(unique(dat$uid_c_dss)) # 2505 unique

# replace blank spaces with NA
dat <- dat %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .)))

# convert labeled values to factors
dat <- dat %>%
  mutate(across(where(is.labelled), ~as_factor(.)))

# Initial removal of unnecessary columns
dat <- dat %>%
  select(-c(name_c, # name_m, removed at bottom of script
            uid_c)) # remove because is the same as uid_c_dss

# Renaming HDSS columns that want to keep in overall
dat <- dat %>%
  mutate(preg_res_dss = as.character(po),
         cod_c_dss = CCOD) %>%
  mutate(preg_res_dss = case_when(
    preg_res_dss == "Livebirth" ~ "Live birth",
    TRUE ~ preg_res_dss
  )) %>%
  select(-c(po, CCOD))
table(dat$preg_res_dss, useNA = "always") # 0 NA

# Renaming HDSS columns that are in overall under a different name
# These are present for matched observations
# I want to make a file with all matched and unmatched. Thus, I want the names to also be in the same in HDSS.
dat <- dat %>%
  rename(dod_c_dss = dod_c,
         coo_m_dss = coo_m,
         doo_m_dss = doo_m,
         dob_m_dss = dob_m,
         dob_c_dss = dob_c,
         f_intype_dss = f_intype,
         sex_c_dss = sex_c)

# date formatting
unique(dat$dod_c_dss) # ok
unique(dat$doo_m_dss) # ok
unique(dat$dob_m_dss) # ok
unique(dat$dob_c_dss) # ok
#dat$dod_c_dss <- as.Date(dat$dod_c_dss, format = "%d-%b-%Y")
#dat$dob_m_dss <- as.Date(dat$dob_m_dss, format = "%d-%b-%Y")

# COD
dat %>% 
  filter(rid_m == "2J00000707") %>%
  select(rid_m, preg_res_dss, dob_c_dss, dod_c_dss, alive, cod_c_dss)

dat$aadd <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)

dat <- dat %>%
  mutate(cstatus = case_when(
    preg_res_dss == "Live birth" & !is.na(dod_c_dss) ~ "Died",
    preg_res_dss == "Live birth" & is.na(dod_c_dss) ~ "Surviving",
    preg_res_dss == "Miscarriage" ~ "Miscarriage",
    preg_res_dss == "Abortion" ~ "Abortion",
    TRUE ~ preg_res_dss
  ))

# assign child-level strata by age
dat <- dat %>%
  mutate(cstrata_a = case_when(
    cstatus == "Abortion" ~ "Abortion",
    cstatus == "Miscarriage" ~ "Miscarriage",
    cstatus == "Stillbirth" ~ "Stillbirth",
    alive == "Alive" ~ "Surviving",
    alive == "Died" & aadd < 28 ~ "Neonatal",
    alive == "Died" & aadd >= 28 & aadd < 365 ~ "Postneonatal",
    alive == "Died" & aadd >= 365 & aadd < 5*365 ~ "1-4",
    alive == "Died" & aadd >= 5*365 & aadd < 10*365 ~ "5-9",
    alive == "Died" & aadd >= 10*365 ~ "10+",
    TRUE ~ NA
  )) 

# convert mother's in-migration date
dat <- dat %>%
  mutate(mot_in_date = as.Date(mot_in_date, format = "%d-%b-%Y"))

dat %>%
  filter(rid_m %in% c("2U00091606", "1G00029040", "3N00031810")) %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, cstatus, alive, cstrata_a) %>%
  head()

# tidy
dat <- dat[order(dat$rid_m, dat$dob_c_dss), ]

# remove columns
dat$name_m <- NULL
dat$aadd <- NULL # this variable is in overall, and it comes from survey
nrow(dat)

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/hdss-clean.rds")

