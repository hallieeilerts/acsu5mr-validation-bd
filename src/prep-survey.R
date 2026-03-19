################################################################################
#' @description Prepare survey data
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
# survey_final_all2: livebirth and stillbirth records from the survey
dat <- read_dta("./data/20250930/survey_final_all2.dta")
################################################################################

# parity variable investigation
# how is this parity variable created? does it just match the number entry for the preg?
unique(dat$match_n)
dat %>%
  filter(match_n == 3) %>%
  select(rid_m, parity_n_sur, parity_s_sur) %>%
  arrange(rid_m, parity_n_sur) %>%
  head()
dat %>%
  select(rid_m, c215_a, c223, parity_n_sur, parity_s_sur) %>%
  filter(c215_a != parity_n_sur)
dat %>%
  filter(rid_m == "3D90015808") %>%
  select(rid_m, c215_a, c223, parity_n_sur, parity_s_sur) %>%
  arrange(c215_a)
# in the survey, this person has parity going up to 6. and it includes 2 abortions.

# examine labels
labels <- sapply(dat, function(x) attr(x, "label") %||% NA_character_)
df_labels <- tibble(
  variable = names(labels),
  label = labels)
#View(df_labels)

nrow(dat) # 2648
length(unique(dat$rid_m)) # 848
length(unique(dat$rid_c)) # 1970
length(unique(dat$serial)) # 2648 is the unique variable

# rid_m is never missing for mothers
nrow(subset(dat, is.na(rid_m))) # 0

# replace blank spaces with NA
dat <- dat %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .)))

# not all children have an rid_c
# when child's rid_c is missing, it could be any pregnancy outcome
table(subset(dat, is.na(rid_c))$c223, useNA = "always")

# convert labeled values to factors
dat <- dat %>%
  mutate(across(where(is.labelled), ~as_factor(.)))

# pregnancy outcome
dat <- dat %>%
  mutate(preg_res_surv = as.character(c223)) %>%
  mutate(preg_res_surv = case_when(
    preg_res_surv == "Born alive" ~ "Live birth",
    preg_res_surv == "Born dead" ~ "Stillbirth",
    TRUE ~ preg_res_surv
  ))

# child status
dat <- dat %>%
  mutate(cstatus_surv = case_when(
    preg_res_surv == "Live birth" & c224 == "No" ~ "Died",
    preg_res_surv == "Live birth" & c224 == "Yes" ~ "Surviving",
    preg_res_surv == "Miscarriage" ~ "Miscarriage",
    preg_res_surv == "Abortion" ~ "Abortion",
    TRUE ~ preg_res_surv
  ))


# Initial removal of unnecessary columns
dat <- dat %>%
  select(-c(c200, # section start time
            c216, c216_a, cal_216, # non final preg outcome information
            c218, c222, c222_a, cal_223, # child name, other preg before or in between, bangla preg condition
            `_id`, `_uuid`, `_submission_time`, `_tags`, `_notes`, # empty variables
            Location, `_Location_latitude`, `_Location_longitude`, `_Location_altitude`, `_Location_precision`, 
            serial2, s3,  # non unique Ids
            a00, a0_1, a1, # section start time, respondent name, hh member number
            a3, a3_a, a4_a, a5, a5_a, a6, a6_a, a7, # roof and floor material, drinking water, asset concatencation
            starts_with("w"), # asset ownership
            a000, b00, b000, c100, c1000, c2000, c3000, d00, d000, dd, # a end time, b start time, b end time, c start time
            `__version__`, metainstanceID, `_parent_index`,
            name_head, name_m, cid_m, # mother names and ids
            uid_c, uid_c_sur))

# create column for date of birth
#View(dat[, c("c220","c220_a", "c223")])
# c220_a only has a value when it was a non live birth
nrow(subset(dat, is.na(c220))) # 0
# c220 is never missing
dat <- dat %>%
  select(-c220_a)
# rename c220 to match overall file
dat <- dat %>%
  rename(dob_c_sur = c220)

# convert int_date to a date
dat <- dat %>%
  mutate(int_date = as.Date(int_date, format = "%Y-%m-%d"))

# create single column for age of death
# these columns are a bit inconsistent
# c228_cc is free text
# c228_B seems to be a transformation of c228_aa, c228_bb, c228_ccc but sometimes has a date
# c228_d is a yes or no
dat[!is.na(dat$c224) & dat$c224 == "No", c("c224","c228", "c228_aa", "c228_bb", "c228_ccc", "c228_cc", "c228_d", "c228_B")]
dat$aad_unit <- NA
dat <- dat %>% mutate(aad_unit = case_when(
  c228 == "In days" ~ 1,
  c228 == "In months" ~ 2,
  c228 == "In years" ~ 3,
  TRUE ~ NA
)) %>%
  mutate(aad_value = case_when(
    aad_unit == 1 ~ c228_aa,
    aad_unit == 2 ~ c228_bb,
    aad_unit == 3 ~ c228_ccc,
    TRUE ~ NA
  ))
# Check that aad_unit never missing when c228 reported
nrow(subset(dat, is.na(aad_unit) & !is.na(c228) & c228 == "Yes")) # 0
# Check that aad_value never missing when c228 unit reported
nrow(subset(dat, is.na(aad_value) & (!is.na(c228_aa) | !is.na(c228_bb) | !is.na(c228_ccc) ))) # 0
# Transform into months
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
# Transform into days
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25
# Transform into years
dat$aady <- dat$aad_value
dat$aady[which(dat$aad_unit == 1)] <- dat$aady[which(dat$aad_unit == 1)] / 365.25
dat$aady[which(dat$aad_unit == 2)] <- dat$aady[which(dat$aad_unit == 2)] / 12
# Delete extra cod columns
dat <- dat %>%
  select(-c(c228, c228_aa, c228_bb, c228_ccc, c228_cc, c228_d, c228_B))

# assign child-level strata by age
dat <- dat %>%
  mutate(cstrata_a_surv = case_when(
    cstatus_surv == "Abortion" ~ "Abortion",
    cstatus_surv == "Miscarriage" ~ "Miscarriage",
    cstatus_surv == "Stillbirth" ~ "Stillbirth",
    cstatus_surv == "Surviving" ~ "Surviving",
    cstatus_surv == "Died" & aadd < 28 ~ "Neonatal",
    cstatus_surv == "Died" & aadd >= 28 & aadd < 365 ~ "Postneonatal",
    cstatus_surv == "Died" & aadd >= 365 & aadd < 5*365 ~ "1-4",
    cstatus_surv == "Died" & aadd >= 5*365 & aadd < 10*365 ~ "5-9",
    cstatus_surv == "Died" & aadd >= 10*365 ~ "10+",
    TRUE ~ NA
  )) 

# Rename columns that actually came from the DSS to make that clear
dat <- dat %>%
  rename(dob_m_dss = dob_m,
         coo_m_dss = coo_m,
         doo_m_dss = doo_m)

# seem to be some variables that are permutations of others
# would be good to only keep essential ones out of c220, c220_a, c220_b, etc

# examine new variables
# dat %>%
#   select(match_n, serial1, rid_m, rid_c, sample, sample2, std_peri, age_out_m, age_extraction_m, mig_result, mig_result) %>%
#   View()
unique(dat$sample) # mother-level strata by age of index child
unique(dat$sample2) # mother-level strata by age/cod of index child
# Rename mother-level strata to clarify whether by age or cause
dat <- dat %>%
  rename(
    mstrata_a = sample, 
    mstrata_ac = sample2
  )

# !!!! i used to remove rid_m in line 39
# keeping now

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/survey-clean.rds")


