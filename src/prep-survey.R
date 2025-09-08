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
dat <- read_dta("./data/ACSU5MR_FILES/survey_final_all2.dta")
################################################################################

# convert labeled values to factors
dat <- dat %>%
  mutate(across(where(is.labelled), ~as_factor(.)))

# Initial removal of unnecesary columns
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
            name_head, name_m, cid_m, rid_m, # mother names and ids
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

# Rename columns that actually came from the DSS to make that clear
dat <- dat %>%
  rename(dob_m_dss = dob_m,
         coo_m_dss = coo_m,
         doo_m_dss = doo_m)

# seem to be some variables that are permutations of others
# would be good to only keep essential ones out of c220, c220_a, c220_b, etc

# Coalesce va
dat <- dat %>%
  mutate(va = coalesce(va1, va2, va3, va4, va5, va6, va7)) %>%
  select(-c(va1, va2, va3, va4, va5, va6, va7))

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/survey-clean.rds")


