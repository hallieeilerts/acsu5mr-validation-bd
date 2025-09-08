################################################################################
#' @description Add records missing records from HDSS to overall that should be there
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
overall <- readRDS("./gen/overall-clean.rds")
hdss <- readRDS("./gen/hdss-clean.rds")
survey <- readRDS("./gen/survey-clean.rds")
################################################################################

# Here i want to add rows to overall file to make it a true overall file.
# I will then add a column that categorizes all observations

# Those with a missing match score are validation study observations that weren't matched to HDSS.
overall$type <- ifelse(is.na(overall$match_score), "VS_NoMatch", "VS_Match")
table(overall$match_score, useNA = "always")
table(overall$type, useNA = "always")

# For those that were merged, they don't include all variables from HDSS.
# cod_c_dss in original data
# child strata variables that i added: "status_c", "sample_c", "sample_c1" 
overall <- merge(overall, hdss[,c("uid_c_dss", "cod_c_dss", "status_c", "sample_c", "sample_c1", "sample_c_unit")], by = "uid_c_dss", all.x = TRUE)

# sample_c and sample_c1 are added for matched study/survey observations in prep-hdss ("VS_Match")
# add it for study observations which didn't match with HDSS ("VS_NoMatch")
overall <- overall %>%
  mutate(sample_c1 = case_when(
          type == "VS_NoMatch" & is.na(sample_c1) & c223 == "Stillbirth" ~ "4", 
          type == "VS_NoMatch" & is.na(sample_c1) & c224 == "Yes" ~ "0",
          type == "VS_NoMatch" & is.na(sample_c1) & c224 == "No" & aadd < 28 ~ "1", 
          type == "VS_NoMatch" & is.na(sample_c1) & c224 == "No" & aadd >= 28 & aadd < 365 ~ "2", 
          type == "VS_NoMatch" & is.na(sample_c1) & c224 == "No" & aadd >= 365 & aadd < 365*5 ~ "3",
          type == "VS_NoMatch" & is.na(sample_c1) & c224 == "No" & aadd >= 365*5 & aadd < 365*10 ~ "5",
          type == "VS_NoMatch" & is.na(sample_c1) & c224 == "No" & aadd >= 365*10 ~ "0",
          TRUE ~ sample_c1),
    sample_c = case_when(
          type == "VS_NoMatch" & is.na(sample_c) & c223 == "Stillbirth" ~ "4_Stillbirth",
          type == "VS_NoMatch" & is.na(sample_c) & c224 == "Yes" ~ "0_Surviving",
          type == "VS_NoMatch" & is.na(sample_c) & c224 == "No" & aadd < 28 ~ "1_Unk", 
          type == "VS_NoMatch" & is.na(sample_c) & c224 == "No" & aadd >= 28 & aadd < 365 ~ "2_Unk", 
          type == "VS_NoMatch" & is.na(sample_c) & c224 == "No" & aadd >= 365 & aadd < 365*5 ~ "3_Unk",
          type == "VS_NoMatch" & is.na(sample_c) & c224 == "No" & aadd >= 365*5 & aadd < 365*10 ~ "5_Unk",
          type == "VS_NoMatch" & is.na(sample_c) & c224 == "No" & aadd >= 365*10 ~ "0_10+",
          TRUE ~ sample_c)
    )
table(subset(overall, type == "VS_NoMatch")$sample_c, useNA = "always")
table(subset(overall, type == "VS_NoMatch")$sample_c1, useNA = "always")
nrow(subset(overall, type == "VS_NoMatch" & is.na(sample_c))) # 0
nrow(subset(overall, type == "VS_NoMatch" & is.na(sample_c1))) # 0

# From the validation study
# miscarriages for which no matching with hdss was attempted (VS - MSC)
# abortions for which no matching with hdss was attempted (VS - AB)
survey_nomatch <- subset(survey, !(serial %in% overall$serial))
nrow(survey_nomatch) # 262
survey_nomatch$type <- NA
survey_nomatch$type[survey_nomatch$c223 == "Miscarriage"] <- "VS_MSC"
survey_nomatch$type[survey_nomatch$c223 == "Abortion"] <- "VS_AB"
table(survey_nomatch$type, useNA = "always")

# From the HDSS
# live births, stillbirths, miscarriage, abortions that were not matched to HDSS (HDSS - not matched)
hdss_nomatch <- subset(hdss, !(uid_c_dss %in% overall$uid_c_dss))
nrow(hdss_nomatch)
hdss_nomatch$type <- "HDSS_NoMatch"
hdss_nomatch$type[hdss_nomatch$preg_res_dss == "Miscarriage"] <- "HDSS_MSC"
hdss_nomatch$type[hdss_nomatch$preg_res_dss == "Abortion"] <- "HDSS_AB"
table(hdss_nomatch$type, useNA = "always")

# Combine overall and survey_nomatch
dat <- bind_rows(overall, survey_nomatch)
# Calculate dod_c_sur
dat$dod_c_sur <- dat$dob_c_sur + dat$aadd
#View(dat[!is.na(dat$c224) & dat$c224 == "No", c("dob_c_sur", "aad_unit", "aad_value", "aadd", "aadm", "aady", "dod_c_sur")])

# Combine overall and hdss_nomatch         
dat <- bind_rows(dat, hdss_nomatch)

# Calculate maternal age
dat <- dat %>%
  mutate(matage_simp = 2025 - year(dob_m_dss),
         matage_cat = cut(matage_simp,
                        breaks = seq(15,55, 5), 
                        labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-55")))
nrow(subset(dat, is.na(matage_simp))) # 0
nrow(subset(dat, is.na(matage_cat))) # 0
  
# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/overall-aug.rds")


