################################################################################
#' @description Overall contains VS records that matched to HDSS and VS records that did not.
#' To make a true overall file, add records from HDSS that didn't match to VS.
#' Also add VS records of abortion and miscarriage for which matching wasn't attempted.
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
overall <- readRDS("./gen/overall-date-clean.rds")
hdss <- readRDS("./gen/hdss-clean.rds")
survey <- readRDS("./gen/survey-clean.rds")
################################################################################

# unique identifier
df_uniq_vals <- as.data.frame(apply(overall, 2, function(x) length(unique(x))))
df_uniq_vals$variable <- row.names(df_uniq_vals)
row.names(df_uniq_vals) <- NULL
names(df_uniq_vals) <- c("n", "variable")
subset(df_uniq_vals, n == nrow(overall))
# there is currently no variable that serves as a unique identifier

# do all observations have a mother_id?
nrow(overall) # 2387
length(unique(overall$rid_m)) # 847
nrow(subset(overall, is.na(rid_m))) # 0
# yes. make sure they do at the end of this script once file has been augmented.

# Those with a missing match score are validation study observations that weren't matched to HDSS.
overall$type <- ifelse(is.na(overall$match_score), "VS_NoMatch", "VS_Match")
table(overall$match_score, useNA = "always")
table(overall$type, useNA = "always")

# Add variables from HDSS -------------------------------------------------

# Already present in the overall file was the mother-level age and age/cause strata
unique(overall$mstrata_a) # 2024all, Strata6, Stillbirth, Neonatal, 1-4, Postneonatal, 5-9    
unique(overall$mstrata_ac)# 2024all, Live birth, Stillbirth, Neonatal (birth asphyxia), 1-4 year (other), Postneonatal (RI+con),
# Neonatal (other), 5-9 year, Postneonatal (other), 1-4 year (drowning)  

# Add some extra variables related to strata that we created in prep-hdss
# cstatus: the child status variable
## Abortion, Miscarriage, Stillbirth, Died, Surviving
# cstrata_a: the child-level age strata
## "Surviving"    "Postneonatal" "Stillbirth"   "Neonatal"  "1-4"   "5-9" "10+" 
# Also want to add the child-level COD and DOD from the HDSS
# cod_c_dss: child ICD code

# For VS live births and stillbirths that were matched to HDSS, add cstatus and cstrata_a
overall <- merge(overall, hdss[,c("uid_c_dss", "dod_c_dss", "cod_c_dss", "cstatus", "cstrata_a")], by = "uid_c_dss", all.x = TRUE)
nrow(subset(overall, type == "VS_NoMatch" & is.na(cstatus))) # 485
nrow(subset(overall, type == "VS_Match" & is.na(cstatus)))   # 0
nrow(subset(overall, type == "VS_NoMatch" & is.na(cstrata_a))) # 485
nrow(subset(overall, type == "VS_Match" & is.na(cstrata_a)))   # 0
nrow(subset(overall, type == "VS_NoMatch" & is.na(cod_c_dss))) # 485
nrow(subset(overall, type == "VS_Match" & is.na(cod_c_dss) & alive == "Died")) # 2
# cstatus
unique(overall$cstatus) # Surviving, Died, Stillbirth, NA
# does not include Abortion, Miscarriage because matching wasn't attempted for this in validation study
# will add cstatus for non-matched VS abortions and miscarriages later
# the NA values are the VS live births and stillbirths that didn't match to HDSS
table(overall$cstatus, useNA = "always")
table(subset(overall, type == "VS_NoMatch")$cstatus, useNA = "always") # 485
# cstrata_a
unique(overall$cstrata_a) # "Surviving" "Postneonatal" "Stillbirth" "Neonatal" "1-4" "5-9" "10+" NA
# the NA values are the VS live births and stillbirths that didn't match to HDSS
table(overall$cstrata_a, useNA = "always")
table(subset(overall, type == "VS_NoMatch")$cstrata_a, useNA = "always") # 485
# the NA values are the VS live births and stillbirths that didn't match to HDSS
table(subset(overall, type == "VS_NoMatch")$cod_c_dss, useNA = "always") # 485

# For VS live births and stillbirths that were not matched to HDSS, add cstatus and cstrata_a
# can be generated from the information in the survey
# not possible to add the COD (which only comes from HDSS)
overall <- overall %>%
  mutate(
    cstatus = case_when(
      type == "VS_NoMatch" & is.na(cstatus) & c223 == "Stillbirth" ~ "Stillbirth",
      type == "VS_NoMatch" & is.na(cstatus) & c223 == "Live birth" & c224 == "Yes" ~ "Surviving",
      type == "VS_NoMatch" & is.na(cstatus) & c223 == "Live birth" & c224 == "No" ~ "Died",
      TRUE ~ cstatus),
    cstrata_a = case_when(
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Stillbirth" ~ "Stillbirth",
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Live birth" & c224 == "Yes" ~ "Surviving",
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Live birth" & c224 == "No" & aadd < 28 ~ "Neonatal",
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Live birth" & c224 == "No" & aadd >= 28 & aadd < 365 ~ "Postneonatal",
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Live birth" & c224 == "No" & aadd >= 365 & aadd < 365*5 ~ "1-4",
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Live birth" & c224 == "No" & aadd >= 365*5 & aadd < 365*10 ~ "5-9",
      type == "VS_NoMatch" & is.na(cstrata_a) & c223 == "Live birth" & c224 == "No" & aadd >= 365*10 ~ "10+",
      TRUE ~ cstrata_a
    )
  )
table(overall$cstatus, useNA = "always") # 0 NA
table(subset(overall, type == "VS_NoMatch")$cstatus, useNA = "always") # 0 NA
table(overall$cstrata_a, useNA = "always") # 0 NA
table(subset(overall, type == "VS_NoMatch")$cstrata_a, useNA = "always") # 0 NA
nrow(subset(overall, type == "VS_NoMatch" & is.na(cstatus)))   # 0
nrow(subset(overall, type == "VS_NoMatch" & is.na(cstrata_a))) # 0
table(overall$type, useNA = "always")

# Parity
# merge on mother-level parity from HDSS
hdss_parity <- hdss %>%
  group_by(rid_m) %>%
  summarise(parity_dss = max(parity_n_dss)) 
table(hdss_parity$parity_dss, useNA = "always")
hdss_parity <- hdss_parity %>%
  mutate(paritycat_dss = cut(parity_dss,
                          breaks = c(0, 1, 2, 3, Inf), 
                          labels = c("1", "2", "3", "4+")))
table(hdss_parity$paritycat_dss, useNA = "always")
overall <- overall %>% left_join(hdss_parity, by = "rid_m")
nrow(subset(overall, is.na(parity_dss)))
# delete parity variables that are counts that I think has come from the FPH
overall$parity <- NULL
overall$parity0 <- NULL

# Create icd/mother strata key --------------------------------------------

# I want to add a child-level categorized COD to matched observations in overall.

# I also want to add a categorized COD to unmatched HDSS observations.
# I have icd codes for HDSS that could be used to categorize COD for non-matched observations.
# However, I need to categorize the ICD codes in the same way as the age/cause mother strata in the overall file.
# The easiest way to do this seems to be creating a key from overall.

# Create key from VS live births and stillbirths that matched to HDSS
# can only keep clear-cut cases where the mother-level strata and child-level clearly match
# because if mother had multiple events, will not always be clear which one the mother-level strata applies to.
# Note we only have CODs for live births for 0-9y. none for stillbirth, none for 10+.
# we also didn't sample by cause strata for 5-9. so no need to include.
cod_key <- overall %>%
  filter(type == "VS_Match" & cstatus == "Died" & mstrata_ac != "2024all") %>%
  filter(mstrata_a == "Neonatal" & cstrata_a == "Neonatal" |
           mstrata_a == "Postneonatal" & cstrata_a == "Postneonatal" | 
            mstrata_a == "1-4" & cstrata_a == "1-4") %>%
  group_by(rid_m) %>% # rid_m is mother id, c215_a is her child number
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1 & !is.na(cod_c_dss)) %>%
  select(mstrata_ac, cstrata_a, cod_c_dss) %>%
  distinct()

# check for errors in key
# any cases where the icd code was assigned to two different maternal age/cause strata for the same age group
cod_key %>%
  group_by(cod_c_dss, cstrata_a) %>%
  mutate(n_mstrata = n_distinct(mstrata_ac)) %>%
  ungroup() %>%
  filter(n_mstrata > 1) %>%
  arrange(cod_c_dss)
# manually correct these in key
# 1G40 is sepsis
# ka21 is prematurity/lbw
# kb21 is birth asphyxia
# pa91 is drowning
cod_key <- cod_key %>%
  filter(!(cstrata_a == "Postneonatal" & cod_c_dss == "1G40" & mstrata_ac != "Postneonatal (other)")) %>%
  filter(!(cstrata_a == "Neonatal" & cod_c_dss == "KA21" & mstrata_ac != "Neonatal (other)")) %>%
  filter(!(cstrata_a == "Neonatal" & cod_c_dss == "KB21" & mstrata_ac != "Neonatal (birth asphyxia)")) %>%
  filter(!(cstrata_a == "1-4" & cod_c_dss == "PA91" & mstrata_ac != "1-4 year (drowning)")) %>%
  rename(cstrata_ac = mstrata_ac)

# Should these cases be corrected in the overall file as well?
# If so, we are saying that some mother-level strata were incorrectly assigned.

# Before correcting...
# Make sure it is not an issue of mother-level versus child-level COD and that i've misspecified which child the strata refers to.
# Also make sure that they dont have many more children in the HDSS file. 
# (so maybe it is referring to one of those children that wasn't matched with VS).

# Case 1: this mother only has one child in overall and HDSS. So the strata has been incorrectly assigned.
overall %>%
  filter(mstrata_ac == "Postneonatal (RI+con)" & cod_c_dss == "1G40") %>%
  nrow()
overall %>% filter(rid_m == "4V65013510") %>% nrow() # 1
hdss %>% filter(rid_m == "4V65013510") %>% nrow() # 1

# Case 2: there are two mothers
overall %>%
  filter(mstrata_ac == "Neonatal (birth asphyxia)" & cod_c_dss == "KA21") %>%
  nrow()
# mother 1 has twins that both died of KA21 which was mislabelled as birth_aphyxia
overall %>% filter(rid_m == "5V22002806") # 2
hdss %>% filter(rid_m == "5V22002806") # 2
# mother 2 has two children. only one died. so there is no ambiguity in who KA21 applies to.
overall %>% filter(rid_m == "5V72085804") # 2
hdss %>% filter(rid_m == "5V72085804") # 2

# Case 3: there are three mothers
overall %>%
  filter(mstrata_ac == "Neonatal (other)" & cod_c_dss == "KB21") %>%
  nrow()
# mother 1 has two children. only one died. so there is no ambiguity in who KB21 applies to.
overall %>% filter(rid_m == "5V18064006") # 2
hdss %>% filter(rid_m == "5V18064006") %>% nrow # 2
# mother 2 has four children. only one died. so there is no ambiguity in who KB21 applies to.
overall %>% filter(rid_m == "4V26033506") # 4
hdss %>% filter(rid_m == "4V26033506") # 4
# mother 3 has three children. two died and they are twins. one was assigned KB21 and one was assigned KB23
# KB21 is birth asphyxia and KB23 means respiratory distress. So in this case, both should probably be Neonatal (birth asphyxia)
overall %>% filter(rid_m == "5VB0045606")
hdss %>% filter(rid_m == "5VB0045606")

# case 4: there are two mothers
overall %>%
  filter(mstrata_ac == "1-4 year (other)" & cod_c_dss == "PA91") %>%
  nrow()
# mother 1 has two children. only one died. so there is no ambiguity in who PA91 applies to.
overall %>% filter(rid_m == "5DX0053610") # 2
hdss %>% filter(rid_m == "5DX0053610") %>% nrow # 2
# mother 2 has 4 children in VS. only two matched with HDSS. one died. another is a stillbirth.
# so no ambiguity about who PA91 applies to.
overall %>% filter(rid_m == "2V17004007") # 4
hdss %>% filter(rid_m == "2V17004007") %>% nrow # 2

# Conclusion: yes, correct the mother-level strata in the overall file as well.
overall <- overall %>%
  mutate(
    mstrata_ac = case_when(
      mstrata_ac == "Postneonatal (RI+con)" & cod_c_dss == "1G40" ~ "Postneonatal (other)",
      mstrata_ac == "Neonatal (birth asphyxia)" & cod_c_dss == "KA21" ~ "Neonatal (other)",
      mstrata_ac == "Neonatal (other)" & cod_c_dss == "KB21" ~ "Neonatal (birth asphyxia)",
      mstrata_ac == "1-4 year (other)" & cod_c_dss == "PA91" ~ "1-4 year (drowning)",
      TRUE ~ mstrata_ac
    )
  )


# Add child-level categorized COD to overall ------------------------------

# merge on cod_key
# fill in NAs for surviving, stillbirth, 5-9 year, 10+
overall <- overall %>%
  left_join(cod_key, by = c("cstrata_a", "cod_c_dss")) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & cstrata_a == "Surviving" ~ "Surviving",
    is.na(cstrata_ac) & cstrata_a == "Stillbirth" ~ "Stillbirth",
    is.na(cstrata_ac) & cstrata_a == "5-9" ~ "5-9 year",
    is.na(cstrata_ac) & cstrata_a == "10+" ~ "10+",
    TRUE ~ cstrata_ac
  )) 

# there are still some NAs
overall %>%
  select(rid_m, mstrata_ac, cstatus, cstrata_a, cstrata_ac, cod_c_dss) %>%
  filter(is.na(cstrata_ac)) %>% nrow # 195

# if the HDSS COD is non-missing, categorize cod group based on cod_key
v_n_ba <- cod_key %>% filter(cstrata_ac == "Neonatal (birth asphyxia)") %>% pull(cod_c_dss)
v_pn_ric <- cod_key %>% filter(cstrata_ac == "Postneonatal (RI+con)") %>% pull(cod_c_dss)
v_c_d <- cod_key %>% filter(cstrata_ac == "1-4 year (drowning)") %>% pull(cod_c_dss)
overall <- overall %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Neonatal" & cod_c_dss %in% v_n_ba ~ "Neonatal (birth asphyxia)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Neonatal" & !is.na(cod_c_dss) ~ "Neonatal (other)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Postneonatal" & cod_c_dss %in% v_pn_ric ~ "Postneonatal (RI+con)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Postneonatal" & !is.na(cod_c_dss) ~ "Postneonatal (other)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "1-4" & cod_c_dss %in% v_c_d ~ "1-4 year (drowning)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "1-4" & !is.na(cod_c_dss) ~ "1-4 year (other)",
    TRUE ~ cstrata_ac
  ))

# there are still some missing
overall %>%
  select(rid_m, mstrata_ac, cstatus, cstrata_a, cstrata_ac, cod_c_dss) %>%
  filter(is.na(cstrata_ac)) %>% nrow # 169
overall %>%
  select(rid_m, mstrata_ac, cstatus, cstrata_a, cstrata_ac, cod_c_dss) %>%
  filter(is.na(cstrata_ac) & cstatus == "Died") %>% nrow # 169

# however only one of the ones missing was matched to the HDSS
# the others are all "additions" to the validation study for which we have no way of knowing the COD
overall %>%
  filter(is.na(cstrata_ac)) %>% 
  group_by(type) %>%
  summarise(n = n())

table(overall$type, useNA = "always")

# Add non-matching rows from the HDSS -------------------------------------

# From the HDSS
# live births, stillbirths, miscarriage, abortions that were not matched to VS (HDSS - not matched)
hdss_nomatch <- subset(hdss, !(uid_c_dss %in% overall$uid_c_dss))
nrow(hdss_nomatch) # 607
hdss_nomatch$type <- "HDSS_NoMatch"
hdss_nomatch$type[hdss_nomatch$preg_res_dss == "Miscarriage"] <- "HDSS_MSC"
hdss_nomatch$type[hdss_nomatch$preg_res_dss == "Abortion"] <- "HDSS_AB"
table(hdss_nomatch$type, useNA = "always")

# merge on cod_key
# fill in NAs for surviving, stillbirth, 5-9 year, 10+

# use key to add mstrata_ac to hdss
hdss_nomatch <- hdss_nomatch %>%
  #select(uid_c_dss, cstatus, cstrata_a, cod_c_dss) %>%
  left_join(cod_key, by = c("cstrata_a", "cod_c_dss")) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & cstrata_a == "Surviving" ~ "Surviving",
    is.na(cstrata_ac) & cstrata_a == "Stillbirth" ~ "Stillbirth",
    is.na(cstrata_ac) & cstrata_a == "Abortion" ~ "Miscarriage",
    is.na(cstrata_ac) & cstrata_a == "Miscarriage" ~ "Miscarriage",
    is.na(cstrata_ac) & cstrata_a == "5-9" ~ "5-9 year",
    is.na(cstrata_ac) & cstrata_a == "10+" ~ "10+",
    TRUE ~ cstrata_ac
  ))

# there are still some NAs
hdss_nomatch %>%
  select(rid_m, cstatus, cstrata_a, cstrata_ac, cod_c_dss) %>%
  filter(is.na(cstrata_ac)) %>% nrow() # 23

# if the HDSS COD is non-missing, categorize cod group based on cod_key
hdss_nomatch <- hdss_nomatch %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Neonatal" & cod_c_dss %in% v_n_ba ~ "Neonatal (birth asphyxia)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Neonatal" & !is.na(cod_c_dss) ~ "Neonatal (other)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Postneonatal" & cod_c_dss %in% v_pn_ric ~ "Postneonatal (RI+con)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "Postneonatal" & !is.na(cod_c_dss) ~ "Postneonatal (other)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "1-4" & cod_c_dss %in% v_c_d ~ "1-4 year (drowning)",
    is.na(cstrata_ac) & !is.na(cod_c_dss) & cstrata_a == "1-4" & !is.na(cod_c_dss) ~ "1-4 year (other)",
    TRUE ~ cstrata_ac
  ))
# no more NAs in child-level COD for non-matching HDSS
hdss_nomatch %>%
  filter(is.na(cstrata_ac)) %>% nrow() # 0

# Combine overall and hdss_nomatch         
overall_aug1 <- bind_rows(overall, hdss_nomatch)

# Check if there are still NAs in VS observations that were matched to HDSS
overall_aug1 %>%
  filter(type %in% c("VS_Match")) %>% 
  select(type, rid_m, mstrata_ac, cstatus, cstrata_a, cstrata_ac, cod_c_dss) %>%
  filter(is.na(cstrata_ac)) %>% nrow() # 1
# yes, one
# get mothers ID to subset all her records (matched and unmatched)
v_mothers_id <- overall_aug1 %>%
  filter(type %in% c("VS_Match")) %>% 
  filter(is.na(cstrata_ac)) %>% pull(rid_m)

# use mother-level age/cause strata to fill in where possible
# must be unambiguous that mother-level COD applies to the child 
# (can't have other records in VS or HDSS that it would apply to)
nrow(overall_aug1) # 2994
overall_aug1$recnr <- 1:nrow(overall_aug1)
cod_fill <- overall_aug1 %>%
  filter(rid_m %in% v_mothers_id & is.na(cstrata_ac) & cstatus == "Died") %>%
  group_by(rid_m, cstrata_a) %>%
  mutate(n = n()) %>%
  #select(type, rid_m, mstrata_ac, cstatus, cstrata_a, cstrata_ac, cod_c_dss, n) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & n == 1 & mstrata_ac %in% c("Neonatal (other)", "Neonatal (birth asphyxia") & cstrata_a == "Neonatal"~
      mstrata_ac,
    is.na(cstrata_ac) & n == 1 & mstrata_ac %in% c("Postneonatal (other)", "Postneonatal (RI+con)") & cstrata_a == "Postneonatal"~
      mstrata_ac,
    is.na(cstrata_ac) & n == 1 & mstrata_ac %in% c("1-4 year (other)", "1-4 year (drowning)") & cstrata_a == "1-4"~
      mstrata_ac,
    TRUE ~ cstrata_ac
  )) %>%
  select(-n)
overall_aug1 <- rbind(subset(overall_aug1, !(recnr %in% cod_fill$recnr)), cod_fill)
nrow(overall_aug1) # 2994

# so we still have 169 with missing CODs
nrow(subset(overall_aug1, is.na(cstrata_ac))) # 169

# these are a combination of 168 validation study additions (children who died and weren't matched to hdss)
# as well as 1 validation study death that was matched to the hdss, but we can't be sure of the cause because we don't have a COD in the hdss data, and it is ambiguous which death the mother-level strata applies to (she has two neonatal deaths that could fit the description, and can't be sure which should be matched to which cause)
overall_aug1 %>%
  filter(is.na(cstrata_ac)) %>% 
  group_by(type) %>%
  summarise(n = n())

table(overall_aug1$type, useNA = "always")

# Add non-matching rows from VS -------------------------------------------

# From the validation study
# miscarriages for which no matching with hdss was attempted (VS - MSC)
# abortions for which no matching with hdss was attempted (VS - AB)
survey_nomatch <- subset(survey, !(serial %in% overall$serial))
nrow(survey_nomatch) # 266
survey_nomatch$type <- NA
survey_nomatch$type[survey_nomatch$preg_res_surv == "Miscarriage"] <- "VS_MSC"
survey_nomatch$type[survey_nomatch$preg_res_surv == "Abortion"] <- "VS_AB"
table(survey_nomatch$type, useNA = "always")

survey_nomatch$cstrata_ac <- NA
survey_nomatch$cstrata_ac[survey_nomatch$preg_res_surv == "Abortion"] <- "Abortion"
survey_nomatch$cstrata_ac[survey_nomatch$preg_res_surv == "Miscarriage"] <- "Miscarriage"
nrow(subset(survey_nomatch, is.na(cstrata_ac))) # 0

# Combine overall and survey_nomatch
overall_aug2 <- bind_rows(overall_aug1, survey_nomatch)

# Calculate dod_c_sur
overall_aug2$dod_c_sur <- ifelse(overall_aug2$serial %in% survey$serial, overall_aug2$dob_c_sur + overall_aug2$aadd, NA)

table(overall_aug2$type, useNA = "always")

# Fill in cstrata_ac ------------------------------------------------------

overall_aug2 %>%
  filter(is.na(cstrata_ac)) %>% 
  group_by(type) %>%
  summarise(n = n())

# unknown either because is an addition from the validation study
# or because it was matched between vs and hdss, but hdss is missing cod
overall_aug2$cstrata_ac[is.na(overall_aug2$cstrata_ac)] <- "Unknown"

# Maternal age at interview -----------------------------------------------

# note that if an hdss pregnancy outcome/child was not matched to the VS, there will not be an int_date
# need to fill in int_date for all child records for each mother
# first check that there is only ever one unique non-missing int_date per mother
overall_aug2 %>%
  group_by(rid_m) %>%
  mutate(int_date_fill = max(int_date, na.rm = TRUE)) %>%
  filter(!is.na(int_date) & int_date != int_date_fill) %>% nrow() # 0
# also check that after filling, there are no missing int_dates
overall_aug2 %>%
  group_by(rid_m) %>%
  mutate(int_date_fill = max(int_date, na.rm = TRUE)) %>%
  filter(is.na(int_date_fill)) %>% nrow() # 0

# ok, proceed
overall_aug2 <- overall_aug2 %>%
  group_by(rid_m) %>%
  mutate(int_date_fill = max(int_date, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(matage = as.numeric(int_date_fill - dob_m_dss)/365.25,
         matage_cat = cut(matage,
                        breaks = seq(15,55, 5), 
                        labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-55")))
nrow(subset(overall_aug2, is.na(matage))) # 0
nrow(subset(overall_aug2, is.na(matage_cat))) # 0

# Pregnancy outcome date --------------------------------------------------

# add one date of pregnancy outcome
overall_aug2 %>%
  as.data.frame() %>% head()
# dob_c_dss
# dob_c_sur
overall_aug2 <- overall_aug2 %>%
  mutate(dob_c_comb = ifelse(!is.na(dob_c_dss), dob_c_dss, dob_c_sur))
nrow(subset(overall_aug2, is.na(dob_c_comb))) # 0


# Age at death ------------------------------------------------------------

# add age at death from hdss
# derived from dob and dod

# age at death in survey (aadd)
# age at death in HDSS (based off dob_c_dss, dod_c_dss)
overall_aug2 <- overall_aug2 %>%
  mutate(aadd_hdss = as.numeric(dod_c_dss - dob_c_dss),
         aadm_hdss = as.numeric(dod_c_dss - dob_c_dss)/30.5,
         aady_hdss = as.numeric(dod_c_dss - dob_c_dss)/365.25) 
 # select(type, cstatus, aadd, aadm, aady, aadd_hdss, aadm_hdss, aady_hdss,
 #         dob_c_dss, dod_c_dss) %>%
 # filter(cstatus %in% c("Died", "Stillbirth")) %>% View
         

# Check id vars -----------------------------------------------------------

nrow(overall_aug2) # 3260

# mother_id
length(unique(overall_aug2$rid_m)) # 848
nrow(subset(overall_aug2, is.na(rid_m))) # 0

# id variables that are somewhat incomplete
length(unique(overall_aug2$rid_c)) # 2014
length(unique(overall_aug2$serial1)) # 848
length(unique(overall_aug2$Userial1)) # 821
length(unique(overall_aug2$serial)) # 2649
length(unique(overall_aug2$uid_c)) # 2649
length(unique(overall_aug2$uid_c_dss)) # 2506

# arrange
# add ordered recnr
nrow(subset(overall_aug2, is.na(dob_c_dss))) # 751
nrow(subset(overall_aug2, is.na(dob_c_sur))) # 607
overall_aug2 <- overall_aug2 %>%
  arrange(rid_m, dob_c_comb) %>%
  mutate(recnr = 1:n()) %>%
  relocate(recnr)

# Save output(s) ----------------------------------------------------------

saveRDS(overall_aug2, "./gen/overall-date-aug.rds")


