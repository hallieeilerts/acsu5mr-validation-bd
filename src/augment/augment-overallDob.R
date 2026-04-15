################################################################################
#' @description Overall contains survey records that matched to HDSS and survey records that did not.
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
overall <- readRDS("./gen/clean/overallDob-clean.rds")
hdss <- readRDS("./gen/clean/hdss-clean.rds")
survey <- readRDS("./gen/clean/survey-clean.rds")
################################################################################

# unique identifier
df_uniq_vals <- as.data.frame(apply(overall, 2, function(x) length(unique(x))))
df_uniq_vals$variable <- row.names(df_uniq_vals)
row.names(df_uniq_vals) <- NULL
names(df_uniq_vals) <- c("n", "variable")
subset(df_uniq_vals, n == nrow(overall))
# there is currently no variable that serves as a unique identifier

# do all observations have a mother_id?
nrow(overall) # 2409
length(unique(overall$rid_m)) # 841
nrow(subset(overall, is.na(rid_m))) # 0
# yes. make sure they do at the end of this script once file has been augmented.

# Those with a missing match score are validation study observations that weren't matched to HDSS.
# note that this file also contains HDSS live births that weren't matched
# in overallDate these were not included and had to be added.
table(overall$match_score, useNA = "always")
overall$type <- NA
overall$type[!is.na(overall$match_score)] <- "VS_Match"
overall$type[is.na(overall$match_score)] <- "VS_NoMatch"
overall$type[is.na(overall$match_score) & is.na(overall$uid_c_sur)] <- "HDSS_NoMatch"
table(overall$match_score, useNA = "always") # 1968 1, 441 NA
table(overall$type, useNA = "always") # 186 hdss no match, 1968 vs match, 255 vs no match, 0 NA

# for every matched case, make sure there is child status information from dss
nrow(subset(overall, type == "VS_Match" & is.na(cstatus_dss)))   # 0
nrow(subset(overall, type == "VS_Match" & is.na(cstatus_agesp_dss)))  # 0
# in one case, we do not have cod information
nrow(subset(overall, type == "VS_Match" & is.na(cod_c_dss) & cstatus_dss == "Died")) # 1

# were multiple different survey records matched to the same dss record?
overall %>%
  group_by(uid_c_dss) %>%
  mutate(n = n()) %>%
  filter(!is.na(uid_c_dss) & n > 1) %>%
  nrow() # 0
# were multiple different dss matched to the same survey record?
overall %>%
  group_by(uid_c_sur) %>%
  mutate(n = n()) %>%
  filter(!is.na(uid_c_sur) & n > 1) %>%
  nrow() # 0

# Add from DSS ------------------------------------------------------------

# in overallDate, the parity variable from dss was missing in overall file
# not the case for overallDob. so don't need to add
# in overallDate, parity_dss from the hdss file is merged on, but still missing for unmatched survey observations (485 in that file)
# here it's actually missing for even fewer than that.
nrow(subset(overall, type == "VS_Match" & is.na(parity_dss)))   # 0
nrow(subset(overall, type == "VS_NoMatch" & is.na(parity_dss))) # 254

# Categorize icd codes ------------------------------------------------------

# Overall has categorized causes of death for mother-level strata
unique(overall$mstrata_ac)
unique(overall$mstrata_c)
# at the child-level, it has icd codes
# it would be best if these are categorized

# to categorize, create a key from survey lb/stb that matched to HDSS
# can only keep clear-cut cases where the mother-level strata and child-level clearly match
# because if mother had multiple events, will not always be clear which one the mother-level strata applies to.
# Note we only have CODs for live births for 0-9y. none for stillbirth, none for 10+.
# we also didn't sample by cause strata for 5-9. so no need to include.
cod_key <- overall %>%
  filter(type == "VS_Match" & cstatus_dss == "Died" & mstrata_ac != "2024all") %>%
  filter(mstrata_a == "Neonatal" & cstatus_agesp_dss == "Neonatal" |
           mstrata_a == "Postneonatal" & cstatus_agesp_dss == "Postneonatal" | 
           mstrata_a == "1-4" & cstatus_agesp_dss == "1-4") %>%
  group_by(rid_m) %>% # rid_m is mother id, c215_a is her child number
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1 & !is.na(cod_c_dss)) %>%
  select(mstrata_ac, cstatus_agesp_dss, cod_c_dss) %>%
  distinct()
# remove the ill-defined causes from the cod_key
cod_key <- cod_key %>% 
  filter(!(cod_c_dss %in% c("R99", "MH14")))

# check for errors in key
# any cases where the icd code was assigned to two different maternal age/cause strata for the same age group
cod_key %>%
  group_by(cod_c_dss, cstatus_agesp_dss) %>%
  mutate(n_mstrata = n_distinct(mstrata_ac)) %>%
  ungroup() %>%
  filter(n_mstrata > 1) %>%
  arrange(cod_c_dss)
# manually correct these in key
# 1G40 is sepsis -> should be postneonatal (other) for mstrata_ac
# ka21 is prematurity/lbw -> should be neonatal (other) for mstrata_ac
# kb21 is birth asphyxia in icd11 -> should be neonatal (birth asphyxia) for mstrata_ac
# pa91 is drowning -> should be 1-4 year (drownning) for m_strata_ac
cod_key <- cod_key %>%
  filter(!(cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "1G40" & mstrata_ac != "Postneonatal (other)")) %>%
  filter(!(cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KA21" & mstrata_ac != "Neonatal (other)")) %>%
  filter(!(cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KB21" & mstrata_ac != "Neonatal (birth asphyxia)")) %>%
  filter(!(cstatus_agesp_dss == "1-4" & cod_c_dss == "PA91" & mstrata_ac != "1-4 year (drowning)")) %>%
  rename(cstrata_ac = mstrata_ac)

# Should these cases be corrected in the overall file as well?
# If so, we are saying that some mother-level strata were incorrectly assigned.

# Before correcting...
# Make sure it is not an issue of mother-level versus child-level COD and that i've mispecified which child the strata refers to.
# Also make sure that they dont have many more children in the HDSS file. 
# (so maybe it is referring to one of those children that wasn't matched with VS).
# investigate each cause with incorrect cod category from above

# Case 1
overall %>%
  filter(mstrata_ac == "Postneonatal (RI+con)" & cod_c_dss == "1G40") %>%
  nrow() # 1
overall %>% filter(rid_m == "4V65013510") %>% nrow() # 1
hdss %>% filter(rid_m == "4V65013510") %>% nrow() # 1
# this mother only has one child in overall and HDSS. So the strata has been incorrectly assigned.

# Case 2
overall %>%
  filter(mstrata_ac == "Neonatal (birth asphyxia)" & cod_c_dss == "KA21") %>%
  nrow() # 3
overall %>%
  filter(mstrata_ac == "Neonatal (birth asphyxia)" & cod_c_dss == "KA21") %>%
  select(rid_m) %>% pull() %>% unique() # "5V22002806" "
# there are two mothers, one with twins
# mother 1 has twins 
# both died of KA21 which was mislabelled as birth_aphyxia
overall %>% filter(rid_m == "5V22002806") %>%
  select(cid_m, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "5V22002806") %>%
  select(rid_m, rid_c, pregout_dss, cstatus_dss, cod_c_dss)

# Case 3
overall %>%
  filter(mstrata_ac == "Neonatal (other)" & cod_c_dss == "KB21") %>%
  nrow() # 5
overall %>%
  filter(mstrata_ac == "Neonatal (other)" & cod_c_dss == "KB21") %>%
  select(rid_m) %>% pull() %>% unique() # "4V26033506" "5V18064006" "5VB0045606" "5V18064006" "3D34013908"
# there are three mothers
# mother 1 has two children. only one died. so there is no ambiguity in who KB21 applies to.
overall %>% filter(rid_m == "5V18064006") %>%
  select(cid_m, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "5V18064006") %>%
  select(rid_m, rid_c, pregout_dss, cstatus_dss, cod_c_dss)
# mother 2 has four children. only one died. so there is no ambiguity in who KB21 applies to.
overall %>% filter(rid_m == "4V26033506") %>%
  select(cid_m, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "4V26033506") %>%
  select(rid_m, rid_c, pregout_dss, cstatus_dss, cod_c_dss)
# mother 3 has three children. two died and they are twins. one was assigned KB21 and one was assigned KB23
# KB21 is birth asphyxia and KB23 means respiratory distress of newborn
# KB23 is shortly after birth
# in this case it's ok if the mother is Neonatal (other), as it could be referring to the KB23 child
overall %>% filter(rid_m == "5VB0045606") %>%
  select(cid_m, c215, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "5VB0045606") %>%
  select(rid_m, rid_c, dob_c_dss, pregout_dss, cstatus_dss, cod_c_dss)
# mother 4 has two children. one died, one surviving
overall %>% filter(rid_m == "5V18064006") %>%
  select(cid_m, c215, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "5V18064006") %>%
  select(rid_m, rid_c, dob_c_dss, pregout_dss, cstatus_dss, cod_c_dss)
# mother 5 has 4 children, 2 that died
# one was assigned P22 and one was assigned KB21
# KB21 is birth asphyxia in icd11 and P22 means respiratory distress of newborn in icd10
# in this case it's ok if the mother is Neonatal (other), as it could be referring to the P22 child
overall %>% filter(rid_m == "3D34013908") %>%
  select(cid_m, c215, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "3D34013908") %>%
  select(rid_m, rid_c, dob_c_dss, pregout_dss, cstatus_dss, cod_c_dss)
# dont recode mstrata for "5VB0045606", "3D34013908"

# case 4
overall %>%
  filter(mstrata_ac == "1-4 year (other)" & cod_c_dss == "PA91") %>%
  nrow() # 2
overall %>%
  filter(mstrata_ac == "1-4 year (other)" & cod_c_dss == "PA91") %>%
  select(rid_m) %>% pull() %>% unique() # "2V17004007" "5DX0053610"
# there are two mothers
# mother 1 has two children. only one died. so there is no ambiguity in who PA91 applies to.
overall %>% filter(rid_m == "5DX0053610")  %>%
  select(cid_m, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "5DX0053610") %>%
  select(rid_m, rid_c, pregout_dss, cstatus_dss, cod_c_dss)
# mother 2 has 4 children in VS. only two matched with HDSS. one died. another is surviving.
# so no ambiguity about who PA91 applies to.
overall %>% filter(rid_m == "2V17004007") %>%
  select(cid_m, pregout_dss, cstatus_dss, cod_c_dss) 
hdss %>% filter(rid_m == "2V17004007") %>%
  select(rid_m, rid_c, pregout_dss, cstatus_dss, cod_c_dss)

v_exceptions <- c("5VB0045606", "3D34013908")
# Conclusion: yes, correct the mother-level strata in the overall file as well, except for vector with exceptions
overall <- overall %>%
  mutate(
    mstrata_ac = case_when(
      !(rid_m %in% v_exceptions) & mstrata_ac == "Postneonatal (RI+con)" & cod_c_dss == "1G40" ~ "Postneonatal (other)",
      !(rid_m %in% v_exceptions) & mstrata_ac == "Neonatal (birth asphyxia)" & cod_c_dss == "KA21" ~ "Neonatal (other)",
      !(rid_m %in% v_exceptions) & mstrata_ac == "Neonatal (other)" & cod_c_dss == "KB21" ~ "Neonatal (birth asphyxia)",
      !(rid_m %in% v_exceptions) & mstrata_ac == "1-4 year (other)" & cod_c_dss == "PA91" ~ "1-4 year (drowning)",
      TRUE ~ mstrata_ac
    )
  )

# Add child-level categorized cod -----------------------------------------

# merge on cod_key
# fill in NAs for surviving, stillbirth, 5-9 year, 10+
overall <- overall %>%
  left_join(cod_key, by = c("cstatus_agesp_dss", "cod_c_dss")) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & cstatus_agesp_dss == "Surviving" ~ "Surviving",
    is.na(cstrata_ac) & cstatus_agesp_dss == "Stillbirth" ~ "Stillbirth",
    is.na(cstrata_ac) & cstatus_agesp_dss == "5-9" ~ "5-9 year",
    is.na(cstrata_ac) & cstatus_agesp_dss == "10+" ~ "10+",
    TRUE ~ cstrata_ac
  )) 

# there are still some NAs
overall %>%
  select(rid_m, mstrata_ac, cstrata_ac, cod_c_dss) %>%
  filter(is.na(cstrata_ac)) %>% nrow # 312
# sometimes these are cases where the validation study event did not match to the hdss, so we dont have cod information
overall %>%
  select(type, rid_m, mstrata_ac, cstrata_ac, cod_c_dss) %>%
  filter(type == "VS_NoMatch" & is.na(cstrata_ac)) %>% nrow # 254
# when it did match, there are 45 cases for which we still don't have the cod categorized
# we have the cod from the hdss, but the categorized cod was not in the key 
# (because the mother had multiple events and mother level strata couldn't be assigned to child)
# (or because the cause was ill-defined and dropped from cod_key)
overall %>%
  select(type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
  filter(type == "VS_Match" & is.na(cstrata_ac)) %>%
  nrow() # 45

# manual review of codes (google code for icd 10) and age at death
# overall %>%
#   select(type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
#   filter(type == "VS_Match" & is.na(cstrata_ac)) %>%
#   View()
# manually assign child-level strata
unique(overall$cstrata_ac)
overall <- overall %>%
  mutate(flag = ifelse(type == "VS_Match" & is.na(cstrata_ac), 1, 0)) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "460" ~ "1-4 year (other)", # common cold
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "P11" ~  "Neonatal (other)", # birth injuries
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "X10" ~ "1-4 year (other)", # burns
    is.na(cstrata_ac) & cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "R50" ~  "Postneonatal (other)", # fever
    is.na(cstrata_ac) & cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "8A63" ~  "Postneonatal (other)", # seizure
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "P55" ~  "Neonatal (other)", # hemolytic disease
    is.na(cstrata_ac) & cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "Q89" ~  "Postneonatal (RI+con)", # congenital
    is.na(cstrata_ac) & cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "G12" ~  "Postneonatal (other)", # congenital
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "G40" ~  "1-4 year (other)", # seizure
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "321" ~  "Neonatal (other)", # icd9, meningitis
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KB86" ~  "Neonatal (other)", # pancreas
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "452" ~  "Neonatal (other)", # blood clot
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KA61" ~  "Neonatal (other)", # anal/rectal
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KB40" ~  "Neonatal (other)", # hernia
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "Q24" ~  "Neonatal (other)", # malformation of heart
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KB26" ~  "Neonatal (other)", # aspiration of meconium
    is.na(cstrata_ac) & cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "LD2Z" ~  "Postneonatal (other)", # developmental anomaly
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "P00" ~  "Neonatal (other)", # maternal conditions
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KB24" ~  "Neonatal (other)", # congenital pneumonia
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "E46" ~  "1-4 year (other)", # protein calorie malnutrition
    is.na(cstrata_ac) & cstatus_agesp_dss == "Postneonatal" & cod_c_dss == "E46" ~  "Postneonatal (other)", # protein calorie malnutrition
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "G03" ~  "1-4 year (other)", # meningitis
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "CA40" ~  "Neonatal (other)", # neoplasm
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "C26" ~  "1-4 year (other)", # neoplasm
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "1A41" ~  "Neonatal (other)", # sepsis
    is.na(cstrata_ac) & cstatus_agesp_dss == "Neonatal" & cod_c_dss == "KA84" ~  "Neonatal (other)", # hemolytic disease
    is.na(cstrata_ac) & cstatus_agesp_dss == "1-4" & cod_c_dss == "2C80" ~  "1-4 year (other)", # neoplasm
    TRUE ~ cstrata_ac
  ))  #%>%
# filter(flag == 1) %>%
# select(type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
# View()

# now check number of matched cases that still dont have cod
overall %>%
  select(type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
  filter(type == "VS_Match" & is.na(cstrata_ac)) %>%
  nrow() # 18
overall %>%
  select(type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
  filter(type == "VS_Match" & is.na(cstrata_ac))
overall %>%
  select(type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
  filter(type == "VS_Match" & is.na(cstrata_ac) & !(cod_c_dss %in% c("R99", "MH14"))) %>% nrow() # 0
# all ill-defined cods (R99, MH14)
overall %>%
  filter(is.na(cstrata_ac)) %>% #filter(type == "HDSS_NoMatch") %>%View
  group_by(type) %>%
  summarise(n = n()) # 18 among matched cases, 254 among non-matches vs, 10 among non-matched hdss
overall <- overall %>%
  select(-flag)

# Add non-matching rows from the HDSS -------------------------------------

# in overallDOB, this is only events for which matching was not attempted
# ie, stillbirths, miscarriage, abortions 
# in overallDate, this included non-matched live births as well

# From the HDSS
# stillbirths, miscarriage, abortions that were not matched to VS (HDSS - not matched)
hdss_nomatch <- subset(hdss, !(uid_c_dss %in% overall$uid_c_dss))
nrow(hdss_nomatch) # 351
unique(hdss_nomatch$pregout_dss) # Miscarriage, Abortion, Stillbirth 

# add mother-level strata to these hdss records from the survey
hdss_nomatch <- hdss_nomatch %>%
  left_join(survey %>% select(rid_m, mstrata_ac) %>% distinct())
# mstrata_ac is not missing for anyone 
nrow(subset(hdss_nomatch, is.na(mstrata_ac))) # 0

# add type variable
hdss_nomatch$type[hdss_nomatch$pregout_dss == "Miscarriage"] <- "HDSS_MSC"
hdss_nomatch$type[hdss_nomatch$pregout_dss == "Abortion"] <- "HDSS_AB"
hdss_nomatch$type[hdss_nomatch$pregout_dss == "Stillbirth"] <- "HDSS_STB"
table(hdss_nomatch$type, useNA = "always") # no NAs

# merge on cod_key to add child-level strata
hdss_nomatch <- hdss_nomatch %>% 
  left_join(cod_key, by = c("cstatus_agesp_dss", "cod_c_dss"))

# fill in strata for miscarriage, abortion, stb, surviving, 5-9 year, 10+
# can't do for neo, pneo, 1-4, because these have cause strata; so need to take that into account
hdss_nomatch <- hdss_nomatch %>% 
  #select(uid_c_dss, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
  #filter(is.na(cstrata_ac)) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & cstatus_agesp_dss == "Abortion" ~ "Abortion",
    is.na(cstrata_ac) & cstatus_agesp_dss == "Miscarriage" ~ "Miscarriage",
    is.na(cstrata_ac) & cstatus_agesp_dss == "Surviving" ~ "Surviving",
    is.na(cstrata_ac) & cstatus_agesp_dss == "Stillbirth" ~ "Stillbirth",
    is.na(cstrata_ac) & cstatus_agesp_dss == "5-9" ~ "5-9 year",
    is.na(cstrata_ac) & cstatus_agesp_dss == "10+" ~ "10+",
    TRUE ~ cstrata_ac
  ))

# there are no NAs
hdss_nomatch %>%
  select(rid_m, cstatus_dss, cstatus_agesp_dss, cod_c_dss, cstrata_ac) %>%
  filter(is.na(cstrata_ac)) %>% nrow() # 0

# Combine overall and hdss_nomatch         
overall_aug1 <- bind_rows(overall, hdss_nomatch)

# Fill in missing child strata --------------------------------------------

# if there is a missing child-level strata
# and it is a matched event from the vs or unmatched from hdss (so we have hdss cod info)
# is there any way we can derive the child-level strata from the mother-level?
# ie, look at all the women's matched and unmatched events and see if the mstrata could only apply to one
overall_aug1 %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>% 
  filter(!is.na(mstrata_ac) & is.na(cstrata_ac)) %>% nrow() # 28
overall_aug1 %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>% 
  select(type, rid_m, mstrata_ac, cstatus_dss, cod_c_dss, cstrata_ac) %>%
  filter(!is.na(mstrata_ac) & is.na(cstrata_ac))

# get mothers ID to subset all her records (matched and unmatched)
v_mothers_id <- overall_aug1 %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>% 
  filter(!is.na(mstrata_ac) & is.na(cstrata_ac)) %>% pull(rid_m)
# check if the mstrata could only be applied to one event
overall_aug1$recnr <- 1:nrow(overall_aug1)
# subset deaths for mother
# recode cstrata_ac if there is only one death for the given mother
# but not if that death has an ill-defined cause or is missing
cod_fill <- overall_aug1 %>%
  filter(rid_m %in% v_mothers_id & is.na(cstrata_ac) & cstatus_dss == "Died") %>%
  group_by(rid_m, cstatus_dss) %>%
  mutate(n = n()) %>%
  #select(recnr, type, rid_m, mstrata_ac, cstatus_dss, cstatus_agesp_dss, cstrata_ac, cod_c_dss, n) %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & n == 1 & 
      mstrata_ac %in% c("Neonatal (other)", "Neonatal (birth asphyxia") & 
      cstatus_agesp_dss == "Neonatal" & !(cod_c_dss %in% c("R99", "MH14")) & !is.na(cod_c_dss) ~ mstrata_ac,
    is.na(cstrata_ac) & n == 1 & 
      mstrata_ac %in% c("Postneonatal (other)", "Postneonatal (RI+con)") & 
      cstatus_agesp_dss == "Postneonatal" & !(cod_c_dss %in% c("R99", "MH14")) & !is.na(cod_c_dss) ~ mstrata_ac,
    is.na(cstrata_ac) & n == 1 & 
      mstrata_ac %in% c("1-4 year (other)", "1-4 year (drowning)") & 
      cstatus_agesp_dss == "1-4" & !(cod_c_dss %in% c("R99", "MH14")) & !is.na(cod_c_dss) ~ mstrata_ac,
    TRUE ~ cstrata_ac
  )) %>%
  select(-n)
nrow(cod_fill) # 27
nrow(subset(cod_fill, !is.na(cstrata_ac))) # 5
# 5 were able to be filled in
overall_aug1 <- rbind(subset(overall_aug1, !(recnr %in% cod_fill$recnr)), cod_fill)
nrow(overall_aug1) # 2760

# now there are still some missing cstrata_ac
overall_aug1 %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>% 
  filter(!is.na(mstrata_ac) & is.na(cstrata_ac)) %>% nrow() # 23
# there missing ones are for non-matched events from the validation study
# and missing ones for matched events that had NA, R99, or MH14 COD
overall_aug1 %>%
  filter(is.na(cstrata_ac)) %>% 
  group_by(type) %>%
  summarise(n = n()) # 254 VS_NoMatch, 18 VS_Match, 5 HDSS_NoMatch
nrow(subset(overall_aug1, is.na(cstrata_ac))) # 277

# addition from the validation study will have unknown COD always
# missing CODs in dss will be unknown
# if fph, specify as unknown because we just don't have the VA
# if dss, recode these as (other) category for the respective age group
#overall_aug1$cstrata_ac[is.na(overall_aug1$cstrata_ac)] <- "Unknown"
overall_aug1 <- overall_aug1 %>%
  mutate(cstrata_ac = case_when(
    is.na(cstrata_ac) & type == "VS_NoMatch" & cstatus_agesp_sur == "Surviving" ~ "Surviving",
    is.na(cstrata_ac) & type == "VS_NoMatch" & cstatus_agesp_sur == "Neonatal" ~ "Neonatal (unknown)",
    is.na(cstrata_ac) & type == "VS_NoMatch" & cstatus_agesp_sur == "Postneonatal" ~ "Postneonatal (unknown)",
    is.na(cstrata_ac) & type == "VS_NoMatch" & cstatus_agesp_sur == "1-4" ~ "1-4 year (unknown)",
    is.na(cstrata_ac) & type == "VS_NoMatch" & cstatus_agesp_sur == "5-9" ~ "5-9 year",
    is.na(cstrata_ac) & type == "VS_NoMatch" & cstatus_agesp_sur == "10+" ~ "10+",
    is.na(cstrata_ac) & type %in% c("VS_Match", "HDSS_NoMatch") & cstatus_agesp_dss == "Neonatal" ~ "Neonatal (other)",
    is.na(cstrata_ac) & type %in% c("VS_Match", "HDSS_NoMatch") & cstatus_agesp_dss == "Postneonatal" ~ "Postneonatal (other)",
    is.na(cstrata_ac) & type %in% c("VS_Match", "HDSS_NoMatch") & cstatus_agesp_dss == "1-4" ~ "1-4 year (other)",
    is.na(cstrata_ac) & type %in% c("VS_Match", "HDSS_NoMatch") & cstatus_agesp_dss == "5-9" ~ "5-9 year",
    is.na(cstrata_ac) & type %in% c("VS_Match", "HDSS_NoMatch") & cstatus_agesp_dss == "10+" ~ "10+",
    TRUE ~ cstrata_ac
  ))
nrow(subset(overall_aug1, is.na(cstrata_ac))) # 0

# Add non-matching rows from VS -------------------------------------------

# From the validation study
# stillbirths for which no matching with hdss was attempted (VS - STB)
# miscarriages for which no matching with hdss was attempted (VS - MSC)
# abortions for which no matching with hdss was attempted (VS - AB)
survey_nomatch <- subset(survey, !(uid_c_sur %in% overall$uid_c_sur))
nrow(survey_nomatch) # 425
table(survey_nomatch$c223, useNA = "always") # 159 stb, 216 msc, 50 ab, 0 lb, 0 NA
survey_nomatch$type <- NA
survey_nomatch$type[survey_nomatch$c223 == "Born dead"] <- "VS_STB"
survey_nomatch$type[survey_nomatch$c223 == "Miscarriage"] <- "VS_MSC"
survey_nomatch$type[survey_nomatch$c223 == "Abortion"] <- "VS_AB"
table(survey_nomatch$type, useNA = "always")

survey_nomatch$cstrata_ac <- NA
survey_nomatch$cstrata_ac[survey_nomatch$c223 == "Born dead"] <- "Stillbirth"
survey_nomatch$cstrata_ac[survey_nomatch$c223 == "Miscarriage"] <- "Miscarriage"
survey_nomatch$cstrata_ac[survey_nomatch$c223 == "Abortion"] <- "Abortion"
nrow(subset(survey_nomatch, is.na(cstrata_ac))) # 0

# change variable types before rbind
survey_nomatch$parity_sur <- as.character(survey_nomatch$parity_sur)
overall_aug1$parity_sur <- as.character(overall_aug1$parity_sur)

# Combine overall and survey_nomatch
overall_aug2 <- bind_rows(overall_aug1, survey_nomatch)

# Save output(s) ----------------------------------------------------------

saveRDS(overall_aug2, "./gen/augment/overallDob-aug.rds")
