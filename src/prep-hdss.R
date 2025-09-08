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
# hdss_final_all2: livebirth and stillbirth records from the HDSS
dat <- read_dta("./data/ACSU5MR_FILES/hdss_final_all2.dta")
################################################################################

nrow(dat) # 2275

# replace blank spaces with NA
dat <- dat %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .)))

# convert labeled values to factors
dat <- dat %>%
  mutate(across(where(is.labelled), ~as_factor(.)))

# Remind, the sample column here doesn't apply to the birth record
# it is mother-level

# Initial removal of unnecesary columns
dat <- dat %>%
  select(-c(name_c, name_head, name_bari, # name_m, removed at bottom of script
            uid_c)) # remove because is the same as uid_c_dss

# Renaming HDSS columns that want to keep in overall
dat <- dat %>%
  rename(preg_res_dss = RESULT,
         cod_c_dss = CCOD) %>%
  mutate(preg_res_dss = case_when(
    preg_res_dss == "1" ~ "Live birth",
    preg_res_dss == "2" ~ "Stillbirth",
    preg_res_dss == "3" ~ "Miscarriage",
    preg_res_dss == "4" ~ "Abortion",
    TRUE ~ preg_res_dss
  ))
table(dat$preg_res_dss, useNA = "always")

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
unique(dat$dod_c_dss)
unique(dat$doo_m_dss) # ok
unique(dat$dob_m_dss)
unique(dat$dob_c_dss) # ok
dat$dod_c_dss <- as.Date(dat$dod_c_dss, format = "%d-%b-%Y")
dat$dob_m_dss <- as.Date(dat$dob_m_dss, format = "%d-%b-%Y")

# Coalesce va
dat <- dat %>%
  mutate(va = coalesce(va1, va2, va3, va4, va5, va6, va7)) %>%
  select(-c(va1, va2, va3, va4, va5, va6, va7))

# Assign strata at child level
unique(dat$va)
unique(dat$sample) # 2024all is a death in 2024 that hasn't been assigned a va yet
dat %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, sample, va)
# (1a) at least one neonatal death due to the leading cause, 
# (1b) at least one neonatal death due to remaining causes, 
# (2a) at least one post-neonatal death due to the leading cause, 
# (2b) at least one post-neonatal death due to the remaining causes, 
# (3a) at least one child death due to the leading cause, 
# (3b) at least one child death due to other causes, 
# (4) at least one stillbirth, 
# (5) at least one 5-9 death, and 
# (6) at least one live birth with no stillbirth or no under-10 deaths. 
dat$aadd <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)
dat <- dat %>%
  mutate(status_c = case_when(
    preg_res_dss == "Live birth" & !is.na(dod_c_dss) ~ "Died",
    preg_res_dss == "Live birth" & is.na(dod_c_dss) ~ "Surviving",
    preg_res_dss == "Miscarriage" ~ "Miscarriage",
    preg_res_dss == "Abortion" ~ "Abortion",
    TRUE ~ preg_res_dss
  ))
dat <- dat %>%
  mutate(sample_c = case_when(
    status_c == "Abortion" ~ "0_Abortion",
    status_c == "Miscarriage" ~ "0_Miscarriage",
    status_c == "Stillbirth" ~ "4_Stillbirth",
    status_c == "Surviving" & sample != "Strata6" ~ "0_Surviving",
    status_c == "Surviving" & sample == "Strata6" ~ "6",
    status_c == "Died" & aadd < 28 & sample == "Neonatal" ~ paste0("1_" , va),
    status_c == "Died" & aadd < 28 & sample == "2024all" ~ paste0("1_" , "NoVAyet"),
    status_c == "Died" & aadd < 28 & !(sample %in% c("Neonatal", "2024all")) ~ paste0("1_" , "Unk"),
    status_c == "Died" & aadd >= 28 & aadd < 365 & sample == "Postneonatal" ~ paste0("2_" , va),
    status_c == "Died" & aadd >= 28 & aadd < 365 & sample == "2024all" ~ paste0("2_" , "NoVAyet"),
    status_c == "Died" & aadd >= 28 & aadd < 365 & !(sample %in% c("Postneonatal", "2024all")) ~ paste0("2_" , "Unk"),
    status_c == "Died" & aadd >= 365 & aadd < 5*365 & sample == "1-4" ~ paste0("3_" , va),
    status_c == "Died" & aadd >= 365 & aadd < 5*365 & sample == "2024all" ~ paste0("3_" , "NoVAyet"),
    status_c == "Died" & aadd >= 365 & aadd < 5*365 & !(sample %in% c("2024all", "1-4")) ~ paste0("3_" , "Unk"),
    status_c == "Died" & aadd >= 5*365 & aadd < 10*365 ~ "5",
    #status_c == "Died" & aadd >= 5*365 & aadd < 10*365 & sample == "2024all" ~ paste0("5_" , "NoVAyet"), # no need because we don't look at cod for 5-9
    status_c == "Died" & aadd >= 10*365 ~ "0_10+",
    TRUE ~ NA
  )) %>%
  mutate(sample_c1 = substr(sample_c, 1, 1)) 
dat %>%
  filter(rid_m %in% c("2U00091606", "1G00029040", "3N00031810")) %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1) %>%
  head()
# check if more than one died per mother
# then it is not always possible to determine who the va information applies to
# this is true even if they had a neonatal and postneonatal death (rid_m == 2V62005523). not clear who the "other" cause applies to.
dat %>%
  mutate(flag = ifelse(status_c == "Died", 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(flag = sum(flag)) %>%
  filter(flag > 1 & status_c == "Died") %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1, flag) %>%
  head()
# Yes this happens

# In cases with multiple deaths, adjust strata
# There aren't too many of these, so going to do this in a loop
dat_adj <- dat %>%
  mutate(flag = ifelse(!(sample %in% c("Stillbirth", "Strata6", "2024all")) & status_c == "Died" &
                         sample_c != "0_10+", 1, 0)) %>%
  group_by(rid_m) %>%
  mutate(flag = sum(flag)) %>%
  filter(flag > 1 & status_c == "Died")

# instance of 0_10+ that was getting recoded as 0_Unk
# should no longer by in dat_adj
#dat_adj %>%
#  filter(rid_m == "2VBC021419") %>%
#  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1,)

dat_rest <- subset(dat, !(uid_c_dss %in% dat_adj$uid_c_dss))

# making sure no adjustments need to be made here***
dat_rest %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1) %>%
  head()
# Looks ok
sort(unique(dat_rest$sample_c))

# mother_ids of those that need adjusting
v_adj <- unique(dat_adj$rid_m)
# if va is birth_asphyxia, belongs to sample_c1 of 1 (check if multiple neonatal deaths though)
# if va is RI+Congenital, belongs to sample_c1 of 2 (check if multiple postneonatal deaths though)
# if va is drowning, belongs to sample_c1 of 3 (check if multiple child deaths though)
# if VA is no strata and one of sample_c1 is 5 (meaning the sample is not 2024all), va belong to 5
# if VA is Other and one of sample_c1 is 5 and one is not, va belongs to the sample_c1 that is not 5
# if VA is Other and more than one of sample_c1 is not 5, can't be determined
df_res <- data.frame()
for(i in 1:length(v_adj)){
  
  adj <- subset(dat_adj, rid_m == v_adj[i])
  cod <- unique(adj$va)
  # check
  # adj %>%
  #   select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1)
  
  if(cod == "Birth Asphyxia"){
    adj <- adj %>%
      mutate(sample_c = case_when(
        sample_c1 == 1 ~ "1_Birth Asphyxia",
        TRUE ~ "Unk"))
  }
  if(cod == "RI+Congenital"){
    adj <- adj %>%
      mutate(sample_c = case_when(
        sample_c1 == 2 ~ "2_RI+Congenital",
        TRUE ~ "Unk"))
  }
  if(cod == "Drowning"){
    adj <- adj %>%
      mutate(sample_c = case_when(
        sample_c1 == 3 ~ "3_Drowning",
        TRUE ~ "Unk"))
  }
  if(cod == "NoStrata"){
    adj <- adj %>%
      mutate(sample_c = case_when(
        sample_c1 == 5 ~ "5",
        TRUE ~ "Unk"))
  }
  if(cod == "Other"){
    # if there is more than one record that is not a 5
    if(nrow(subset(adj, sample_c1 != 5)) > 1){
      adj$sample_c <- "Unk"
    }
    # if after removing the 5s, there is only one record, resolve
    if(nrow(subset(adj, sample_c1 != 5)) == 1){
      adj <- adj %>%
        mutate(sample_c = case_when(
          sample_c1 != 5 ~ paste0(sample_c1, "_" , va),
          TRUE ~ "Unk"))
    }
  }
  
  # if after removing the unknowns, there is more than one sample_c with the same COD, unresolved
  if(sum(duplicated(subset(adj, !(sample_c %in% "Unk"))$sample_c)) > 0){
    adj$sample_c <- "Unk"
    adj$resolved <- 0
  }
  # if all are unknown, unresolved
  if(nrow(subset(adj, !(sample_c %in% "Unk"))) == 0){
    adj$sample_c <- "Unk"
    adj$resolved <- 0
  }
  # if after removing the unknowns, there is only one sample_c with a unique COD, resolved
  if(nrow(subset(adj, !(sample_c %in% "Unk"))) != 0 &
      sum(duplicated(subset(adj, !(sample_c %in% "Unk"))$sample_c)) == 0 ){
    adj$resolved <- 1
  }
  
  # check if performed well
  #adj %>%
  #  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1, resolved)
  df_res <- rbind(df_res, adj)
  
}

# if resolved, recode Unk
adj <- df_res %>%
  mutate(sample_c = case_when(
    resolved == 1 & sample_c != "Unk" ~ sample_c,
    resolved == 1 & sample_c == "Unk" ~ paste0(sample_c1, "_Unk"),
    resolved == 0 ~ paste0(sample_c1, "_Unk"),
    TRUE ~ NA
  )) 

adj %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1, resolved) %>%
  head()

# remove cols
adj$flag <- adj$resolved <- NULL

# recombine
dat <- rbind(dat_rest, adj)
dat <- dat[order(dat$rid_m, dat$dob_c_dss), ]

# make separate column for cod type/cat
dat$sample_c_unit <- sub('.*_', '', dat$sample_c)
dat$sample_c_unit[dat$sample_c_unit == "5"] <- "5-9 death"
dat$sample_c_unit[dat$sample_c_unit == "6"] <- "Surviving"
# remove spaces and symbols in cod
dat$sample_c <- sub(" ", "", dat$sample_c)
dat$sample_c <- sub("+", "", dat$sample_c)

dat %>%
  select(rid_m, name_m, preg_res_dss, dob_c_dss, dod_c_dss, aadd, sample, va, status_c, sample_c, sample_c1, sample_c_unit) %>%
  head()

# remove columns
dat$name_m <- NULL
dat$aadd <- NULL # this variable is in overall, and it comes from survey
nrow(dat)

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/hdss-clean.rds")

