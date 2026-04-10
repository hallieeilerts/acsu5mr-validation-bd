################################################################################
#' @description Repeat steps preparing survey and hdss data on overall data that has been matched by date
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
library(stringr)
#' Inputs
# survey_final_all2: livebirth and stillbirth records from the survey
dat <- read_dta("./data/20250930/overall_name_lb.dta")
################################################################################

# Variables that are different with overall_date
# overall_date - overall_dob
# preg_res_dss - po
# dob_m_dss - dob_m
# coo_m_dss - coo_m
# doo_m_dss - doo_m
# dob_c_dss - dob_c
# dob_c_sur - c220
# doo_c_dss - dod_c
# coo_c_dss - CCOD
# name_m_dss - name_m
# name_c_dss - name_c
# sex_c_dss - sex_c
# f_intype_dss - f_intype

# Variable examination ----------------------------------------------------

# to help decide which variables to keep

# serial is a count, serial is a per respondent count
# rid_m and rid_c are effectively id's from the dss (they are only present for matches)
dat %>%
  select(serial, serial1, rid_m, rid_c, uid_c_sur, match_n)
nrow(dat) # 2408
length(unique(dat$serial)) # 2224
# but serial is no longer unique because of double matches
dat %>%
  group_by(serial) %>%
  mutate(nserial = n()) %>%
  filter(nserial >1) %>%
  select(serial, match_n,
         rid_m, x1, # mother id for both survey and dss, date of interview
         rid_c, po, dob_c,# child-level information from dss
         uid_c_sur, c215, c216, c220, c223, # child-level information from survey
  ) %>%
  head()
# uid_c is not unique for same reason as serial
nrow(dat) # 2408
length(unique(dat$uid_c)) # 2244

# what is the difference between all the uid's?
dat %>%
  select(serial, match_n,
         rid_m, x1, # mother id for both survey and dss, date of interview
         rid_c, po, dob_c,# child-level information from dss
         uid_c, uid_c_dss, uid_c_sur, 
         c215, c216, c220, c223, # child-level information from survey
  ) %>%
  head()
nrow(subset(dat, is.na(uid_c))) # 0
nrow(subset(dat, is.na(uid_c_dss))) # 0
nrow(subset(dat, is.na(uid_c_sur))) # 0
sum(dat$uid_c != dat$uid_c_dss) # 618
sum(dat$uid_c != dat$uid_c_sur) # 185
# uid_c is not the same as uid_c_sur
# in overall_date it is the same
# this happens when uid_c_sur is missing
nrow(subset(dat, uid_c != uid_c_sur & uid_c_sur == "")) # 185
# and when uid_c matches uid_c_dss
# so these are unmatched dss ids
nrow(subset(dat, uid_c != uid_c_sur & uid_c == uid_c_dss & is.na(match_n))) # 185

# neither uid_c or rid_c is unique
# rid_c is only present if matched to hdss
# uid_c can be duplicate
nrow(dat) # 2408
length(unique(dat$rid_c)) # 2162
nrow(subset(dat, is.na(rid_c))) # 0
length(unique(dat$uid_c)) # 2244
nrow(subset(dat, is.na(uid_c))) # 0
# examine the cases where uid_c is not unique
# uid_c is a variable that was made from the serial and parity
# so there are some cases where a case in the dss and fph have the same uid, but they weren't matched up
# (because the names were so different)
# so the uid_c is duplicate
dat %>%
  group_by(uid_c) %>%
  mutate(nuidc = n()) %>%
  filter(nuidc >1) %>%
  select(serial, match_n,
         rid_m, x1, # mother id for both survey and dss, date of interview
         uid_c_dss, rid_c, po, dob_c,# child-level information from dss
         uid_c_sur, c215, c216, c220, c223, # child-level information from survey
  ) %>%
  arrange(uid_c) %>%
  head()

# the source of the parity variables (dss or fph) in overallDob are clearly labeled

# in overallDob, the child's dob variable from the survey is still c220.
# no need to rename.
# all untransformed survey variables should have their original names
unique(dat$c220)[1:5]

# it does have dod_c (date of death child) instead of doo_c as in overallDate
head(dat$dod_c)
# it does have CCOD (cause of death of child) instead of coo_c as in overallDate
head(dat$CCOD)

# what is the difference between match_n and match_n2
unique(dat$match_n)
unique(dat$match_n2)
# match_n has extra labels for stb, msc, ab; those these aren't used
# match_n has NAs and match_n2 doesn't.
# when match_n is NA, match_n2 is 2 (only dss)
unique(subset(dat, is.na(match_n))$match_n2) 
table(dat$match_n, dat$match_n2, useNA = "always")
# number 1 (fully) in match_n is 3 (in both) in match_n2
# number 2 (partially) in match_n is also 3 (in both) in match_n2
# what is the difference between fully and partially matched?
# looks like it just refers to whether the name was an exact match or not
nrow(subset(dat, match_n == 2 & match_n2 == 3)) # 60
dat %>%
  filter(match_n ==2 & match_n2 == 3) %>%
  select(name_c, c218, dob_c, c220) %>%
  head()
# number 3 (not matched) in match_n is 1 (only in survey) in match_n2
# NA in match_n is 2 (only in dss) in match_n2
head(subset(dat, is.na(match_n))[,c("match_n", "match_n2")])
# to match overallDate, need to create a match_score variable that is 1 for match and NA for no match


# Select variables --------------------------------------------------------

# Remove unnecessary variables
dat <- dat %>%
  select(-c(
    #Userial1, # same as 1st serial
    serial2, # 2nd serial
    serial3, # 3rd serial (provided) 
    uid_c, # child unique id serial1 + parity, same as uid_c_sur
    parity_s_dss,
    sd, # dup of int_date
    start, # dup of str_tim
    end, # dup of end_tim
    x1_a, #' interview result (others)
    c200, # section start time
    cal_216, # pregnancy outcome type (calculate)
    cal_223, # current condition of pregnancy
    `_id`,
    `_uuid`, 
    `_submission_time`,
    `_index1`,
    `_parent_table_name`,
    `_index`,
    `_tags`, 
    `_notes`,
    Location, 
    `_Location_latitude`, 
    `_Location_longitude`, 
    `_Location_altitude`, 
    `_Location_precision`, 
    aa, # interview start time
    s3,  # 3rd serial (generated)
    a00, # section A start time
    a0_1, # respondent name
    a0, # enumerator name
    a3, a3_a, a4, a4_a, a5, a5_a, a6, a6_a, a7, # roof and floor material, drinking water source, asset concatenation
    starts_with("w"), # asset ownership
    a000, # section A end time
    b00,  # section B start time
    b000, # section B end time
    b117_a, # demanded language
    b123_a, #' b123_a type mobile (others)
    b130_a, # religion (other)
    c100, # section C1 start time
    c1000, # section C1 end time
    c222_A, # pregnancy after the last one
    c222_B, # pregnancy serial accuracy
    c2000, # section C2 end time
    c3000, # section C2 end time
    d00, # section D start time
    d000, # section D end time
    dd, # interview end time
    resp_age, # recode of age
    `__version__`, 
    metainstanceID, 
    `_parent_index`,
    sd, # interview date
    sd0, # interview date in character format
    latest,
    vil,
    hh,
    name_bari,
    name_head,
    std_peri,
    # section start and end times
    start_time,
    end_time,
    int_dura,
    a_str_tim,a_end_tim,a_start_time,a_end_time,a_int_dura,
    b_str_tim,b_end_tim, b_start_time,b_end_time,b_int_dura,
    c_str_tim,c_end_tim,c_start_time,c_end_time,c_int_dura,
    c_ob_str_tim,c_ob_end_tim,c_ob_start_time,c_ob_end_time,c_ob_int_dura,
    d_str_tim,d_end_tim,d_start_time,d_end_time,d_int_dura,
    # summary birth history
    c201,    #' c201 gave birth (yes/no)
    c202,    #' c202 children living with respondent
    c203_a,  #' c203_a son living with respondent
    c203_aa, #' c203_aa son living with respondent (calculate)
    c203_b,  #' c203_b daughter living with respondent
    c203_bb, #' c203_bb daughter living with respondent (calculate)
    c204,    #' c204 alive chilren not living with respondent
    c205_a,  #' c205_a son not living with respondent
    c205_aa, #' c205_aa son not living with respondent (calculate)
    c205_b,  #' c205_b daughter not living with respondent
    c205_bb, #' c205_bb daughter not living with respondent (calculate)
    c206,    #' c206 children born alive but died later
    c207_a,  #' c207_a son born alive but died later
    c207_aa, #' c207_aa son born alive but died later (calculated)
    c207_b,  #' c207_b daughter born alive but died later
    c207_bb, #' c207_bb daughter born alive but died later (calculated)
    c208,    #' c208 total children born alive (calculate)
    c209,    #' c209 total child number accuracy
    c210,    #' c210 born dead/miscarriage/abortion (yes/no)
    c211,    #' c211 stillbirth/miscarriage/abortion number
    c211_a,  #' c211_a stillbirth/miscarriage/abortion number (calculated)
    c212,    #' c212 total number of pregnancy (calculated)
    c212_1,  #' c212_1 total pregnancy accuracy
    c216_a, # pregnancy outcome type 2
    c220_a, # pregnancy ending date
    preg_num,#' preg_num number of total pregnancy
    num_of_preg, #' num_of_preg recode of preg_num
    c215_a,  # c215_a pregnancy serial number
    c222,    # c222 other pregnancy before
    c222_a,  # c222_a other pregnancy in between
    age_alive,#' age_alive RECODE of c225 (Age at last birthday)
    parity_s_sur, # parity from survey
    alive # alive on survey date
  )) 


# keep variables and order
# hdss and then survey
dat <- dat %>%
  select(
    serial, # count from 1 to n
    serial1, # 1st serial (per respondent)
    match_n, # name matching
    #match_score, # preg date matching score
    #match_level,
    sample, 
    sample2,
    rid_m, # rid mother
    cid_m, # cid mother
    rid_c, # rid child
    uid_c_sur, # child child unique id serial1+parity
    uid_c_dss, # unique id for pregnancy (serial+parity) dss
    int_date, #' interview date
    str_tim,
    end_tim, 
    x0, #' visit time
    x0_1, #' 1st visit date
    x0_2, #' 2nd visit date
    x0_3, #' 3rd visit date
    x1, #' interview result
    ifr, # final interview result
    name_m, # name mother
    age, # age of mother
    dob_m, # dob mother
    coo_m,  #' cause of out mother
    doo_m, #' doo_m date of out mother
    age_out_m, # mothers age at out date
    age_extraction_m, # mothers age at data extraction
    mot_in_date,
    mig_result,
    f_intype, # first in type of mother
    parity_n_dss,
    po,
    name_c, 
    sex_c, 
    dob_c,
    dod_c,
    CCOD, 
    #va1, va2, va3, va4, va5, va6, va7,
    a1, # hh member number
    HH_size, # recode of a1 (hh member number)
    asset_score, # scores for factor 1
    asset_quintile, # 5 quantiles of asset_score
    c215, # pregnancy type
    c216, # pregnancy outcome type
    c218, # name of child
    c217, # movement after birth
    c219, # gender of child
    c220, # dob
    c221, # pregnancy ending time (months)
    c221_a, # pregnancy ending time (weeks)
    c221_aa, # pregnancy ending time (months/weeks)
    c221_aaa, #  pregnancy ending time (text)
    c223, # pregnancy outcome type (calculated)
    c224, # child alive/dead
    c225, # age at last birthday
    c226, # living with respondent
    c228, # age at death (day/month year)
    c228_aa, # age at death (day)
    c228_bb, # age at death (month)
    c228_ccc, # age at death (year)
    c228_cc, # age at death (text)
    c228_d, # completed 1st birthday
    c228_B, # age (month) at death
    b110_a, #' respondents birth year
    b110_b, #' respondents birth month
    b111, #' respondents age
    b111_a, #' respondents marital status 
    b112, #' respondents health condition
    b112_a, #' respondents health score
    self_hscore, #' Recode of b112_a
    b113, #' respondents education
    b113_a, #' education (school/madrasa)
    b113_b, #' madrasa type
    b114, #' completed grade
    b115, #' completed class
    b117, #' reading capability
    b119, #' newspaper read
    b120, #' radio hear
    b121, #' television watch
    b122, #' own mobile
    b123, #' type mobile
    b130, #' religion
    c244, #' presence of others during section C
    c244_a, #' type of person present
    c244_a1, #' adult male (yes/no)
    c244_a_1, #' adult male number
    c244_a2,#' adult female (yes/no)
    c244_a_2, #' adult female number
    c244_a3, #' adolescent boy (yes/no)
    c244_a_3, #' adolescent boy number
    c244_a4, #' adolescent girl (yes/no)
    c244_a_4, #' adolescent girl number
    c244_a5,#' children (yes/no)
    c244_a_5,#' children number
    d1, # interview location
    d2, # interview interuption (yes/no)
    d3, # interview interuption time
    d4, # familiarity with respondent
    d5, # familiarity with other member (yes/no)
    d6, # how much familiar with other member
    d7, # respondent coorperation
    d8, # anyone else around
    d9, # comments about interview
    d10, # other work during interview
    d10_a, # type of other work
    d10_a1, # cooking
    d10_a2, # feeding HH members
    d10_a3, # feeding cattle/poultry
    d10_a4, # sewing
    d10_a5, # other household chores
    d10_a6, # others
    d10_a_1, # specify household chores
    d10_a_2, # specify others
    d11, # emotional breakdown
    d11_a, # counseling
    d12, # discomfort (yes/no)
    d12_a, # discomforting information
    d12_a1, # miscarriage information
    d12_a2, # abortion information
    d12_a3, # previous marriage information
    d12_a4, # other information
    d12_a_1, # specify other information
    d13, # seek help from other people
    d13_a, # other people interfered
    d14, # respondent in urgency
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
  mutate(mot_in_date = as.Date(mot_in_date, format = "%d-%b-%Y"),
         int_date = as.Date(int_date, format = "%Y-%m-%d"),
         dob_c = as.Date(dob_c, format = "%Y-%m-%d"),
         dod_c = as.Date(dod_c, format = "%Y-%m-%d")
  )


# Swap incorrect matches to same mother -----------------------------------

# when survival status doesn't match
dat %>%
  filter(match_n %in% c("Fully:m", "Partially:m")) %>% # for hdss/survey matches
  filter((c224 == "No" & is.na(dod_c)) | # survey says died and dss has no dod
           (c224 == "Yes" & !is.na(dod_c)) ) %>% # survey says surviving and dss has dod
  select(rid_m, name_c, sex_c, dob_c, po, dod_c, 
       c215, c216, c218, c219, c220, c223, c224, c228, c228_aa, c228_bb, c228_cc) # c224 is "child is still alive?"
# two children born to 1D93002840 have been swapped. correct these here.
# there are three others. 
# the one born to 3V11000917 has the same dob in each source.
# the one born to 4V76021009 has a slightly different dob, but the name is the exact same.
# the one born to 4W00036810 has dob that are 5 years different and the name is different. unmatch in section below

dat <- dat %>%
  mutate(recnr = 1:n())

df_swap <- dat %>%
  filter(match_n %in% c("Fully:m", "Partially:m")) %>% # for hdss/survey matches
  filter(rid_m == "1D93002840" & 
           (c224 == "No" & is.na(dod_c) | # survey says died and dss has no dod
           c224 == "Yes" & !is.na(dod_c)))
df_other <- subset(dat, !(recnr %in% df_swap$recnr))

# swap the child-level survey information
df_swap <- df_swap %>%
  group_by(rid_m) %>%
  mutate(minrecnr = min(recnr),
         maxrecnr = max(recnr)) 
# select all child-level variables, shift record number
kid1 <- df_swap %>%
  select(rid_m, recnr, minrecnr, maxrecnr,
         #rid_c, uid_c_dss, parity, preg_res_dss, name_c_dss, sex_c_dss, dob_c_dss, doo_c_dss, coo_c_dss, va1, va2, va3, va4, va5, va6, va7,
         uid_c_sur, c215, c216, c218, c217, c219, c220, c221, c221_a, c221_aa, c221_aaa, c223, c224, c225, c226, c228, c228_aa, c228_bb, c228_ccc,
         c228_cc, c228_d, c228_B) %>%
  filter(recnr == minrecnr) %>%
  mutate(recnr = maxrecnr) %>% # recode as max
  select(-c(minrecnr, maxrecnr))
kid2 <- df_swap %>%
  select(rid_m, recnr, minrecnr, maxrecnr,
         #rid_c, uid_c_dss, parity, preg_res_dss, name_c_dss, sex_c_dss, dob_c_dss, doo_c_dss, coo_c_dss, va1, va2, va3, va4, va5, va6, va7,
         uid_c_sur, c215, c216, c218, c217, c219, c220, c221, c221_a, c221_aa, c221_aaa, c223, c224, c225, c226, c228, c228_aa, c228_bb, c228_ccc,
         c228_cc, c228_d, c228_B) %>%
  filter(recnr == maxrecnr) %>%
  mutate(recnr = minrecnr) %>% # recode as min
  select(-c(minrecnr, maxrecnr))
# merge on child-level variables for other kid
kid1corrected <- df_swap %>%
  filter(recnr == minrecnr) %>%
  select(-c(uid_c_sur, c215, c216, c218, c217, c219, c220, c221, c221_a, c221_aa, c221_aaa, c223, c224, c225, c226, c228, c228_aa, c228_bb, 
            c228_ccc,c228_cc, c228_d, c228_B)) %>%
  left_join(kid2, by = c("recnr", "rid_m"))  
kid2corrected <- df_swap %>%
  filter(recnr == maxrecnr) %>%
  select(-c(uid_c_sur, c215, c216, c218, c217, c219, c220, c221, c221_a, c221_aa, c221_aaa, c223, c224, c225, c226, c228, c228_aa, c228_bb, 
            c228_ccc,c228_cc, c228_d, c228_B)) %>%
  left_join(kid1, by = c("recnr", "rid_m"))  

# check that we have swapped the names so now it is a better match
df_swap %>%
  select(rid_m, recnr, minrecnr, maxrecnr,
         rid_c, uid_c_dss, po, name_c, sex_c, dob_c, dod_c, CCOD,
         uid_c_sur, c215, c216, c218, c217, c219, c220, c221, c221_a, c221_aa, c221_aaa, c223, c224, c225, c226, c228, c228_aa, c228_bb, c228_ccc,
         c228_cc, c228_d, c228_B)
kid1corrected %>%
  select(rid_m, recnr, minrecnr, maxrecnr,
         rid_c, uid_c_dss, po, name_c, sex_c, dob_c, dod_c, CCOD,
         uid_c_sur, c215, c216, c218, c217, c219, c220, c221, c221_a, c221_aa, c221_aaa, c223, c224, c225, c226, c228, c228_aa, c228_bb, c228_ccc,
         c228_cc, c228_d, c228_B)

kidscorrected <- kid1corrected %>%
  bind_rows(kid2corrected) %>%
  select(-c(minrecnr, maxrecnr))

datnew <- rbind(df_other, kidscorrected)
nrow(dat) == nrow(datnew) # TRUE
datnew <- datnew[order(datnew$rid_m, datnew$c220),]
datnew$recnr <- NULL
dat <- datnew



# Fix incorrect matches ---------------------------------------------------


# when survival status doesn't match and dob is more than 4 years different
dat %>%
  filter(match_n %in% c("Fully:m", "Partially:m")) %>% # for hdss/survey matches
  filter((c224 == "No" & is.na(dod_c)) | # survey says died and dss has no dod
           (c224 == "Yes" & !is.na(dod_c)) ) %>% # survey says surviving and dss has dod
  filter(abs(as.numeric(dob_c - c220)/365.25) >= 4) %>%
  select(rid_m, uid_c_dss, name_c, sex_c, dob_c, po, dod_c, 
         c215, c216, c218, c219, c220, c223, c224, c228, c228_aa, c228_bb, c228_cc) # c224 is "child is still alive?"
# rid_m 4W00036810 uid_c_dss 1915+2 has dobs that are 5 years different and the name is different. unmatch

# when dob is very different
dat %>%
  filter(match_n %in% c("Fully:m", "Partially:m")) %>% # for hdss/survey matches
  filter(abs(as.numeric(dob_c - c220)/365.25) >= 4) %>% # more than 4 years different in dob
  filter(!(!is.na(dod_c) & c224 == "No")) %>% # exclude those where both sources say individual died. be more lenient for those
  select(match_n, rid_m, uid_c_dss, name_c, sex_c, dob_c, po, dod_c, 
         c215, c216, c218, c219, c220, c223, c224, c228, c228_aa, c228_bb, c228_cc) # c224 is "child is still alive?"
# three of these look like data entry errors in the FPH. the year of the dob is wrong, but the month/day are right.
# rid_m 1F00011240, 2DX0011209, 2V02002630
# and then two of these look like they could be different kids that should not have been matched
# 4W00036810, 5V49009404
# only unmatch the one where the survival status does not match (4W00036810)

dat <- dat %>%
  mutate(recnr = 1:n())

df_unmatch <- dat %>%
  filter(rid_m == "4W00036810" & is.na(dod_c) & c224 == "No")
df_other <- subset(dat, !(recnr %in% df_unmatch$recnr))

# variables that belong to both
# "rid_m", "match_n2", "match_n", 
# survey variables
v_sur <- c("serial", "sample", "sample2",  "cid_m", 
           "uid_c_sur",
           "int_date", "str_tim", "end_tim", "x0", "x0_1", "x0_2",
           "x0_3", "x1", "ifr", 
           "a1","HH_size","asset_score" ,"asset_quintile" ,
           "c215", "c216", "c218", "c217", "c219", "c220", "c221", "c221_a",
           "c221_aa", "c221_aaa", "c223", "c224", "c225", "c226", "c228", "c228_aa",
           "c228_bb", "c228_ccc", "c228_cc", "c228_d", "c228_B", "b110_a", "b110_b", "b111",
           "b111_a", "b112", "b112_a", "self_hscore", "b113", "b113_a", "b113_b", "b114",
           "b115", "b117", "b119", "b120", "b121", "b122", "b123", "b130",
           "c244", "c244_a", "c244_a1", "c244_a_1", "c244_a2", "c244_a_2", "c244_a3", "c244_a_3",
           "c244_a4", "c244_a_4", "c244_a5", "c244_a_5", "d1", "d2", "d3", "d4",
           "d5", "d6", "d7", "d8", "d9", "d10", "d10_a", "d10_a1",
           "d10_a2", "d10_a3", "d10_a4", "d10_a5", "d10_a6", "d10_a_1", "d10_a_2", "d11",
           "d11_a", "d12", "d12_a", "d12_a1", "d12_a2", "d12_a3", "d12_a4", "d12_a_1",
           "d13", "d13_a", "d14", "parity_n_sur", "recnr")
# dss variables

v_dss <- c("rid_c",
           "uid_c_dss", 
           "po", "name_c", "sex_c", "dob_c", "dod_c", "CCOD")
# mother-level dss variables don't need to be recoded as NA
# "name_m", "age", "dob_m", "coo_m", "doo_m", "age_out_m", "age_extraction_m", "mot_in_date", "mig_result", "f_intype", "parity_n_dss"

# for unmatched hdss record, recode survey columns as NA
df_unmatch_dss <- df_unmatch %>%
  mutate(across(all_of(v_sur), ~NA))  %>%
  mutate(match_n = NA, # this variable is NA for unmatched HDSS records
         match_n2 = "Only in DSS")
# for unmatched survey record, recode dss columns as NA, and recode match_n
df_unmatch_sur <- df_unmatch %>%
  mutate(across(all_of(v_dss), ~NA)) %>%
  mutate(match_n = "Not matched",
         match_n2 = "Only in survey")

datnew <- rbind(df_other, df_unmatch_dss, df_unmatch_sur )
nrow(dat) + 1 == nrow(datnew)  # TRUE
datnew <- datnew[order(datnew$rid_m, datnew$c220),]
datnew$recnr <- NULL
dat <- datnew

# Rename variables with dss suffix ----------------------------------------

# Rename columns that actually came from the DSS to make that clear
dat <- dat %>%
  rename(name_m_dss = name_m,
         dob_m_dss = dob_m,
         age_m_dss = age,
         ageout_m_dss = age_out_m,
         ageext_m_dss = age_extraction_m,
         doi_m_dss = mot_in_date,
         toi_m_dss = f_intype,
         migres_m_dss = mig_result,
         pregout_dss = po,
         name_c_dss = name_c,
         sex_c_dss = sex_c,
         dob_c_dss = dob_c,
         dod_c_dss = dod_c,
         cod_c_dss = CCOD,
         parity_dss = parity_n_dss)

# Rename variables with sur suffix ----------------------------------------

dat <- dat %>%
  rename(int_date_sur = int_date,
         parity_sur = parity_n_sur)

# Rename key variables ----------------------------------------------------

# Rename mother-level strata to clarify whether by age or cause
# mother-level strata by age of index child
unique(dat$sample) 
# mother-level strata by age/cod of index child
unique(dat$sample2) 

dat <- dat %>%
  rename(
    mstrata_a = sample, 
    mstrata_ac = sample2 
  )

# make sure mstrata_a and mstrata_ac apply to all records for the mother
# in this file, the unmatched records don't have it even though they should
nrow(subset(dat, is.na(mstrata_a))) # 186
nrow(subset(dat, is.na(mstrata_ac))) # 186
key_mstrata <- dat %>% 
  select(rid_m, mstrata_a, mstrata_ac) %>%
  distinct() %>%
  filter(!is.na(mstrata_a))
dat <- dat %>%
  select(-c(mstrata_a, mstrata_ac)) %>%
  left_join(key_mstrata, by = "rid_m")
nrow(subset(dat, is.na(mstrata_a))) # 1
nrow(subset(dat, is.na(mstrata_ac))) # 1
subset(dat, is.na(mstrata_a))$rid_m # "5D34010707"
# I looked it up in the survey file in prep-survey, and this mother should be 
# mstrata_a of Neonatal, and mstrata_ac of Neonatal (other)
dat$mstrata_a[dat$rid_m == "5D34010707"] <- "Neonatal"
dat$mstrata_ac[dat$rid_m == "5D34010707"] <- "Neonatal (other)"
nrow(subset(dat, is.na(mstrata_a))) # 0
nrow(subset(dat, is.na(mstrata_ac))) # 0

# in overallDate i coalesce va to make mstrata_c
# there are not va variables in this file, so will create from mstrata_ac
# This is mother-level information on child COD
dat <- dat %>%
  mutate(mstrata_c = case_when(
    mstrata_ac == "Neonatal (other)" ~ "Other",
    mstrata_ac == "Stillbirth" ~ "Stillbirth",
    mstrata_ac == "Neonatal (birth asphyxia)" ~ "Birth Asphyxia",
    mstrata_ac == "1-4 year (other)" ~ "Other",
    mstrata_ac == "Live birth" ~ NA,
    mstrata_ac == "2024all" ~ NA,
    mstrata_ac == "5-9 year" ~ "NoStrata",
    mstrata_ac == "1-4 year (drowning)" ~ "Drowning",
    mstrata_ac == "Postneonatal (RI+con)" ~ "RI+Congenital",
    mstrata_ac == "Postneonatal (other)" ~ "Other",
    TRUE ~ NA
  ))
# NoStrata, Birth Asphyxia, Other, RI+Congenital, Drowning 


# Create new match variable -----------------------------------------------

dat <- dat %>%
  mutate(match_score = case_when(
    match_n2 == "In both" ~ 1,
    TRUE ~ NA
  ))

# Create new variables from dss -------------------------------------------

# age at death in days
dat$aadd_dss <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)
dat$aadm_dss <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)/30.5
dat$aady_dss <- as.numeric(dat$dod_c_dss - dat$dob_c_dss)/365.25

# child status variable which combines survival status with adverse pregnancy outcomes
dat <- dat %>%
  mutate(cstatus_dss = case_when( # formerly cstatus
    pregout_dss == "Livebirth" & !is.na(dod_c_dss) ~ "Died",
    pregout_dss == "Livebirth" & is.na(dod_c_dss) ~ "Surviving",
    pregout_dss == "Stillbirth" ~ "Stillbirth",
    pregout_dss == "Miscarriage" ~ "Miscarriage",
    pregout_dss == "Abortion" ~ "Abortion",
    TRUE ~ pregout_dss
  ))

# assign child-level strata by age
dat <- dat %>%
  mutate(cstatus_agesp_dss = case_when( 
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

# tidy
dat <- dat[order(dat$rid_m, dat$dob_c_dss), ]


# Create new variables from sur ----------------------------------------------------

# child status variable which combines survival status with adverse pregnancy outcomes
dat <- dat %>%
  mutate(cstatus_sur = case_when( # formerly cstatus_surv
    c223 == "Born alive" & c224 == "No" ~ "Died",
    c223 == "Born alive" & c224 == "Yes" ~ "Surviving",
    c223 == "Born dead" ~ "Stillbirth",
    c223 == "Miscarriage" ~ "Miscarriage",
    c223 == "Abortion" ~ "Abortion",
    TRUE ~ c223
  ))

# simplified age at death variables
# to replace c228 variables with 
dat %>%
  filter(!is.na(c224) & c224 == "No") %>%
  select(c224, c228, c228_aa, c228_bb, c228_ccc, c228_cc, c228_d, c228_B)
dat <- dat %>% mutate(aad_unit_sur = case_when(
  c228 == "In days" ~ 1,
  c228 == "In months" ~ 2,
  c228 == "In years" ~ 3,
  TRUE ~ NA
)) %>%
  mutate(aad_val_sur = case_when(
    aad_unit_sur == 1 ~ c228_aa,
    aad_unit_sur == 2 ~ c228_bb,
    aad_unit_sur == 3 ~ c228_ccc,
    TRUE ~ NA
  ))
dat %>%
  filter(!is.na(c224) & c224 == "No") %>%
  select(c224, c228, c228_aa, c228_bb, c228_ccc, c228_cc, c228_d, c228_B, aad_unit_sur, aad_val_sur)
# Check that aad_unit never missing when c228 reported
nrow(subset(dat, is.na(aad_unit_sur) & !is.na(c228) & c228 == "Yes")) # 0
# Check that aad_value never missing when c228 unit reported
nrow(subset(dat, is.na(aad_val_sur) & (!is.na(c228_aa) | !is.na(c228_bb) | !is.na(c228_ccc) ))) # 0
# Transform into months
dat$aadm_sur <- dat$aad_val_sur
dat$aadm_sur[which(dat$aad_unit_sur == 1)] <- dat$aadm_sur[which(dat$aad_unit_sur == 1)] / 30.5
dat$aadm_sur[which(dat$aad_unit_sur == 3)] <- dat$aadm_sur[which(dat$aad_unit_sur == 3)]*12
# Transform into days
dat$aadd_sur <- dat$aad_val_sur
dat$aadd_sur[which(dat$aad_unit_sur == 2)] <- dat$aadd_sur[which(dat$aad_unit_sur == 2)] * 30.5
dat$aadd_sur[which(dat$aad_unit_sur == 3)] <- dat$aadd_sur[which(dat$aad_unit_sur == 3)] * 365.25
# Transform into years
dat$aady_sur <- dat$aad_val_sur
dat$aady_sur[which(dat$aad_unit_sur == 1)] <- dat$aady_sur[which(dat$aad_unit_sur == 1)] / 365.25
dat$aady_sur[which(dat$aad_unit_sur == 2)] <- dat$aady_sur[which(dat$aad_unit_sur == 2)] / 12
# Delete c228 cod columns
dat <- dat %>%
  select(-c(c228, c228_aa, c228_bb, c228_ccc, c228_cc, c228_d, c228_B))

# date of death
dat$dod_c_sur <- dat$c220 + dat$aadd_sur

# child-level age strata
dat <- dat %>%
  mutate(cstatus_agesp_sur = case_when(
    cstatus_sur == "Abortion" ~ "Abortion",
    cstatus_sur == "Miscarriage" ~ "Miscarriage",
    cstatus_sur == "Stillbirth" ~ "Stillbirth",
    cstatus_sur == "Surviving" ~ "Surviving",
    cstatus_sur == "Died" & aadd_sur < 28 ~ "Neonatal",
    cstatus_sur == "Died" & aadd_sur >= 28 & aadd_sur < 365 ~ "Postneonatal",
    cstatus_sur == "Died" & aadd_sur >= 365 & aadd_sur < 5*365 ~ "1-4",
    cstatus_sur == "Died" & aadd_sur >= 5*365 & aadd_sur < 10*365 ~ "5-9",
    cstatus_sur == "Died" & aadd_sur >= 10*365 ~ "10+",
    TRUE ~ NA
  )) 

# tidy --------------------------------------------------------------------

# tidy
dat <- dat[order(dat$rid_m, dat$c220), ]

# check to see have all desired variables from both survey and hdss
dat %>%
  select(match_n,
         rid_m, serial1, x1, # mother id for both survey and dss, date of interview
         mstrata_a, mstrata_ac, mstrata_c, # mother-level strata drawn from dss for sampling, but only available in survey file?
         rid_c, pregout_dss, cstatus_dss, cstatus_agesp_dss, dob_c_dss, dod_c_dss, cod_c_dss, # child-level information from dss
         uid_c_sur, c215, c216, c220, c223, aady_sur, cstatus_sur, cstatus_agesp_sur # child-level information from survey
  ) %>%
  head()


# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/clean/overallDob-clean.rds")


