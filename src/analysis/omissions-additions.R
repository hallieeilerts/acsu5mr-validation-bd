################################################################################
#' @description analyse matches/nonmatches at the event-level
#' Assess:
#' omissions/additions of live births, deaths
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
library(officer)
library(flextable)
#' Inputs
overall <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Assign denominator: 
# A - Mothers: lifelong residents
# C - Mother+pregnancies: 10 yr residents
# D - Mother+pregnancies: 5 yr residents
# Since this is event level agreement, we use DSS as the reference standard.
# DSS and FPH events are included conditional on the DSS DOB, and only FPH DOB if unmatched.
# Hence there is no need for denomC_dss and denomC_sur columns. There is just one denomC.
dat <- overall %>%
  mutate(denomA = ifelse(dob_m_dss == doi_m_dss, 1, 0),
         denomC = ifelse(
           # mother's in-migration is more than 10 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 10 & 
             # dss dob is within past 10 years or
              (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 10 | 
                 # unmatched validation study dob is within past 10 years
                  (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 10)), 
                         1, 0),
         denomD = ifelse(
           # mother's in-migration is more than 10 years ago
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 5 & 
             # dss dob is within past 10 years
              (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 5 | 
                 # unmatched validation study dob is within past 10 years
                  (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 5)), 
                         1, 0)
         )

dat %>%
  select(rid_m, denomA) %>%
  distinct() %>%
  group_by(denomA) %>%
  summarise(n = n()) # 210 lifelong residents
dat %>%
  select(rid_m, denomC) %>%
  distinct() %>%
  group_by(denomC) %>%
  summarise(n = n()) # 497 mothers
dat %>%
  select(rid_m, denomD) %>%
  distinct() %>%
  group_by(denomD) %>%
  summarise(n = n()) # 597 mothers

# Omission/addition of live births ----------------------------------------

tabLBa <- dat %>%
  filter(denomA == 1 & (pregout_dss == "Live birth" | c223 == "Live birth")) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>%
  select(type, n, per, total) %>%
  mutate(denom = "A")

tabLBc <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth")) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  mutate(denom = "C")
sum(subset(tabLBc, type %in% c("Omission", "Match"))$n)
# ********809+66 = 875
# this should equal the dss bar of live birth events in figure 1
# in that figure, we were just totalling events in dss and fph in past ten years
# in this exercise, we use dss as a reference standard. 
# so the denomC is based off the dss DOB if available, and then fph dob's in the past 10 years otherwise.

tabLBd <- dat %>%
  filter(denomD == 1 & (pregout_dss == "Live birth" | c223 == "Live birth")) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  mutate(denom = "D")

# combine
tabLB <- tabLBa %>%
  bind_rows(tabLBc, tabLBd) %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  pivot_wider(id_cols = c(denom, total), values_from = c(n, per), names_from = type) %>%
  mutate(event = "Live birth",
         total_per = "100.00") %>%
  select(event, denom, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition, total, total_per) 
tabLB 

tabLBLong <- tabLBa %>%
  bind_rows(tabLBc, tabLBd) %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  rename(n_lb = n,
         total_lb = total,
         per_lb = per)


# Omission/addition of deaths ----------------------------------------

# do matches ever disagree on survival status?
dat %>%
  filter(type == "VS_Match") %>%
  filter(cstatus_dss != cstatus_sur) %>%
  nrow() # 2
dat %>%
  filter(type == "VS_Match") %>%
  filter(cstatus_dss != cstatus_sur) %>%
  select(rid_m, uid_c_dss,
         name_c_dss, sex_c_dss, dob_c_dss, pregout_dss, cstatus_dss, dod_c_dss,
         c215, c216, c218, c219, c220, c223, c224, cstatus_sur, dod_c_sur)
# rid_m 3V11000917 uid_c_dss 1291+2 
# names and DOBs are pretty much an exact match
# rid_m 4V76021009 uid_c_dss 1685+2
# names match. DOBs are two years apart. accept the match.

tabDa <- dat %>%
  filter(denomA == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  mutate(denom = "A")


tabDc <- dat %>%
  filter(denomC == 1  & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  mutate(denom = "C")
subset(tabDc, type %in% c("Omission", "Match"))$n
sum(subset(tabDc, type %in% c("Omission", "Match"))$n)
# ********25+328 = 363
# this should equal the dss bar of non-surviving children in figure 1
# in that figure, we were just totalling events in dss and fph in past ten years
# in this exercise, we use dss as a reference standard. 
# so the denomC is based off the dss DOB if available, and then fph dob's in the past 10 years otherwise.

tabDd <- dat %>%
  filter(denomD == 1  & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  mutate(denom = "D")

# combine
tabDth <- tabDa %>%
  bind_rows(tabDc, tabDd) %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  pivot_wider(id_cols = c(denom, total), values_from = c(n, per), names_from = type) %>%
  mutate(event = "Death",
         total_per = "100.00") %>%
  select(event, denom, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition, total, total_per) 
tabDth

tabDthLong <- tabDa %>%
  bind_rows(tabDc, tabDd) %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  rename(n_dth = n,
         total_dth = total,
         per_dth = per)

# Omission/addition of deaths by age ----------------------------------------

tabDageA <- dat %>%
  filter(denomA == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  mutate(cstatus = coalesce(cstatus_agesp_dss, cstatus_agesp_sur)) %>% # dss override sur for type == VS_Match
  group_by(type, cstatus) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, cstatus, n, per, total) %>%
  mutate(denom = "A")

tabDageC <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  mutate(cstatus = coalesce(cstatus_agesp_dss, cstatus_agesp_sur)) %>% # dss override sur for type == VS_Match
  group_by(type, cstatus) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, cstatus, n, per, total) %>%
  mutate(denom = "C")

tabDageD <- dat %>%
  filter(denomD == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  mutate(cstatus = coalesce(cstatus_agesp_dss, cstatus_agesp_sur)) %>% # dss override sur for type == VS_Match
  group_by(type, cstatus) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, cstatus, n, per, total) %>%
  mutate(denom = "D")

# combine
tabDthAge <- tabDageA %>%
  bind_rows(tabDageC, tabDageD) %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  pivot_wider(id_cols = c(denom, cstatus), values_from = c(n, per), names_from = type) %>%
  mutate(event = "Death") %>%
  rename(n_Omission_agesp = n_Omission,
         per_Omission_agesp = per_Omission,
         n_Addition_agesp = n_Addition,
         per_Addition_agesp = per_Addition) %>%
  select(event, denom, cstatus, n_Omission_agesp, per_Omission_agesp, n_Addition_agesp, per_Addition_agesp) 

tabDthAgeLong <- tabDageA %>%
  bind_rows(tabDageC, tabDageD) %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  rename(n_dthAge = n,
         total_dthAge = total,
         per_dthAge = per)

# Table --------------------------------------------------------------

# create totals row
tabLBtotal <- tabLBLong %>%
  select(denom, total_lb) %>%
  distinct() %>%
  rename(n_lb = total_lb) %>%
  mutate(per_lb = "100.00",
         type = "Total")
tabDthtotal <- tabDthLong %>%
  select(denom, total_dth) %>%
  distinct() %>%
  rename(n_dth = total_dth) %>%
  mutate(per_dth = "100.00",
         type = "Total")
# tabDthAgetotal <- tabDthAgeLong %>%
#   select(denom, total_dthAge) %>%
#   distinct() %>%
#   rename(n_dthAge = total_dthAge) %>%
#   mutate(per_dthAge = "100.00",
#          cstatus = "Total")

# combine all long data with totals row
tabAllLong <- tabLBLong %>% 
  bind_rows(tabLBtotal) %>%
  left_join(tabDthLong %>% bind_rows(tabDthtotal), by = c("type", "denom")) %>%
  left_join(tabDthAgeLong, by = c("type", "denom")) %>%
  mutate(rank2 = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    rank1 = case_when(
      denom == "A" ~ 1,
      denom == "C" ~ 2,
      denom == "D" ~ 3,
      TRUE ~ NA),
    cstatus = factor(cstatus, levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "Total"))) %>%
  arrange(rank1, rank2, cstatus)  %>%
  select(denom, type,
         n_lb, per_lb,
         n_dth, per_dth,
         cstatus,
         n_dthAge, per_dthAge) %>%
  filter(denom %in% c("A", "C")) %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")))


ft <- flextable(tabAllLong) %>%
  set_header_labels(values = c("Sample", "Match type", "N", "%", "N", "%",
                               "Age group", "N", "%")) %>%
  add_header_row(values = c(" ","Live birth", "Death"), colwidths = c(2, 2, 5)) %>%
  set_caption(caption = "Match status of live births and deaths between DSS and FPH among pregnancies to women with uninterrupted residency in the DSS in the 10 years preceding the validation study interview") %>%
  merge_v(j = ~ denom + type + n_lb + per_lb + n_dth + per_dth) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabAllLong), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-event-matches.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


