################################################################################
#' @description analyse additions of live births and deaths
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

# Subsamples: (A) all-women, (B)lifelong-resident, (C) recent-pregnancies
# Denominator: deaths in DSS
# Since this is event level agreement, we use DSS as the reference standard.
# Matched DSS and FPH events are included conditional on the DSS DOB
# Unmatched FPH use FPH DOB.
dat <- overall %>%
  mutate(subsampA = 1,
         subsampB = ifelse(dob_m_dss == doi_m_dss, 1, 0),
         subsampC = ifelse(
           # mother's in-migration is more than 15 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 15 & 
             # dss dob is within past 15 years or
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 15 | 
                # unmatched validation study dob is within past 10 years
                (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 15)), 
           1, 0),
         # deaths in dss
         eventDth_dss = ifelse(cstatus_dss == "Died", 1, 0),
         # deaths in each source
         eventDth = ifelse((!is.na(cstatus_dss) & cstatus_dss == "Died") |
                            (!is.na(cstatus_sur) & cstatus_sur == "Died"), 1, 0),
         denomA = ifelse(subsampA == 1 & eventDth_dss == 1, 1, 0),
         denomB = ifelse(subsampB == 1 & eventDth == 1, 1, 0),
         denomC = ifelse(subsampC == 1 & eventDth == 1, 1, 0)
  )

dat %>%
  select(rid_m, denomC) %>%
  distinct() %>%
  group_by(denomC) %>%
  summarise(n = n()) # 356 women with residency in prev 15 years with live births in dss or fph during that time
# Used to be 473 women with residency in prev 10 years with live births in dss or fph during that time

# Omissions ---------------------------------------------------------------

dat %>%
  select(rid_m, denomA) %>%
  distinct() %>%
  group_by(denomA) %>%
  summarise(n = n()) # 835 mothers

# omissions of deaths
tabD <- dat %>%
  filter(denomA == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omissions of age-specific deaths
tabDage <- dat %>%
  filter(denomA == 1) %>%
  group_by(type, cstatus_agesp_dss) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_dss, n, per, total) 

# combine
tabComb <- tabD %>% 
  rename(n_dth = n, per_dth = per) %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_dss = factor(cstatus_agesp_dss,
                               levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+", "Total"))) %>%
  arrange(rank, cstatus_agesp_dss)  %>%
  select(type,n_dth, per_dth, cstatus_agesp_dss, n, per) 

tabComb1 <- tabComb %>%
  mutate(subsample = "All-women") %>%
  select(subsample, everything())

# Additions and omissions: lifelong resident -------------------------------------------------

# additions or omission of deaths
tabD <- dat %>%
  filter(denomB == 1) %>%
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
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omission or additions of age-specific deaths
tabDage <- dat %>%
  filter(denomB == 1) %>%
  group_by(type, cstatus_agesp_comb) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_comb, n, per, total) 

# combine
tabComb <- tabD %>% 
  rename(n_dth = n, per_dth = per) %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_comb = factor(cstatus_agesp_comb,
                               levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "Total"))) %>%
  arrange(rank, cstatus_agesp_comb)  %>%
  select(type, n_dth, per_dth, cstatus_agesp_comb, n, per) 

tabComb2 <- tabComb %>%
  mutate(subsample = "Lifelong-resident") %>%
  select(subsample, everything())


# Additions and omissions: recent pregnancies -------------------------------------------------

# additions or omission of deaths
tabD <- dat %>%
  filter(denomC == 1) %>%
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
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omission or additions of age-specific deaths
tabDage <- dat %>%
  filter(denomC == 1) %>%
  group_by(type, cstatus_agesp_comb) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_comb, n, per, total) 

# combine
tabComb <- tabD %>% 
  rename(n_dth = n, per_dth = per) %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_comb = factor(cstatus_agesp_comb,
                                levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "Total"))) %>%
  arrange(rank, cstatus_agesp_comb)  %>%
  select(type, n_dth, per_dth, cstatus_agesp_comb, n, per) 

tabComb3 <- tabComb %>%
  mutate(subsample = "Recent-pregnancies") %>%
  select(subsample, everything())


# Flextable ---------------------------------------------------------------

tabComb3 <- tabComb1 %>%
  rename(cstatus_agesp_comb = cstatus_agesp_dss) %>%
  bind_rows(tabComb2) %>%
  bind_rows(tabComb3) %>%
  mutate(per_dth = sprintf("%.2f", round(per_dth, 2)),
         per = sprintf("%.2f", round(per, 2))) %>%
  mutate(per = ifelse(per == "NA", "", per))

ft <- flextable(tabComb3) %>%
  set_header_labels(values = c("Sample", "Event agreement", "N", "%","Age group", "N", "%")) %>%
  add_header_row(values = c(" ", "Death"), colwidths = c(2, 5)) %>%
  set_caption(caption = "Omissions and additions of deaths in FPH among all-women, lifelong-resident, and recent-pregnancies subsamples.") %>%
  merge_v(j = ~ subsample + type + n_dth + per_dth) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabComb), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-additionsOmissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

