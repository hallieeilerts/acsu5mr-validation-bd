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

# Assign denominator: 
# B - Mothers: lifelong residents
# C - Mother+pregnancies: 10 yr residents
# Since this is event level agreement, we use DSS as the reference standard.
# DSS and FPH events are included conditional on the DSS DOB, and only FPH DOB if unmatched.
# Hence there is no need for denomC_dss and denomC_sur columns. There is just one denomC.
dat <- overall %>%
  mutate(denomB = ifelse(dob_m_dss == doi_m_dss, 1, 0),
         denomC = ifelse(
           # mother's in-migration is more than 10 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 10 & 
             # dss dob is within past 10 years or
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 10 | 
                # unmatched validation study dob is within past 10 years
                (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 10)), 
           1, 0),
         denomE = ifelse( (!is.na(c223) & c223 == "Live birth")  |
                            (!is.na(pregout_dss) & pregout_dss == "Live birth" & type == "VS_Match"), 1, 0),
         denomBE = ifelse(denomB == 1 & denomE == 1, 1, 0),
         denomCE = ifelse(denomC == 1 & denomE == 1, 1, 0)
  )

dat %>%
  select(rid_m, denomBE) %>%
  distinct() %>%
  group_by(denomBE) %>%
  summarise(n = n()) # 209 lifelong residents with pregnancies in denom E
dat %>%
  select(rid_m, denomCE) %>%
  distinct() %>%
  group_by(denomCE) %>%
  summarise(n = n()) # 472 women with residency in prev 10 years with pregnancies in denom E


# Additions ---------------------------------------------------------------

# addition of live births
tabLB <- dat %>%
  filter(denomBE == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  ))  %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# additions of deaths
tabD <- dat %>%
  filter(denomBE == 1 & cstatus_sur == "Died") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )


# additions of age-specific deaths
tabDage <- dat %>%
  filter(denomBE == 1 & cstatus_sur == "Died") %>%
  group_by(type, cstatus_agesp_sur) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_sur, n, per, total) 

# combine
tabComb <- tabLB %>% 
  rename(n_lb = n,
         per_lb = per) %>%
  left_join(tabD %>% rename(n_dth = n, per_dth = per), by = "type") %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Addition" ~ 2,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_sur = factor(cstatus_agesp_sur,
                               levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+", "Total"))) %>%
  arrange(rank, cstatus_agesp_sur)  %>%
  select(type, n_lb, per_lb, n_dth, per_dth, cstatus_agesp_sur, n, per) %>%
  mutate(per_lb = sprintf("%.2f", round(per_lb, 2)),
         per_dth = sprintf("%.2f", round(per_dth, 2)),
         per = sprintf("%.2f", round(per, 2))) %>%
  mutate(per = ifelse(per == "NA", "", per))


ft <- flextable(tabComb) %>%
  set_header_labels(values = c("Match type", "N", "%", "N", "%","Age group", "N", "%")) %>%
  add_header_row(values = c(" ","Live birth", "Death"), colwidths = c(1, 2, 5)) %>%
  set_caption(caption = "Reporting of FPH live births and deaths in DSS (ie, matches and additions). Sample limited to live births of women with lifelong residency in DSS.") %>%
  merge_v(j = ~ type + n_lb + per_lb + n_dth + per_dth) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabComb), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-event-additions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Additions and omissions -------------------------------------------------

# addition or omissions of live births
tabLB <- dat %>%
  filter(denomB == 1 &
           ((!is.na(pregout_dss) & pregout_dss== "Live birth") | 
              (is.na(pregout_dss) & c223 == "Live birth"))) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    type == "HDSS_NoMatch" ~ "Omission",
    TRUE ~ NA
  ))  %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# additions or omission of deaths
tabD <- dat %>%
  filter(denomB == 1 &
           ((!is.na(pregout_dss) & pregout_dss== "Live birth") | 
              (is.na(pregout_dss) & c223 == "Live birth")) &
           ((!is.na(cstatus_dss) & cstatus_dss == "Died") | 
            (is.na(cstatus_dss) & cstatus_sur == "Died"))) %>%
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
  filter(denomB == 1 &
           ((!is.na(pregout_dss) & pregout_dss== "Live birth") | 
              (is.na(pregout_dss) & c223 == "Live birth")) &
           ((!is.na(cstatus_dss) & cstatus_dss == "Died") | 
              (is.na(cstatus_dss) & cstatus_sur == "Died"))) %>%
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
tabComb <- tabLB %>% 
  rename(n_lb = n,
         per_lb = per) %>%
  left_join(tabD %>% rename(n_dth = n, per_dth = per), by = "type") %>%
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
  select(type, n_lb, per_lb, n_dth, per_dth, cstatus_agesp_comb, n, per) %>%
  mutate(per_lb = sprintf("%.2f", round(per_lb, 2)),
         per_dth = sprintf("%.2f", round(per_dth, 2)),
         per = sprintf("%.2f", round(per, 2))) %>%
  mutate(per = ifelse(per == "NA", "", per))


ft <- flextable(tabComb) %>%
  set_header_labels(values = c("Match type", "N", "%", "N", "%","Age group", "N", "%")) %>%
  add_header_row(values = c(" ","Live birth", "Death"), colwidths = c(1, 2, 5)) %>%
  set_caption(caption = "Omissions and additions in FPH of live births and deaths. Sample limited to live births of women with lifelong residency in DSS.") %>%
  merge_v(j = ~ type + n_lb + per_lb + n_dth + per_dth) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabComb), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-event-additionsOmissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

