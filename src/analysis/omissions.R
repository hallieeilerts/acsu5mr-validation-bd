################################################################################
#' @description analyse omissions of live births and deaths
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
# D - Live births in DSS, only matched live births in FPH
dat <- overall %>%
  mutate(denomD = ifelse((!is.na(pregout_dss) & pregout_dss == "Live birth")  |
                           (!is.na(c223) & c223 == "Live birth" & type == "VS_Match"), 1, 0)
  )

dat %>%
  select(rid_m, denomD) %>%
  distinct() %>%
  group_by(denomD) %>%
  summarise(n = n()) # 835 mothers

# omissions of live births
tabLB <- dat %>%
  filter(denomD == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omissions of deaths
tabD <- dat %>%
  filter(denomD == 1 & cstatus_dss == "Died") %>%
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
  filter(denomD == 1 & cstatus_dss == "Died") %>%
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

#


# combine
tabComb <- tabLB %>% 
  rename(n_lb = n,
         per_lb = per) %>%
  left_join(tabD %>% rename(n_dth = n, per_dth = per), by = "type") %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_dss = factor(cstatus_agesp_dss,
                               levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+", "Total"))) %>%
  arrange(rank, cstatus_agesp_dss)  %>%
  select(type, n_lb, per_lb, n_dth, per_dth, cstatus_agesp_dss, n, per) %>%
  mutate(per_lb = sprintf("%.2f", round(per_lb, 2)),
         per_dth = sprintf("%.2f", round(per_dth, 2)),
         per = sprintf("%.2f", round(per, 2))) %>%
  mutate(per = ifelse(per == "NA", "", per))


ft <- flextable(tabComb) %>%
  set_header_labels(values = c("Match type", "N", "%", "N", "%","Age group", "N", "%")) %>%
  add_header_row(values = c(" ","Live birth", "Death"), colwidths = c(1, 2, 5)) %>%
  set_caption(caption = "Reporting of DSS live births and deaths in FPH (ie, matches and omissions)") %>%
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

output_path <- here::here("gen/figures", "table-event-omissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

