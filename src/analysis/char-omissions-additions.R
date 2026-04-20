################################################################################
#' @description analyse at the event-level, denominator D
#' Assess:
#' characteristics of omissions of live births and deaths
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
library(purrr)
library(officer)
library(flextable)
library(modelsummary)
library(marginaleffects)
library(stringr)
library(lme4)
#' Inputs
overall <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Omissions
# Subsample: (A) all-women
# Denominator: deaths in DSS
## Additions
# Subsample: (C) recent-pregnancies
# Denominator: deaths in FPH
dat <- overall %>%
  mutate(subsampA = 1,
         subsampC = ifelse(
           # mother's in-migration is more than 15 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 15 & 
             # dss dob is within past 15 years or
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 15 | 
                # unmatched validation study dob is within past 15 years
                (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 15)), 
           1, 0),
         # deaths in dss
         eventDth_dss = ifelse(cstatus_dss == "Died", 1, 0),
         # deaths in survey
         eventDth_sur = ifelse(cstatus_sur == "Died", 1, 0),
         denomA = ifelse(subsampA == 1 & eventDth_dss == 1, 1, 0),
         denomC = ifelse(subsampC == 1 & eventDth_sur == 1, 1, 0)
  )

# omissions ---------------------------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", "observer_sur",
  "cstatus_agesp_comb", "cstrata_ac"
)

datDth <- dat %>%
  filter(denomA == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>% 
  select(type, all_of(vars)) 

# create counts and percentages
tabDth <- datDth %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(variable, value, type) %>%
  group_by(variable, value) %>%
  mutate(per = sprintf("%.2f", round(n / sum(n)*100, 2))) %>% 
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  mutate(n_Omission = ifelse(is.na(n_Omission), 0, n_Omission),
         per_Omission = ifelse(is.na(per_Omission), 0, per_Omission)) %>%
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission) 

# add total row
tabDthtot <- dat %>%
  filter(denomA == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(total = "") %>%
  select(type, total) %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(variable, value, type) %>%
  group_by(variable, value) %>%
  mutate(per = sprintf("%.2f", round(n / sum(n)*100, 2))) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission) 
tabDth <- tabDth %>%
  bind_rows(tabDthtot)

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  mydat <- datDth
  
  tab <- table(mydat$type, mydat[[v]])
  test <- chisq.test(tab)
  
  tibble(
    variable = v,
    statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value
  )
})
tabChi <- tabChi %>%
  mutate(pvalcat = sprintf("%.2f", round(p_value, 2))) %>%
  mutate(pvalcat = ifelse(p_value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat)) 
  
# merge on chi-squared
tabDth <- tabDth %>%
  left_join(tabChi %>% select(variable, pvalcat))

# order variables
# household level
v_hh <- c("hhsizecat_sur", "hhassets_sur", "observer_sur")
# women-level
v_wom <- c("magecat2_int", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "deathrecency_cat", "cstatus_agesp_comb", "cstrata_ac")
v_all <- c(v_hh, v_wom, v_ch, "total")
length(v_all) == length(unique(tabDth$variable)) # TRUE
df_varrank <- data.frame(variable = v_all,
                         variablerank = 1:length(v_all))


# order values
tabDth <- tabDth %>%
  left_join(df_varrank, by = "variable") %>%
  mutate(valuerank = case_when(
    variable == "birthrecency_cat" & value == "0-4" ~ 1,
    variable == "birthrecency_cat" & value == "5-9" ~ 2,
    variable == "birthrecency_cat" & value == "10-14" ~ 3,
    variable == "birthrecency_cat" & value == "15+" ~ 4,
    variable == "deathrecency_cat" & value == "0-4" ~ 1,
    variable == "deathrecency_cat" & value == "5-9" ~ 2,
    variable == "deathrecency_cat" & value == "10-14" ~ 3,
    variable == "deathrecency_cat" & value == "15+" ~ 4,
    variable == "hhsizecat_sur" & value == "Small" ~ 1,
    variable == "hhsizecat_sur" & value == "Medium" ~ 2,
    variable == "hhsizecat_sur" & value == "Large" ~ 3,
    variable == "observer_sur" & value == "Missing" ~ 1,
    variable == "observer_sur" & value == "No one" ~ 1,
    variable == "observer_sur" & value == "Partial" ~ 2,
    variable == "observer_sur" & value == "Full time" ~ 3,
    variable == "cstatus_agesp_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_agesp_comb" & value == "Neonatal" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Postneonatal" ~ 3,
    variable == "cstatus_agesp_comb" & value == "1-4" ~ 4,
    variable == "cstatus_agesp_comb" & value == "5-9" ~ 5,
    variable == "cstatus_agesp_comb" & value == "10+" ~ 6,
    variable == "cstrata_ac" & value == "Surviving" ~ 1,
    variable == "cstrata_ac" & value == "Neonatal (birth asphyxia)" ~ 2,
    variable == "cstrata_ac" & value == "Neonatal (other)" ~ 3,
    variable == "cstrata_ac" & value == "Neonatal (unknown)" ~ 4,
    variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 5,
    variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 6,
    variable == "cstrata_ac" & value == "Postneonatal (unknown)" ~ 7,
    variable == "cstrata_ac" & value == "1-4 year (drowning)" ~ 8,
    variable == "cstrata_ac" & value == "1-4 year (other)" ~ 9,
    variable == "cstrata_ac" & value == "1-4 year (unknown)" ~ 9,
    variable == "cstrata_ac" & value == "5-9 year" ~ 10,
    variable == "cstrata_ac" & value == "10+" ~ 11,
    TRUE ~ 1
  )) %>%
  arrange(variablerank, valuerank)

# clean up variable names
tabDth <- tabDth %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "hhassets_sur"  ~ "Household wealth quintile",
    variable == "observer_sur"  ~ "Interview observed by others",
    variable == "magecat2_int" ~ "Mother age",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recency (years)",
    variable == "deathrecency_cat"   ~ "Death recency (years)",
    variable == "cstatus_agesp_comb"   ~ "Age of death",
    variable == "cstrata_ac"   ~ "Cause of death",
    variable == "total"   ~ "Total",
  )) %>%
  select(-c(variablerank, valuerank)) 
tabDth$n_Match[is.na(tabDth$n_Match)] <- 0
tabDth$n_Omission[is.na(tabDth$n_Omission)] <- 0
#tabDth$n_Addition[is.na(tabDth$n_Addition)] <- 0
tabDth$per_Match[is.na(tabDth$per_Match)] <- "0.00"
tabDth$per_Omission[is.na(tabDth$per_Omission)] <- "0.00"
#tabDth$per_Addition[is.na(tabDth$per_Addition)] <- "0.00"

tabDthO <- tabDth

ft <- tabDthO %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", ""), colwidths = c(2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of DSS deaths by reporting in FPH (ie, matches and omissions)") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabDth), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

# output_path <- here::here("gen/figures", "table-dths-omissions-char.docx")
# print(doc, target = output_path)
# cat("Saved to:", output_path, "\n")


# additions ---------------------------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", "observer_sur",
  "cstatus_agesp_comb"
)

datDth <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, addition from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>% 
  select(type, all_of(vars)) %>%
  mutate(birthrecency_cat = factor(birthrecency_cat, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(deathrecency_cat = factor(deathrecency_cat, levels = c("0-4", "5-9", "10-14"))) 
# refactor for chi-squared

# create counts and percentages
tabDth <- datDth %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(variable, value, type) %>%
  group_by(variable, value) %>%
  mutate(per = sprintf("%.2f", round(n / sum(n)*100, 2))) %>% 
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  mutate(n_Addition = ifelse(is.na(n_Addition), 0, n_Addition),
         per_Addition = ifelse(is.na(per_Addition), 0, per_Addition)) %>%
  select(variable, value, n_Match, per_Match, n_Addition, per_Addition) 

# add total row
tabDthtot <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, addition from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(total = "") %>%
  select(type, total) %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(variable, value, type) %>%
  group_by(variable, value) %>%
  mutate(per = sprintf("%.2f", round(n / sum(n)*100, 2))) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  select(variable, value, n_Match, per_Match, n_Addition, per_Addition) 
tabDth <- tabDth %>%
  bind_rows(tabDthtot)

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  mydat <- datDth
  
  tab <- table(mydat$type, mydat[[v]])
  test <- chisq.test(tab)
  
  tibble(
    variable = v,
    statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value
  )
})
tabChi <- tabChi %>%
  mutate(pvalcat = sprintf("%.2f", round(p_value, 2))) %>%
  mutate(pvalcat = ifelse(p_value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat)) 

# merge on chi-squared
tabDth <- tabDth %>%
  left_join(tabChi %>% select(variable, pvalcat))

# order variables
# household level
v_hh <- c("hhsizecat_sur", "hhassets_sur", "observer_sur")
# women-level
v_wom <- c("magecat2_int", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "deathrecency_cat", "cstatus_agesp_comb") #  "cstatus_comb",
v_all <- c(v_hh, v_wom, v_ch, "total")
length(v_all) == length(unique(tabDth$variable)) # TRUE
df_varrank <- data.frame(variable = v_all,
                         variablerank = 1:length(v_all))


# order values
tabDth <- tabDth %>%
  left_join(df_varrank, by = "variable") %>%
  mutate(valuerank = case_when(
    variable == "birthrecency_cat" & value == "0-4" ~ 1,
    variable == "birthrecency_cat" & value == "5-9" ~ 2,
    variable == "birthrecency_cat" & value == "10-14" ~ 3,
    variable == "birthrecency_cat" & value == "15+" ~ 4,
    variable == "deathrecency_cat" & value == "0-4" ~ 1,
    variable == "deathrecency_cat" & value == "5-9" ~ 2,
    variable == "deathrecency_cat" & value == "10-14" ~ 3,
    variable == "deathrecency_cat" & value == "15+" ~ 4,
    variable == "hhsizecat_sur" & value == "Small" ~ 1,
    variable == "hhsizecat_sur" & value == "Medium" ~ 2,
    variable == "hhsizecat_sur" & value == "Large" ~ 3,
    variable == "observer_sur" & value == "Missing" ~ 1,
    variable == "observer_sur" & value == "No one" ~ 1,
    variable == "observer_sur" & value == "Partial" ~ 2,
    variable == "observer_sur" & value == "Full time" ~ 3,
    variable == "cstatus_agesp_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_agesp_comb" & value == "Neonatal" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Postneonatal" ~ 3,
    variable == "cstatus_agesp_comb" & value == "1-4" ~ 4,
    variable == "cstatus_agesp_comb" & value == "5-9" ~ 5,
    variable == "cstatus_agesp_comb" & value == "10+" ~ 6,
    TRUE ~ 1
  )) %>%
  arrange(variablerank, valuerank)

# clean up variable names
tabDth <- tabDth %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "hhassets_sur"  ~ "Household wealth quintile",
    variable == "observer_sur"  ~ "Interview observed by others",
    variable == "magecat2_int" ~ "Mother age",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recency (years)",
    variable == "deathrecency_cat"   ~ "Death recency (years)",
    variable == "cstatus_agesp_comb"   ~ "Age of death",
    variable == "total"   ~ "Total",
  )) %>%
  select(-c(variablerank, valuerank)) 
tabDth$n_Match[is.na(tabDth$n_Match)] <- 0
tabDth$n_Addition[is.na(tabDth$n_Addition)] <- 0
tabDth$per_Match[is.na(tabDth$per_Match)] <- "0.00"
tabDth$per_Addition[is.na(tabDth$per_Addition)] <- "0.00"

tabDthA <- tabDth

ft <- tabDthA %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Additions", ""), colwidths = c(2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of FPH deaths by reporting in DSS (ie, matches and additions)") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabDth), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

# output_path <- here::here("gen/figures", "table-dths-additions-char.docx")
# print(doc, target = output_path)
# cat("Saved to:", output_path, "\n")


# both --------------------------------------------------------------------

tabDthComb <- tabDthO %>%
  left_join(tabDthA, by = c("variable", "value"), suffix = c("_o", "_a"))

ft <- tabDthComb %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "p-value", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", " ", "Match", "Addition", " "), 
                 colwidths = c(2, 2, 2, 1, 2, 2, 1)) %>%
  set_caption(caption = "") %>%
  merge_v(j = ~ variable + pvalcat_o + pvalcat_a) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabDthComb), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-dths-additionsOmissions-char.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")
