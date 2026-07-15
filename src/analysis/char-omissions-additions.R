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
overall <- readRDS("./gen/augment/overallName-recode.rds")
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
         denomC = ifelse(subsampC == 1 & (eventDth_sur == 1 | eventDth_dss == 1), 1, 0)
  )

# omissions ---------------------------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat2_int", "meducat_sur",
  "hhsizecat_sur", "hhassets_sur", 
  "intinterupt_sur", "observer_sur", "intcoop_sur", "otherwork_sur",
  "breakdown_sur", "support_sur",
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
         per_Omission = ifelse(is.na(per_Omission), "0.00", per_Omission)) %>%
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
  mydat <- mydat[!(mydat[[v]] == "Missing"),]
  
  if(v == "cstrata_ac"){
    mydat <- mydat[!(mydat[[v]] == "5-9 year"),]
    mydat <- mydat[!(mydat[[v]] == "10+"),]
  }
  
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
v_hh <- c("hhsizecat_sur", "hhassets_sur", "observer_sur", "intinterupt_sur", "intcoop_sur", "breakdown_sur",
          "otherwork_sur", "support_sur")
# women-level
v_wom <- c("magecat2_int", "meducat_sur", "paritymaxcat_comb")
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
    variable == "meducat_sur" & value == "Missing" ~ 1,
    variable == "meducat_sur" & value == "None" ~ 2,
    variable == "meducat_sur" & value == "Primary" ~ 3,
    variable == "meducat_sur" & value == "Secondary" ~ 4,
    variable == "meducat_sur" & value == "Higher secondary" ~ 5,
    variable == "observer_sur" & value == "Missing" ~ 1,
    variable == "observer_sur" & value == "No one" ~ 1,
    variable == "observer_sur" & value == "Partial" ~ 2,
    variable == "observer_sur" & value == "Full time" ~ 3,
    variable == "intinterupt_sur" & value == "Not at all" ~ 1,
    variable == "intinterupt_sur" & value == "Partially" ~ 2,
    variable == "intinterupt_sur" & value == "Fully" ~ 3,
    variable == "intcoop_sur" & value == "Normal" ~ 1,
    variable == "intcoop_sur" & value == "Good" ~ 2,
    variable == "intcoop_sur" & value == "Very good" ~ 3,
    variable == "breakdown_sur" & value == "Missing" ~ 1,
    variable == "breakdown_sur" & value == "None" ~ 2,
    variable == "breakdown_sur" & value == "Mild" ~ 3,
    variable == "breakdown_sur" & value == "Moderate" ~ 4,
    variable == "breakdown_sur" & value == "Severe" ~ 5,
    variable == "otherwork_sur" & value == "Missing" ~ 1,
    variable == "otherwork_sur" & value == "No" ~ 2,
    variable == "otherwork_sur" & value == "Yes" ~ 3,
    variable == "support_sur" & value == "Missing" ~ 1,
    variable == "support_sur" & value == "No" ~ 2,
    variable == "support_sur" & value == "Yes" ~ 3,
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

# Remove strata that don't have cause
tabDth <- tabDth %>%
  filter(!(variable == "cstrata_ac" & value %in% c("5-9 year", "10+")))

# clean up variable names
tabDth <- tabDth %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "hhassets_sur"  ~ "Household wealth quintile",
    variable == "observer_sur"  ~ "Interview observed by others",
    variable == "intinterupt_sur"  ~ "Interview interrupted by others",
    variable == "intcoop_sur"  ~ "Respondent cooperation",
    variable == "breakdown_sur"  ~ "Respondent emotional breakdown",
    variable == "otherwork_sur"  ~ "Respondent conducting other work during interview",
    variable == "support_sur"  ~ "Respondent received support from others",
    variable == "magecat2_int" ~ "Mother age",
    variable == "meducat_sur" ~ "Mother education",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recall period (years)",
    variable == "deathrecency_cat"   ~ "Death recall period (years)",
    variable == "cstatus_agesp_comb"   ~ "Age-at-death",
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
# remove matches columns
tabDthO <- tabDthO[,c("variable","value","n_Omission", "per_Omission", "pvalcat")]

ft <- tabDthO %>%
  flextable() %>%
  #set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "p-value")) %>%
  #add_header_row(values = c(" ","Match", "Omission", ""), colwidths = c(2, 2, 2, 1)) %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ", "Omission", ""), colwidths = c(2, 2, 1)) %>%
  set_caption(caption = "Characteristics of DSS deaths by reporting in FPH (ie, matches and omissions)") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabDthO), part = "all") %>%
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
  "magecat2_int", "meducat_sur",
  "hhsizecat_sur", "hhassets_sur", 
  "intinterupt_sur", "observer_sur", "intcoop_sur", "otherwork_sur",
  "breakdown_sur", "support_sur",
  "cstatus_agesp_comb"
)


datDth <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, addition from validation study
    type == "VS_Match" ~ "MatchOrOmission",
    type == "HDSS_NoMatch" ~ "MatchOrOmission",
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
         per_Addition = ifelse(is.na(per_Addition), "0.00", per_Addition)) %>%
  select(variable, value, n_MatchOrOmission, per_MatchOrOmission, n_Addition, per_Addition) 

# add total row
tabDthtot <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, addition from validation study
    type == "VS_Match" ~ "MatchOrOmission",
    type == "HDSS_NoMatch" ~ "MatchOrOmission",
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
  select(variable, value, n_MatchOrOmission, per_MatchOrOmission, n_Addition, per_Addition) 
tabDth <- tabDth %>%
  bind_rows(tabDthtot)

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  mydat <- datDth
  mydat <- mydat[!(mydat[[v]] == "Missing"),]
  
  if(v == "cstrata_ac"){
    mydat <- mydat[!(mydat[[v]] == "5-9 year"),]
    mydat <- mydat[!(mydat[[v]] == "10+"),]
  }
  
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
v_hh <- c("hhsizecat_sur", "hhassets_sur", "observer_sur", "intinterupt_sur", "intcoop_sur", "breakdown_sur",
          "otherwork_sur", "support_sur")
# women-level
v_wom <- c("magecat2_int", "meducat_sur", "paritymaxcat_comb")
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
    variable == "meducat_sur" & value == "Missing" ~ 1,
    variable == "meducat_sur" & value == "None" ~ 2,
    variable == "meducat_sur" & value == "Primary" ~ 3,
    variable == "meducat_sur" & value == "Secondary" ~ 4,
    variable == "meducat_sur" & value == "Higher secondary" ~ 5,
    variable == "observer_sur" & value == "Missing" ~ 1,
    variable == "observer_sur" & value == "No one" ~ 1,
    variable == "observer_sur" & value == "Partial" ~ 2,
    variable == "observer_sur" & value == "Full time" ~ 3,
    variable == "intinterupt_sur" & value == "Not at all" ~ 1,
    variable == "intinterupt_sur" & value == "Partially" ~ 2,
    variable == "intinterupt_sur" & value == "Fully" ~ 3,
    variable == "intcoop_sur" & value == "Normal" ~ 1,
    variable == "intcoop_sur" & value == "Good" ~ 2,
    variable == "intcoop_sur" & value == "Very good" ~ 3,
    variable == "breakdown_sur" & value == "Missing" ~ 1,
    variable == "breakdown_sur" & value == "None" ~ 2,
    variable == "breakdown_sur" & value == "Mild" ~ 3,
    variable == "breakdown_sur" & value == "Moderate" ~ 4,
    variable == "breakdown_sur" & value == "Severe" ~ 5,
    variable == "otherwork_sur" & value == "Missing" ~ 1,
    variable == "otherwork_sur" & value == "No" ~ 2,
    variable == "otherwork_sur" & value == "Yes" ~ 3,
    variable == "support_sur" & value == "Missing" ~ 1,
    variable == "support_sur" & value == "No" ~ 2,
    variable == "support_sur" & value == "Yes" ~ 3,
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
    variable == "intinterupt_sur"  ~ "Interview interrupted by others",
    variable == "intcoop_sur"  ~ "Respondent cooperation",
    variable == "breakdown_sur"  ~ "Respondent emotional breakdown",
    variable == "otherwork_sur"  ~ "Respondent conducting other work during interview",
    variable == "support_sur"  ~ "Respondent received support from others",
    variable == "magecat2_int" ~ "Mother age",
    variable == "meducat_sur" ~ "Mother education",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recall period (years)",
    variable == "deathrecency_cat"   ~ "Death recall period (years)",
    variable == "cstatus_agesp_comb"   ~ "Age-at-death",
    variable == "total"   ~ "Total",
  )) %>%
  select(-c(variablerank, valuerank)) 
tabDth$n_MatchOrOmission[is.na(tabDth$n_MatchOrOmission)] <- 0
tabDth$n_Addition[is.na(tabDth$n_Addition)] <- 0
tabDth$per_MatchOrOmission[is.na(tabDth$per_MatchOrOmission)] <- "0.00"
tabDth$per_Addition[is.na(tabDth$per_Addition)] <- "0.00"

tabDthA <- tabDth
# remove matches and omissions columns
tabDthA <- tabDthA[,c("variable","value","n_Addition", "per_Addition", "pvalcat")]


ft <- tabDthA %>%
  flextable() %>%
  # set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "p-value")) %>%
  # add_header_row(values = c(" ","Match", "Additions", ""), colwidths = c(2, 2, 2, 1)) %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ", "Additions", ""), colwidths = c(2, 2, 1)) %>%
  set_caption(caption = "Characteristics of FPH deaths by reporting in DSS (ie, matches and additions)") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabDthA), part = "all") %>%
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

# Add zeroes for any missing values or percentages
tabDthComb$per_Addition[tabDthComb$variable != "Cause of death" &
                      is.na(tabDthComb$n_Addition)] <- "0.00"
tabDthComb$n_Addition[tabDthComb$variable != "Cause of death" &
                        is.na(tabDthComb$n_Addition)] <- 0

# Remove those with zero omissions that aren't a focus
tabDthComb <- tabDthComb %>%
  filter(!(variable == "Age-at-death" & value == "10+" & n_Omission == 0)) %>%
  filter(!(value == "Missing" & n_Omission == 0 & n_Addition == 0))

# check if any zeros remaining
tabDthComb %>%
  filter(n_Omission == 0 & n_Addition == 0)
# fully interrupted interview. this is ok because it is part of a scaled response

# Remove variable for social support
# not sure what it means and wasn't in original questionnaire
tabDthComb <- tabDthComb %>%
  filter(!(variable == "Respondent received support from others"))

# add parentehsis to %
tabDthComb$per_Omission <- paste0("(", tabDthComb$per_Omission, ")")
tabDthComb$per_Addition <- paste0("(", tabDthComb$per_Addition, ")")

# remove household wealth quintile
tabDthComb <- tabDthComb %>% filter(variable != "Household wealth quintile")

ft <- tabDthComb %>%
  flextable() %>%
  # set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "p-value", "N", "%", "N", "%", "p-value")) %>%
  # add_header_row(values = c(" ","Match", "Omission", " ", "Match", "Addition", " "), 
  #                colwidths = c(2, 2, 2, 1, 2, 2, 1)) %>%
  # add_header_row(values = c(" ","All-women", "Recent-pregnancies"), 
  #                colwidths = c(2, 5, 5)) %>%
  set_header_labels(values = c("Variable", "Value", "N", "(%)", "p-value", "N", "(%)", "p-value")) %>%
  add_header_row(values = c(" ","Omission", " ", "Addition", " "), 
                 colwidths = c(2, 2, 1, 2, 1)) %>%
  add_header_row(values = c(" ","All-women", "Recent-births"), 
                 colwidths = c(2, 3, 3)) %>%
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


# PAA figure: omissions by COD --------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", 
  "intinterupt_sur", "observer_sur", "intcoop_sur", "breakdown_sur",
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
figDat <- datDth %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(variable, value, type) %>%
  group_by(variable, value) %>%
  mutate(per = n / sum(n)*100) %>% 
  pivot_longer(
    cols = c(n, per),
    names_to = "name",
    values_to = "metric_value"
  ) %>%
  mutate(name = ifelse(name == "n", "N", "%")) %>%
  mutate(name = factor(name, levels = c("N", "%"))) 
totals <- figDat %>%
  filter(name == "N") %>%
  group_by(variable, value) %>%
  summarise(total = sum(metric_value), .groups = "drop") %>%
  mutate(name = "N") %>%
  mutate(name = factor(name, levels = c("N", "%")))

# Limit to cstrata_ac
figDat1 <- figDat %>%
  filter(variable == "cstrata_ac") %>%
  mutate(value = factor(value, levels = c(
    "10+", "5-9 year","1-4 year (other)","1-4 year (drowning)", "Postneonatal (other)",
    "Postneonatal (RI+con)", "Neonatal (other)","Neonatal (birth asphyxia)"),
    labels = c("10+", "5-9 year","1-4 year - other","1-4 year - drowning", "Postneonatal - other",
               "Postneonatal - RI+con", "Neonatal - other","Neonatal - birth asphyxia")
  ))
totals1 <- totals %>%
  filter(variable == "cstrata_ac") %>%
  mutate(value = factor(value, levels = c(
    "10+", "5-9 year","1-4 year (other)","1-4 year (drowning)", "Postneonatal (other)",
    "Postneonatal (RI+con)", "Neonatal (other)","Neonatal (birth asphyxia)"),
    labels = c("10+", "5-9 year","1-4 year - other","1-4 year - drowning", "Postneonatal - other",
               "Postneonatal - RI+con", "Neonatal - other","Neonatal - birth asphyxia")))

myplot <- figDat1 %>%
  mutate(type = factor(type, levels = c("Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", metric_value, round(metric_value, 1))) %>%
  ggplot() +
  geom_bar(aes(x = value, y = metric_value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals1,
    aes(x = value, y = total, label = total),
    hjust = -0.05,
    size = 4
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = value, y = metric_value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, nrow = 2, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.45, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave("./gen/figures/paa/matching-aw-cod.png", myplot, width = 6, height = 6, dpi = 500)

figDat1 <- figDat %>%
  filter(variable == "deathrecency_cat") %>%
  mutate(value = factor(value, levels = c("15+", "10-14","5-9", "0-4")))
totals1 <- totals %>%
  filter(variable == "deathrecency_cat") %>%
  mutate(value = factor(value, levels = c("15+", "10-14","5-9", "0-4")))
myplot <- figDat1 %>%
  mutate(type = factor(type, levels = c("Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", metric_value, round(metric_value, 1))) %>%
  ggplot() +
  geom_bar(aes(x = value, y = metric_value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals1,
    aes(x = value, y = total, label = total),
    hjust = -0.05,
    size = 4
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = value, y = metric_value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, nrow = 2, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.45, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
myplot
ggsave("./gen/figures/paa/matching-aw-recalld.png", myplot, width = 6, height = 6, dpi = 500)


figDat1 <- figDat %>%
  filter(variable == "magecat2_int") %>%
  mutate(value = factor(value, levels = c("45+", "40-44","35-39", "30-34", "25-29", "15-24")))
totals1 <- totals %>%
  filter(variable == "magecat2_int") %>%
  mutate(value = factor(value, levels = c("45+", "40-44","35-39", "30-34", "25-29", "15-24")))
myplot <- figDat1 %>%
  mutate(type = factor(type, levels = c("Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", metric_value, round(metric_value, 1))) %>%
  ggplot() +
  geom_bar(aes(x = value, y = metric_value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals1,
    aes(x = value, y = total, label = total),
    hjust = -0.05,
    size = 4
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = value, y = metric_value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, nrow = 2, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.45, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
myplot
ggsave("./gen/figures/paa/matching-aw-motherage.png", myplot, width = 6, height = 6, dpi = 500)

figDat1 <- figDat %>%
  filter(variable == "intcoop_sur") %>%
  mutate(value = factor(value, levels = c("Very good", "Good", "Normal")))
totals1 <- totals %>%
  filter(variable == "intcoop_sur") %>%
  mutate(value = factor(value, levels = c("Very good", "Good", "Normal")))
myplot <- figDat1 %>%
  mutate(type = factor(type, levels = c("Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", metric_value, round(metric_value, 1))) %>%
  ggplot() +
  geom_bar(aes(x = value, y = metric_value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals1,
    aes(x = value, y = total, label = total),
    hjust = -0.05,
    size = 4
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = value, y = metric_value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, nrow = 2, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.45, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
myplot
ggsave("./gen/figures/paa/matching-aw-coop.png", myplot, width = 6, height = 6, dpi = 500)


# combined percents figures
figDat1 <- figDat %>%
  filter(variable %in% c("cstatus_agesp_comb", "cstrata_ac", "deathrecency_cat", "magecat2_int", "intcoop_sur")) %>%
  filter(!(variable == "cstrata_ac" & value %in% c("10+", "5-9 year"))) %>%
  mutate(value = case_when(
    variable == "cstatus_agesp_comb" & value == "5-9" ~ "5-9y",
    variable == "cstatus_agesp_comb" & value == "1-4" ~ "1-4y",
    variable == "cstatus_agesp_comb" & value == "10+" ~ "10+",
    TRUE ~ value
  )) %>%
  mutate(value = factor(value, levels = c(
    "10+", 
    "5-9y", "1-4y", "Postneonatal", "Neonatal",
    #"5-9 year", 
    "1-4 year (other)","1-4 year (drowning)", "Postneonatal (other)",
    "Postneonatal (RI+con)", "Neonatal (other)","Neonatal (birth asphyxia)",
               "15+", "10-14", "5-9", "0-4",
               "45+", "40-44","35-39", "30-34", "25-29", "15-24",
               "Very good", "Good", "Normal")
  )) %>%
  filter(name == "%") %>%
  mutate(label = round(metric_value, 0)) %>%
  mutate(variable = case_when(
    variable == "cstatus_agesp_comb" ~ "Age at death; p=0.03", 
    variable == "cstrata_ac" ~ "COD; p=0.13",
    variable == "deathrecency_cat" ~ "Recall period of death (years); p<0.01",
    variable == "magecat2_int" ~ "Mother's age; p<0.01",
    variable == "intcoop_sur" ~ "Respondent cooperation; p<0.01"
  ))
myplot <- figDat1 %>%
  mutate(type = factor(type, levels = c("Omission", "Match"))) %>%
  ggplot() +
  geom_bar(aes(x = value, y = metric_value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = value, y = metric_value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~variable, nrow = 2, scales = "free_y", labeller = label_wrap_gen(width = 30)) +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.45, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18),
        rect = element_rect(fill = "transparent")) +
  guides(fill = guide_legend(reverse = TRUE))
myplot
ggsave("./gen/figures/paa/matching-aw-all.png", myplot, width = 12, height = 6, dpi = 500)

