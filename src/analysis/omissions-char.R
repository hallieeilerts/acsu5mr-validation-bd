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

# Assign denominator: 
# D - Live births in DSS, only matched live births in FPH
dat <- overall %>%
  mutate(denomD = ifelse((!is.na(pregout_dss) & pregout_dss == "Live birth")  |
                           (!is.na(c223) & c223 == "Live birth" & type == "VS_Match"), 1, 0)
  )

# Omission of live births -------------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", "observer_sur",
  "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac"
)

datLB <- dat %>%
  filter(denomD == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>% 
  select(type, all_of(vars)) #%>% # refactor birthrecency_cat because 15+ category is causing issue with chi-square test
  #mutate(birthrecency_cat = factor(birthrecency_cat, levels = c("0-4", "5-9", "10-14", "15+")))

# create counts and percentages
tabLB <- datLB %>%
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
tabLBtot <- dat %>%
  filter(denomD == 1) %>%
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
tabLB <- tabLB %>%
  bind_rows(tabLBtot)

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  mydat <- datLB
  
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
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat))

# merge on chi-squared
tabLB <- tabLB %>%
  left_join(tabChi %>% select(variable, pvalcat))

# order variables
# household level
v_hh <- c("hhsizecat_sur", "hhassets_sur", "observer_sur")
# women-level
v_wom <- c("magecat2_int", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "deathrecency_cat",
          "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac")
v_all <- c(v_hh, v_wom, v_ch, "total")
length(v_all) == length(unique(tabLB$variable)) # TRUE
df_varrank <- data.frame(variable = v_all,
                         variablerank = 1:length(v_all))

# order values
tabLB <- tabLB %>%
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
    variable == "cstatus_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_comb" & value == "Died" ~ 2,
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
tabLB <- tabLB %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "hhassets_sur"  ~ "Household wealth quintile",
    variable == "observer_sur"  ~ "Interview observed by others",
    variable == "magecat2_int" ~ "Mother age",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recency (years)",
    variable == "deathrecency_cat"   ~ "Death recency (years) (if applicable)",
    variable == "cstatus_comb"   ~ "Survival status",
    variable == "cstatus_agesp_comb"   ~ "Age of death (if applicable)",
    variable == "cstrata_ac"   ~ "Cause of death (if applicable)",
    variable == "total"   ~ "Total",
    TRUE ~ NA
  )) %>%
  select(-c(variablerank, valuerank)) 
tabLB$n_Match[is.na(tabLB$n_Match)] <- 0
tabLB$n_Omission[is.na(tabLB$n_Omission)] <- 0
#tabLB$n_Addition[is.na(tabLB$n_Addition)] <- 0
tabLB$per_Match[is.na(tabLB$per_Match)] <- "0.00"
tabLB$per_Omission[is.na(tabLB$per_Omission)] <- "0.00"
#tabLB$per_Addition[is.na(tabLB$per_Addition)] <- "0.00"
tabLB <- tabLB %>%
  filter(variable != "Cause of death (if applicable)") %>%
  filter(variable != "Death recency (years) (if applicable)")

ft <- tabLB %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%",  "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", ""), colwidths = c(2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of DSS live births by reporting in FPH (ie, matches and omissions)") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabLB), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-lb-omissions-char.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Omission of deaths ------------------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", "observer_sur",
  "cstatus_agesp_comb", "cstrata_ac"
)

datDth <- dat %>%
  filter(denomD == 1 & cstatus_dss == "Died") %>%
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
  filter(denomD == 1 & cstatus_dss == "Died") %>%
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
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "deathrecency_cat", "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac")
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
    variable == "cstatus_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_comb" & value == "Died" ~ 2,
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


ft <- tabDth %>%
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

output_path <- here::here("gen/figures", "table-dths-omissions-char.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")
