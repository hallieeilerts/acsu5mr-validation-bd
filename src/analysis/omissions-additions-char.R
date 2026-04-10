################################################################################
#' @description analyse the overallDob file, event level, denominator C
#' Assess:
#' characteristics of omissions/additions of live births, deaths
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
#' Inputs
dat <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Assign denominator: 
# A - Mothers: lifelong residents
# C - Mother+pregnancies: 10 yr residents
# D - Mother+pregnancies: 5 yr residents
dat <- dat %>%
  mutate(denomA = ifelse(dob_m_dss == doi_m_dss, 1, 0),
         denomC = ifelse(
           # mother's in-migration is more than 10 years ago, and
           as.numeric(as.Date(max(unique(dat$int_date_sur))) - doi_m_dss)/365.25 >= 10 & 
             # dss dob is within past 10 years or
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(dat$int_date_sur))) - dob_c_dss)/365.25 <= 10 | 
                # unmatched validation study dob is within past 10 years
                (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(dat$int_date_sur))) - c220)/365.25 <= 10)), 
           1, 0),
         denomD = ifelse(
           # mother's in-migration is more than 10 years ago
           as.numeric(as.Date(max(unique(dat$int_date_sur))) - doi_m_dss)/365.25 >= 5 & 
             # dss dob is within past 10 years
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(dat$int_date_sur))) - dob_c_dss)/365.25 <= 5 | 
                # unmatched validation study dob is within past 10 years
                (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(dat$int_date_sur))) - c220)/365.25 <= 5)), 
           1, 0)
  )

# Descriptive table: omission/addition of live births ----------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", 
  "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac"
)

datLB <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth")) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>% 
  select(type, all_of(vars)) %>% # refactor birthrecency_cat because 15+ category is causing issue with chi-square test
  mutate(birthrecency_cat = factor(birthrecency_cat, levels = c("0-4", "5-9", "10-14")))

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
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition) 

# add total row
tabLBtot <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth")) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
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
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition) 
tabLB <- tabLB %>%
  bind_rows(tabLBtot)

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  if(v == "cstrata_ac"){
    mydat <- subset(datLB, type != "Addition")
  }else{
    mydat <- datLB
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
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat))

# merge on chi-squared
tabLB <- tabLB %>%
  left_join(tabChi %>% select(variable, pvalcat))

# order variables
# household level
v_hh <- c("hhsizecat_sur", "hhassets_sur")
# women-level
v_wom <- c("magecat2_int", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac")
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
    variable == "hhsizecat_sur" & value == "Small" ~ 1,
    variable == "hhsizecat_sur" & value == "Medium" ~ 2,
    variable == "hhsizecat_sur" & value == "Large" ~ 3,
    variable == "cstatus_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_comb" & value == "Died" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_agesp_comb" & value == "Neonatal" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Postneonatal" ~ 3,
    variable == "cstatus_agesp_comb" & value == "1-4" ~ 4,
    variable == "cstatus_agesp_comb" & value == "5-9" ~ 5,
    variable == "cstrata_ac" & value == "Surviving" ~ 1,
    variable == "cstrata_ac" & value == "Neonatal (birth asphyxia)" ~ 2,
    variable == "cstrata_ac" & value == "Neonatal (other)" ~ 3,
    variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 4,
    variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 5,
    variable == "cstrata_ac" & value == "1-4 year (drowning)" ~ 6,
    variable == "cstrata_ac" & value == "1-4 year (other)" ~ 7,
    variable == "cstrata_ac" & value == "5-9 year" ~ 8,
    variable == "cstrata_ac" & value == "Unknown" ~ 9,
    TRUE ~ 1
  )) %>%
  arrange(variablerank, valuerank)

# clean up variable names
tabLB <- tabLB %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "hhassets_sur"  ~ "Household wealth quintile",
    variable == "magecat2_int" ~ "Mother age",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recency (years)",
    variable == "cstatus_comb"   ~ "Survival status",
    variable == "cstatus_agesp_comb"   ~ "Age of death (if applicable)",
    variable == "cstrata_ac"   ~ "Cause of death (if applicable)",
    variable == "total"   ~ "Total",
    TRUE ~ NA
  )) %>%
  select(-c(variablerank, valuerank)) 

ft <- tabLB %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", "Addition", ""), colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of live births in sample C (pregnancies in previous 10 years for resident women) by match status") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabLB), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-lb-omissionsAdditions-char.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Descriptive table: omission/addition of deaths ----------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", 
  "cstatus_agesp_comb", "cstrata_ac"
)

datDth <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>% 
  select(type, all_of(vars)) %>% # refactor birthrecency_cat because 15+ category is causing issue with chi-square test
  mutate(birthrecency_cat = factor(birthrecency_cat, levels = c("0-4", "5-9", "10-14")))

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
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition) 

# add total row
tabDthtot <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died"))  %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
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
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition) 
tabDth <- tabDth %>%
  bind_rows(tabDthtot)

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  if(v == "cstrata_ac"){
    mydat <- subset(datDth, type != "Addition")
  }else{
    mydat <- datDth
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
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat))

# merge on chi-squared
tabDth <- tabDth %>%
  left_join(tabChi %>% select(variable, pvalcat))

# order variables
# household level
v_hh <- c("hhsizecat_sur", "hhassets_sur")
# women-level
v_wom <- c("magecat2_int", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac")
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
    variable == "hhsizecat_sur" & value == "Small" ~ 1,
    variable == "hhsizecat_sur" & value == "Medium" ~ 2,
    variable == "hhsizecat_sur" & value == "Large" ~ 3,
    variable == "cstatus_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_comb" & value == "Died" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_agesp_comb" & value == "Neonatal" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Postneonatal" ~ 3,
    variable == "cstatus_agesp_comb" & value == "1-4" ~ 4,
    variable == "cstatus_agesp_comb" & value == "5-9" ~ 5,
    variable == "cstrata_ac" & value == "Surviving" ~ 1,
    variable == "cstrata_ac" & value == "Neonatal (birth asphyxia)" ~ 2,
    variable == "cstrata_ac" & value == "Neonatal (other)" ~ 3,
    variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 4,
    variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 5,
    variable == "cstrata_ac" & value == "1-4 year (drowning)" ~ 6,
    variable == "cstrata_ac" & value == "1-4 year (other)" ~ 7,
    variable == "cstrata_ac" & value == "5-9 year" ~ 8,
    variable == "cstrata_ac" & value == "Unknown" ~ 9,
    TRUE ~ 1
  )) %>%
  arrange(variablerank, valuerank)

# clean up variable names
tabDth <- tabDth %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "hhassets_sur"  ~ "Household wealth quintile",
    variable == "magecat2_int" ~ "Mother age",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recency (years)",
    variable == "cstatus_agesp_comb"   ~ "Age of death",
    variable == "cstrata_ac"   ~ "Cause of death",
    variable == "total"   ~ "Total",
  )) %>%
  select(-c(variablerank, valuerank)) 

ft <- tabDth %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", "Addition", ""), colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of deaths in sample C (pregnancies in previous 10 years for resident women) by match status") %>%
  merge_v(j = ~ variable + pvalcat) %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabDth), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-dths-omissionsAdditions-char.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Regression for match or no match ----------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", 
  "cstatus_agesp_comb", "cstrata_ac"
)

datDth <- dat %>%
  filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
           (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  ))  %>% 
  select(type, all_of(vars)) %>%
  filter(type != "Addition")


datDth$type <- factor(datDth$type)
datDth$type <- relevel(datDth$type, ref = "Match")  # reference category

model <- glm(
  type ~ birthorder_cat_comb +
    paritymaxcat_comb +
    birthrecency_cat +
    magecat2_int +
    hhsizecat_sur +
    hhassets_sur +
    cstatus_agesp_comb +
    cstrata_ac,
  data = dat,
  family = binomial()
)

summary(model)
