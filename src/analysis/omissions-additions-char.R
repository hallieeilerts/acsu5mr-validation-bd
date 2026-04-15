################################################################################
#' @description analyse at the event-level, denominator C only
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
library(modelsummary)
library(marginaleffects)
library(stringr)
library(lme4)
#' Inputs
dat <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Assign denominator: 
# A - Mothers: lifelong residents
# C - Mother+pregnancies: 10 yr residents
# D - Mother+pregnancies: 5 yr residents
# Since this is event level agreement, we use DSS as the reference standard.
# DSS and FPH events are included conditional on the DSS DOB, and only FPH DOB if unmatched.
# Hence there is no need for denomC_dss and denomC_sur columns. There is just one denomC.
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
    variable == "cstrata_ac" & value == "Neonatal (unknown)" ~ 4,
    variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 5,
    variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 6,
    variable == "cstrata_ac" & value == "Postneonatal (unknown)" ~ 7,
    variable == "cstrata_ac" & value == "1-4 year (drowning)" ~ 8,
    variable == "cstrata_ac" & value == "1-4 year (other)" ~ 9,
    variable == "cstrata_ac" & value == "1-4 year (unknown)" ~ 9,
    variable == "cstrata_ac" & value == "5-9 year" ~ 10,
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
tabLB$n_Match[is.na(tabLB$n_Match)] <- 0
tabLB$n_Omission[is.na(tabLB$n_Omission)] <- 0
tabLB$n_Addition[is.na(tabLB$n_Addition)] <- 0
tabLB$per_Match[is.na(tabLB$per_Match)] <- "0.00"
tabLB$per_Omission[is.na(tabLB$per_Omission)] <- "0.00"
tabLB$per_Addition[is.na(tabLB$per_Addition)] <- "0.00"
tabLB <- tabLB %>%
  filter(variable != "Cause of death (if applicable)")

ft <- tabLB %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", "Addition", ""), colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of live births in previous 10 years for resident women by match status") %>%
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

output_path <- here::here("gen/figures", "table-lb-matchType-char.docx")
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

# !!!!why are 10-14 in here when denominator is past 10 years??
# dat %>%
#   filter(denomC == 1 & (pregout_dss == "Live birth" | c223 == "Live birth") &
#            (cstatus_dss == "Died" | cstatus_sur == "Died")) %>%
#   mutate(type = case_when(
#     type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
#     type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
#     type == "VS_Match" ~ "Match",
#     TRUE ~ NA
#   ))  %>% 
#   filter(birthrecency_cat == "10-14") %>%
#   View()

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
    variable == "cstrata_ac" & value == "Neonatal (unknown)" ~ 4,
    variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 5,
    variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 6,
    variable == "cstrata_ac" & value == "Postneonatal (unknown)" ~ 7,
    variable == "cstrata_ac" & value == "1-4 year (drowning)" ~ 8,
    variable == "cstrata_ac" & value == "1-4 year (other)" ~ 9,
    variable == "cstrata_ac" & value == "1-4 year (unknown)" ~ 9,
    variable == "cstrata_ac" & value == "5-9 year" ~ 10,
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
tabDth$n_Match[is.na(tabDth$n_Match)] <- 0
tabDth$n_Omission[is.na(tabDth$n_Omission)] <- 0
tabDth$n_Addition[is.na(tabDth$n_Addition)] <- 0
tabDth$per_Match[is.na(tabDth$per_Match)] <- "0.00"
tabDth$per_Omission[is.na(tabDth$per_Omission)] <- "0.00"
tabDth$per_Addition[is.na(tabDth$per_Addition)] <- "0.00"


ft <- tabDth %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "%", "N", "%", "N", "%", "p-value")) %>%
  add_header_row(values = c(" ","Match", "Omission", "Addition", ""), colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Characteristics of deaths in sample C (pregnancies in previous 10 years for resident women) by match status") %>%
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

output_path <- here::here("gen/figures", "table-dths-matchType-char.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Regression: omission of any event ---------------------------------------

datAll <- dat %>%
  filter(denomC == 1) %>% 
  filter(type  %in% c("HDSS_NoMatch", "VS_Match")) %>%
  mutate(omission = ifelse(type == "HDSS_NoMatch", 1, 0))

# set reference categories
unique(datAll$cstatus_dss)
datAll$cstatus_dss <- factor(datAll$cstatus_dss, levels = c("Surviving", "Died"))

model1 <- glm(
  omission ~ 
    #birthorder_cat_comb +
    #paritymaxcat_comb +
    birthrecency_cat +
    magecat2_int +
    #hhsizecat_sur +
    #hhassets_sur +
    cstatus_dss,
  data = datAll,
  family = binomial()
)
summary(model1)

# Regression: omission of death ---------------------------------------

# Denominator is deaths in DSS
# No need to limit it to C (women with ongoing residency in past 10 years)
# because here we are using DSS as reference and just seeing if reported in FPH
datDth <- dat %>%
  filter(cstatus_dss == "Died") %>%  # denomC == 1 & 
  filter(type  %in% c("HDSS_NoMatch", "VS_Match")) %>%
  mutate(omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  mutate(cod_cat = ifelse(cstrata_c %in% c("Drowning", "Birth asphyxia", "RI and congenital"), "leading", "other"))

# set reference categories
unique(datDth$cstatus_agesp_dss)
datDth$cstatus_agesp_dss <- factor(datDth$cstatus_agesp_dss, levels = c("Neonatal", "Postneonatal", "1-4", "5-9"))
unique(datDth$cstrata_ac)
datDth$cstrata_ac <- factor(datDth$cstrata_ac, 
          levels = c("Neonatal (other)" , "Neonatal (birth asphyxia)", 
                     "Postneonatal (other)", "Postneonatal (RI+con)", 
                      "1-4 year (other)", "1-4 year (drowning)" ,
                     "5-9 year"))
datDth$cod_cat <- factor(datDth$cod_cat, levels = c("other", "leading"))

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(cstatus_agesp_dss != "5-9") %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, levels = c("Neonatal", "Postneonatal", "1-4")))

# null model
m1 <- glm(omission ~ 1, data = subset(datDth, !is.na(cstatus_agesp_dss)), family = binomial())
# age is the main predictor of interest
m2 <- glm(omission ~ cstatus_agesp_dss, data = datDth, family = binomial())
# add controls to age model - does age effect hold after adjustment?
m3 <- glm(omission ~ cstatus_agesp_dss + birthrecency_cat + magecat2_int, data = datDth, family = binomial())
# add cause to adjusted age model - does cause add anything?
m4 <- glm(omission ~ cstatus_agesp_dss + cod_cat + birthrecency_cat + magecat2_int, 
          data = datDth, family = binomial())
# test whether cause interacts with age (binary cause)
m5 <- glm(omission ~ cstatus_agesp_dss * cod_cat + birthrecency_cat + magecat2_int, 
          data = datDth, family = binomial())
# test whether specific causes within age groups matter
m6 <- glm(omission ~ cstrata_ac + birthrecency_cat + magecat2_int, 
          data = datDth, family = binomial())

anova(m1, m2, test = "Chisq")  # does age matter?
anova(m2, m3, test = "Chisq")  # do controls matter?
anova(m3, m4, test = "Chisq")  # does cause add anything after age + controls?
anova(m4, m5, test = "Chisq")  # does cause interact with age?
anova(m3, m6, test = "Chisq")  # do specific cause×age cells add anything beyond age + controls?
# The last comparison is key — it directly tests whether the granular cause × age strata explain more than age alone with controls, which is your central question. If non-significant, you conclude age-specific correction factors are sufficient and M3 is your final model for generating them.

# regression flextable
models <- list("Model 1" = m1, 
               "Model 2" = m2,
               "Model 3" = m3,
               "Model 4" = m4,
               "Model 5" = m5,
               "Model 6" = m6)
# TRUE: c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)
tabMod <- msummary(models, output = "data.frame", stars = TRUE) %>%
  mutate(group = case_when(
    str_detect(term, "Intercept") ~ "Intercept",
    str_detect(term, "^cstatus_agesp_dss(?!.*×)") ~ "Age of death (ref: Neonatal)",
    str_detect(term, "birthrecency") ~ "Birth recency (ref: 0-4)",
    str_detect(term, "magecat2") ~ "Mother age (ref: 15-24)",
    str_detect(term, "cstatus_agesp_dssPostneonatal × cod_catleading") ~ 
      "Age of death  * Cause group",
    str_detect(term, "cstatus_agesp_dss1-4 × cod_catleading") ~ 
      "Age of death  * Cause group",
    str_detect(term, "cod_catleading") ~ "Cause-group of death (ref: other)",
    str_detect(term, "cstrata_ac") ~ "Cause of death (ref: Neonatal - other)",
    TRUE ~ "Other"
  ),
  level = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "birthrecency_cat5-9" ~ "5-9",
    term == "birthrecency_cat10-14" ~ "10-14",
    term == "birthrecency_cat15+" ~ "15+",
    term == "magecat2_int25-29" ~ "25-29",
    term == "magecat2_int30-34" ~ "30-34",
    term == "magecat2_int35-39" ~ "35-39",
    term == "magecat2_int40-44" ~ "40-44",
    term == "magecat2_int45+" ~ "45+",
    term == "cstatus_agesp_dssPostneonatal" ~ "Postneonatal",
    term == "cstatus_agesp_dss1-4" ~ "1-4 years",
    term == "cstatus_agesp_dss5-9" ~ "5-9 years",
    term == "cstrata_acNeonatal (birth asphyxia)" ~ "Neonatal - birth asphyxia",
    term == "cstrata_acPostneonatal (RI+con)" ~ "Postneonatal - RI+con",
    term == "cstrata_acPostneonatal (other)" ~ "Postneonatal - other",
    term == "cstrata_ac1-4 year (drowning)" ~ "1-4 years - drowning",
    term == "cstrata_ac1-4 year (other)" ~ "1-4 years - other",
    term == "cstrata_ac5-9 year" ~ "5-9 years",
    term == "cod_catleading" ~ "Leading cause",
    term == "cstatus_agesp_dssPostneonatal × cod_catleading" ~ "Postneonatal * leading cause",
    term == "cstatus_agesp_dss1-4 × cod_catleading" ~ "1-4 year * leading cause",
    TRUE ~ term
  ))
tabMod <- tabMod %>% filter(part == "estimates" & group != "Intercept") %>%
  bind_rows(tabMod %>% filter(group == "Intercept")) %>%
  bind_rows(tabMod %>% filter(part == "gof"))
ft <- tabMod %>%
  dplyr::select(group, level, `Model 1`, `Model 2`, `Model 3`, `Model 4`, `Model 5`, `Model 6`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  flextable() %>%
  merge_v(j = ~ Variable + Value) %>%
  set_caption(caption = "Logistic regression on deaths omitted from FPH among pregnancies in previous 10 years to resident women") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft


# Sensitivity table -------------------------------------------------------

summary(m3)
results_age <- avg_predictions(m3, variables = c("cstatus_agesp_dss"))
results_agebr <- avg_predictions(m3, variables = c("cstatus_agesp_dss", "birthrecency_cat"))

make_table <- function(results, groupvars) {
  if(length(groupvars) == 1){
    out <- data.frame(
      group          = results[[groupvars]],
      omission       = results$estimate,
      omission_lower = results$conf.low,
      omission_upper = results$conf.high,
      sensitivity    = 1 - results$estimate,
      sens_lower     = 1 - results$conf.high,
      sens_upper     = 1 - results$conf.low
    )
  }else{
    out <- data.frame(results)[,groupvars]
    out$omission       <- results$estimate
    out$omission_lower <- results$conf.low
    out$omission_upper <- results$conf.high
    out$sensitivity    <- 1 - results$estimate
    out$sens_lower     <- 1 - results$conf.high
    out$sens_upper     <- 1 - results$conf.low
  }
  
  return(out)
}

make_table(results_age, "cstatus_agesp_dss")

myres <- make_table(results_agebr, c("cstatus_agesp_dss", "birthrecency_cat"))


myres %>% 
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(omission_bound = paste0("(", omission_lower, ", ", omission_upper, ")"))%>%
  mutate(sens_bound = paste0("(", sens_lower, ", ", sens_upper, ")")) %>%
  dplyr::select(cstatus_agesp_dss, birthrecency_cat, omission, omission_bound, sensitivity, sens_bound)




  # Old regression ----------------------------------------------------------


# Fixed effects model on age
m1 <- glm(omission ~ cstatus_agesp_dss, data = datDth, family = binomial())
summary(m1)
m2 <- glm(omission ~ cstatus_agesp_dss + birthrecency_cat + magecat2_int, data = datDth, family = binomial())
summary(m2)

# Fixed effects model on age/cause
m3 <- glm(omission ~ cstrata_ac, data = datDth, family = binomial())
summary(m3)
foo <- summary(m3)
plogis(foo$coefficients[,1]) # back transformed coefficients
# for non-reference categories, this is the log-odds differences from the intercept
# we actually want... plogis(intercept + coefficient) for non-reference categories
# for neonatal other... plogis(log(0.1684/(1-0.1684)) + log(0.4818/(1-0.4818)))
# per-stratum probabilities with CIs from the fixed effects model
newdat <- data.frame(cstrata_ac = levels(datDth$cstrata_ac))
preds <- predict(m3, newdata = newdat, type = "response", se.fit = TRUE)
newdat$omission <- preds$fit
newdat$omission_lower <- plogis(qlogis(preds$fit) - 1.96 * preds$se.fit)
newdat$omission_upper <- plogis(qlogis(preds$fit) + 1.96 * preds$se.fit)
newdat$sensitivity <- 1 - newdat$omission
# a higher omission rate means lower sensitivity
# so the lower bound of sensitivity comes from the upper bound of omission and vice versa
newdat$sens_lower  <- 1 - newdat$omission_upper
newdat$sens_upper  <- 1 - newdat$omission_lower
newdat

# Fixed effects model on age/cause with controls
m4 <- glm(omission ~ cstrata_ac + birthrecency_cat + magecat2_int, data = datDth, family = binomial())
summary(m4)
# stratum-specific estimates taking account of control variables
# marginal standardization. average over the actual distribution of controls in the data
# predict every observation at each stratum level and average
results <- avg_predictions(m4, variables = "cstrata_ac")
# clean table
make_table <- function(results, groupvar) {
  data.frame(
    group          = results[[groupvar]],
    omission       = results$estimate,
    omission_lower = results$conf.low,
    omission_upper = results$conf.high,
    sensitivity    = 1 - results$estimate,
    sens_lower     = 1 - results$conf.high,
    sens_upper     = 1 - results$conf.low
  )
}
make_table(results, "cstrata_ac")


# Fixed effects model on age/cause if cause is leading/other
m5 <- glm(omission ~ cstatus_agesp_dss + cod_cat, data = datDthyoung, family = binomial())
summary(m5)
plogis(-1.62851)               # neonatal omission
plogis(-1.62851 + (-0.85500))  # postneonatal
plogis(-1.62851 + (-0.74293))  # 1-4 year
# coefficient for cod_catleading is -0.047 which is tiny and non-sgificant
# after controlling for age, cause of death doesnt seem associated with omission
# by cause
results_cod <- avg_predictions(m5, variables = "cod_cat")
# by age
results_age <- avg_predictions(m5, variables = "cstatus_agesp_dss")
make_table(results_cod, "cod_cat")
make_table(results_age, "cstatus_agesp_dss")
# The cause-specific table will show you two very similar omission rates — the −0.047 coefficient means leading cause has marginally lower omission than other cause but the difference is negligible and you cannot distinguish it from zero. The age-specific table will show the gradient more clearly with neonatal having the highest omission rate.
# so how does this fit in with my other results? is cause important?

m6 <- glm(omission ~ cstatus_agesp_dss * cod_cat, data = datDthyoung, family = binomial())
anova(m5, m6, test = "Chisq")
# The interaction is not significant (p = 0.677) and the deviance hardly changes at all (0.78 on 2 df). So the data do not support the interaction model — cause does not operate differently by age group in a statistically detectable way.


# hierarchical model
m_hier <- glmer(omission ~  (1 | cstrata_ac), data = datDth,family = binomial())
summary(m_hier)
# RE variance of 0.0652 is small but not zero. some variation across strata, but modest
# Estimate = mean log-odds of omission across all strata
# back transform for average omission rate across strata
plogis(-2.06) # 11.3% omission probability across all strata
# estimate +/- SD
plogis(-2.06 - 0.255) # 8.9%
plogis(-2.06 + 0.255) # 14.1%
newdat <- data.frame(cstrata_ac = levels(datDth$cstrata_ac))
preds <- predict(m_hier, newdata = newdat, type = "response", re.form = NULL)
newdat$omission_pred <- preds
newdat
# define prediction function
pred_fun <- function(m) {
  predict(m, newdata = newdat, type = "response", re.form = NULL)
}
# bootstrap
boot_out <- bootMer(m_hier, 
                    FUN = pred_fun, 
                    nsim = 1000, 
                    use.u = FALSE,  # resample random effects
                    type = "parametric")
# extract CIs for omission
ci_matrix <- t(apply(boot_out$t, 2, quantile, probs = c(0.025, 0.975)))
newdat$omission_lower <- ci_matrix[, 1]
newdat$omission_upper <- ci_matrix[, 2]
newdat$sensitivity <- 1 - preds
newdat$sens_lower   <- 1 - newdat$omission_upper 
newdat$sens_upper   <- 1 - newdat$omission_lower
newdat

# The narrow range of predictions (9–14%) and small random effect variance together suggest that omission rates do not differ substantially by age-at-death or cause-of-death stratum. This is actually a meaningful finding — it suggests a relatively uniform correction factor of roughly 0.86–0.90 sensitivity (i.e. surveys capture about 86–90% of deaths) may apply across strata rather than needing stratum-specific corrections
# However, before concluding this, check two things. First confirm your sample sizes per stratum — if some cells are small, the pooling may be masking real differences rather than revealing their absence. Second, compare this hierarchical model back to your saturated fixed-effects model from before to see if the stratum-specific estimates were already similar there, which would corroborate this finding.


# old ---------------------------------------------------------------------


m1 <- glm(
  omission ~ 
    #birthorder_cat_comb +
    #paritymaxcat_comb +
    birthrecency_cat +
    magecat2_int +
    #hhsizecat_sur +
    #hhassets_sur +
    cstatus_agesp_dss,
  data = datDth,
  family = binomial()
)
summary(model1)


model1 <- glm(
  omission ~ 
    cstatus_agesp_dss +
    cod_cat +
    cstatus_agesp_dss * cod_cat,
  data = datDth,
  family = binomial()
)
summary(model1)
model2 <- glm(
  omission ~ 
    cstatus_agesp_dss * cod_cat,
  data = datDth,
  family = binomial()
)
summary(model2)


#as_flextable(model1, model2)
models <- list("Model A" = model1, "Model B" = model2)

# TRUE: c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)
tabMod <- msummary(models, output = "data.frame", stars = TRUE) %>%
  mutate(group = case_when(
    str_detect(term, "Intercept") ~ "Intercept",
    str_detect(term, "birthrecency") ~ "Birth recency (ref: 0-4)",
    str_detect(term, "mage") ~ "Mother age (ref: 15-24)",
    str_detect(term, "cstatus_agesp") ~ "Age of death (ref: Neonatal)",
    str_detect(term, "cstrata") ~ "Cause of death (ref: Neonatal (birth asphyxia))",
    TRUE ~ "Other"
  ),
  level = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "birthrecency_cat5-9" ~ "5-9",
    term == "birthrecency_cat10-14" ~ "10-14",
    term == "magecat2_int25-29" ~ "25-29",
    term == "magecat2_int30-34" ~ "30-34",
    term == "magecat2_int35-39" ~ "35-39",
    term == "magecat2_int40-44" ~ "40-44",
    term == "magecat2_int45+" ~ "45+",
    term == "cstatus_agesp_dssPostneonatal" ~ "Postneonatal",
    term == "cstatus_agesp_dss1-4" ~ "1-4 years",
    term == "cstatus_agesp_dss5-9" ~ "5-9 years",
    term == "cstrata_acNeonatal (other)" ~ "Neonatal (other)",
    term == "cstrata_acPostneonatal (RI+con)" ~ "Postneonatal (RI+con)",
    term == "cstrata_acPostneonatal (other)" ~ "Postneonatal (other)",
    term == "cstrata_ac1-4 year (drowning)" ~ "1-4 years (drowning)",
    term == "cstrata_ac1-4 year (other)" ~ "1-4 years (other)",
    term == "cstrata_ac5-9 year" ~ "5-9 years",
    TRUE ~ term
  )) 
tabMod <- tabMod %>% filter(part == "estimates" & group != "Intercept") %>%
  bind_rows(tabMod %>% filter(group == "Intercept")) %>%
  bind_rows(tabMod %>% filter(part == "gof"))
ft <- tabMod %>%
  select(group, level, `Model A`, `Model B`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  flextable() %>%
  merge_v(j = ~ Variable + Value) %>%
  set_caption(caption = "Logistic regression on deaths omitted from FPH among pregnancies in previous 10 years to resident women") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "regression-dths-omissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")
