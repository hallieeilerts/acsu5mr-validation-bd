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
overall <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

## Omissions
# Subsample: (A) all-women
# Denominator: deaths in DSS
# Exclusions: (we don't want to estimate a coefficient for)
# deaths taking place 15+ years ago
# because no need for correct factor given survey practice of calculating rates 0-4, 5-9, 10-14
# those for children 10+
# because data is too sparse
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
         # deathrecency < 15 years ago
         happenedRecently = ifelse(deathrecency < 15, 1, 0),
         # children < 10y
         youngerChildren = ifelse(cstatus_agesp_comb != "10+", 1, 0),
         denomA = ifelse(subsampA == 1 & eventDth_dss == 1 & 
                           happenedRecently == 1 & youngerChildren == 1, 1, 0) ,
         denomC = ifelse(subsampC == 1 & eventDth_sur == 1, 1, 0)
  ) 


# Regression: omission ----------------------------------------------------

# Regression on omission of deaths

datDth <- dat %>%
  filter(denomA == 1) %>%  
  mutate(omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  mutate(cod_cat = ifelse(cstrata_c %in% c("Drowning", "Birth asphyxia", "RI and congenital"), "leading", "other"))

# set reference categories
unique(datDth$deathrecency_cat)
datDth$deathrecency_cat <- factor(datDth$deathrecency_cat, levels = c("0-4", "5-9", "10-14"))
unique(datDth$cstatus_agesp_comb)
datDth$cstatus_agesp_comb <- factor(datDth$cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4", "5-9"))
unique(datDth$cstrata_ac)
datDth$cstrata_ac <- factor(datDth$cstrata_ac, 
                            levels = c("Neonatal (other)" , "Neonatal (birth asphyxia)", 
                                       "Postneonatal (other)", "Postneonatal (RI+con)", 
                                       "1-4 year (other)", "1-4 year (drowning)" ,
                                       "5-9 year"))
datDth$cod_cat <- factor(datDth$cod_cat, levels = c("other", "leading"))

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(!(cstatus_agesp_comb %in% c("5-9"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4")))

# null model
m1 <- glm(omission ~ 1, data = datDth, family = binomial())
# age is the main predictor of interest
m2 <- glm(omission ~ cstatus_agesp_comb, data = datDth, family = binomial())
# add controls to age model - does age effect hold after adjustment?
m3 <- glm(omission ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth, family = binomial())
# add cause to adjusted age model - does cause add anything?
m4 <- glm(omission ~ cstatus_agesp_comb + cod_cat + deathrecency_cat + magecat2_int, 
          data = datDth, family = binomial())
# test whether cause interacts with age (binary cause)
m5 <- glm(omission ~ cstatus_agesp_comb * cod_cat + deathrecency_cat + magecat2_int, 
          data = datDth, family = binomial())
# test whether specific causes within age groups matter
m6 <- glm(omission ~ cstrata_ac + deathrecency_cat + magecat2_int, 
          data = datDth, family = binomial())

anova(m1, m2, test = "Chisq")  # does age matter?
anova(m2, m3, test = "Chisq")  # do controls matter?
anova(m3, m4, test = "Chisq")  # does cause add anything after age + controls? no
anova(m4, m5, test = "Chisq")  # does cause interact with age? no
anova(m3, m6, test = "Chisq")  # do specific cause*age cells add anything beyond age + controls? no
# The last comparison itests whether the granular cause*age strata explain more than age alone with controls
# non-significant = age-specific correction factors are sufficient and m3 is the final model for generating them
m3O <- m3

# regression flextable
modelsO <- list("Model 1" = m1, 
               "Model 2" = m2,
               "Model 3" = m3,
               "Model 4" = m4,
               "Model 5" = m5,
               "Model 6" = m6)
# TRUE: c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)
tabMod <- msummary(modelsO, output = "data.frame", stars = TRUE, fmt = 2)  %>%
  mutate(group = case_when(
    str_detect(term, "Intercept") ~ "Intercept",
    str_detect(term, "^cstatus_agesp_comb(?!.*×)") ~ "Age of death (ref: Neonatal)",
    str_detect(term, "deathrecency") ~ "Death recency (ref: 0-4)",
    str_detect(term, "magecat2") ~ "Mother age (ref: 15-24)",
    str_detect(term, "cstatus_agesp_combPostneonatal × cod_catleading") ~ 
      "Age of death  x Cause group",
    str_detect(term, "cstatus_agesp_comb1-4 × cod_catleading") ~ 
      "Age of death  x Cause group",
    str_detect(term, "cod_catleading") ~ "Cause-group of death (ref: other)",
    str_detect(term, "cstrata_ac") ~ "Cause of death (ref: Neonatal - other)",
    TRUE ~ "Other"
  ),
  level = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "deathrecency_cat5-9" ~ "5-9",
    term == "deathrecency_cat10-14" ~ "10-14",
    term == "magecat2_int25-29" ~ "25-29",
    term == "magecat2_int30-34" ~ "30-34",
    term == "magecat2_int35-39" ~ "35-39",
    term == "magecat2_int40-44" ~ "40-44",
    term == "magecat2_int45+" ~ "45+",
    term == "cstatus_agesp_combPostneonatal" ~ "Postneonatal",
    term == "cstatus_agesp_comb1-4" ~ "1-4 years",
    term == "cstatus_agesp_comb5-9" ~ "5-9 years",
    term == "cstatus_agesp_comb10+" ~ "10+ years",
    term == "cstrata_acNeonatal (birth asphyxia)" ~ "Neonatal - birth asphyxia",
    term == "cstrata_acPostneonatal (RI+con)" ~ "Postneonatal - RI+con",
    term == "cstrata_acPostneonatal (other)" ~ "Postneonatal - other",
    term == "cstrata_ac1-4 year (drowning)" ~ "1-4 years - drowning",
    term == "cstrata_ac1-4 year (other)" ~ "1-4 years - other",
    term == "cstrata_ac5-9 year" ~ "5-9 years",
    term == "cstrata_ac10+" ~ "10+ years",
    term == "cod_catleading" ~ "Leading cause",
    term == "cstatus_agesp_combPostneonatal × cod_catleading" ~ "Postneonatal x leading cause",
    term == "cstatus_agesp_comb1-4 × cod_catleading" ~ "1-4 year x leading cause",
    TRUE ~ term
  ))
tabMod <- tabMod %>% filter(part == "estimates" & group != "Intercept") %>%
  bind_rows(tabMod %>% filter(group == "Intercept")) %>%
  bind_rows(tabMod %>% filter(part == "gof"))
tabModO <- tabMod
ft <- tabModO %>%
  dplyr::select(group, level, `Model 1`, `Model 2`, `Model 3`, `Model 4`, `Model 5`, `Model 6`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  flextable() %>%
  merge_v(j = ~ Variable + Value) %>%
  set_caption(caption = "Logistic regression on DSS deaths omitted from FPH") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft

# doc <- read_docx() %>%
#   body_add_par("Table 1", style = "heading 1") %>%
#   body_add_flextable(ft)
# 
# output_path <- here::here("gen/figures", "table-regression-dths-omissions.docx")
# print(doc, target = output_path)
# cat("Saved to:", output_path, "\n")

# Regression: addition ----------------------------------------------------

# Regression on addition of deaths

datDth <- dat %>%
  filter(denomC == 1) %>%  
  mutate(addition = ifelse(type == "VS_NoMatch", 1, 0)) 

# set reference categories
unique(datDth$deathrecency_cat)
datDth$deathrecency_cat <- factor(datDth$deathrecency_cat, levels = c("0-4", "5-9", "10-14"))
unique(datDth$cstatus_agesp_comb)
datDth$cstatus_agesp_comb <- factor(datDth$cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4", "5-9"))

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(!(cstatus_agesp_comb %in% c("5-9"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4")))

# null model
m1 <- glm(addition ~ 1, data = datDth, family = binomial())
# age is the main predictor of interest
m2 <- glm(addition ~ cstatus_agesp_comb, data = datDth, family = binomial())
# add controls to age model - does age effect hold after adjustment?
m3 <- glm(addition ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth, family = binomial())

anova(m1, m2, test = "Chisq")  # does age matter?
anova(m2, m3, test = "Chisq")  # do controls matter? yes

# final model
m3A <- m3

# regression flextable
modelsA <- list("Model 1" = m1, 
               "Model 2" = m2,
               "Model 3" = m3)
# TRUE: c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)
tabMod <- msummary(modelsA, output = "data.frame", stars = TRUE, fmt = 2) %>%
  mutate(group = case_when(
    str_detect(term, "Intercept") ~ "Intercept",
    str_detect(term, "^cstatus_agesp_comb(?!.*×)") ~ "Age of death (ref: Neonatal)",
    str_detect(term, "deathrecency") ~ "Death recency (ref: 0-4)",
    str_detect(term, "magecat2") ~ "Mother age (ref: 15-24)",
    TRUE ~ "Other"
  ),
  level = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "deathrecency_cat5-9" ~ "5-9",
    term == "deathrecency_cat10-14" ~ "10-14",
    term == "magecat2_int25-29" ~ "25-29",
    term == "magecat2_int30-34" ~ "30-34",
    term == "magecat2_int35-39" ~ "35-39",
    term == "magecat2_int40-44" ~ "40-44",
    term == "magecat2_int45+" ~ "45+",
    term == "cstatus_agesp_combPostneonatal" ~ "Postneonatal",
    term == "cstatus_agesp_comb1-4" ~ "1-4 years",
    term == "cstatus_agesp_comb5-9" ~ "5-9 years",
    TRUE ~ term
  ))
tabMod <- tabMod %>% filter(part == "estimates" & group != "Intercept") %>%
  bind_rows(tabMod %>% filter(group == "Intercept")) %>%
  bind_rows(tabMod %>% filter(part == "gof"))
tabModA <- tabMod
ft <- tabModA %>%
  dplyr::select(group, level, `Model 1`, `Model 2`, `Model 3`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  flextable() %>%
  merge_v(j = ~ Variable + Value) %>%
  set_caption(caption = "Logistic regression on DSS deaths omitted from FPH") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft


# Combined flextable ------------------------------------------------------

tabMod <- tabModO %>% select(-c(term)) %>%
  left_join(tabModA %>% select(-c(term)), by = c("part", "statistic", "group", "level"))

ft <- tabMod %>%
  dplyr::select(group, level, `Model 1.x`, `Model 2.x`, `Model 3.x`, `Model 4`, `Model 5`, `Model 6`,
                `Model 1.y`, `Model 2.y`, `Model 3.y`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  flextable() %>%
  add_header_row(values = c(" ","Omission", "Addition"), colwidths = c(2, 6, 3)) %>%
  merge_v(j = ~ Variable + Value) %>%
  set_caption(caption = "Logistic regression") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-regression-dths.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Sensitivity table -------------------------------------------------------

predO <- avg_predictions(m3O, variables = c("cstatus_agesp_comb", "deathrecency_cat"))
predA <- avg_predictions(m3A, variables = c("cstatus_agesp_comb", "deathrecency_cat"))

make_table <- function(results, groupvars) {
  if(length(groupvars) == 1){
    out <- data.frame(
      group          = results[[groupvars]],
      outcome       = results$estimate,
      outcome_lb = results$conf.low,
      outcome_ub = results$conf.high,
      sensitivity    = 1 - results$estimate,
      sens_lb     = 1 - results$conf.high,
      sens_ub     = 1 - results$conf.low
    )
  }else{
    out <- data.frame(results)[,groupvars]
    out$outcome       <- results$estimate
    out$outcome_lb <- results$conf.low
    out$outcome_ub <- results$conf.high
    out$sensitivity    <- 1 - results$estimate
    out$sens_lb     <- 1 - results$conf.high
    out$sens_ub     <- 1 - results$conf.low
  }
  
  return(out)
}

#make_table(results_age, "cstatus_agesp_comb")

tabPredO <- make_table(predO, c("cstatus_agesp_comb", "deathrecency_cat"))
tabPredA <- make_table(predA, c("cstatus_agesp_comb", "deathrecency_cat"))
tabPred <- tabPredO %>%
  left_join(tabPredA, by = c("cstatus_agesp_comb", "deathrecency_cat")) %>%
  mutate(outcome_lb.x = ifelse(outcome_lb.x < 0, 0, outcome_lb.x),
         outcome_lb.y = ifelse(outcome_lb.y < 0, 0, outcome_lb.y),
         sens_upper.x = ifelse(sens_ub.x > 1 ,1 , sens_ub.x),
         sens_upper.y = ifelse(sens_ub.y > 1 ,1 , sens_ub.y)) %>%
  mutate(cf = (1 - outcome.y)/(1 - outcome.x),
         cf_lb = (1 - outcome_ub.y)/(1 - outcome_ub.x), 
         cf_ub = (1 - outcome_lb.y)/(1 - outcome_lb.x)) %>% 
  mutate(cf = cf/100,
         cf_lb = cf_lb/100,
         cf_ub = cf_ub/100) %>% # divide by 100 before next step
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(outcome_ci.x = paste0("(", outcome_lb.x, ", ", outcome_ub.x, ")"),
         outcome_ci.y = paste0("(", outcome_lb.y, ", ", outcome_ub.y, ")"),
         cf_ci = paste0("(", cf_lb, ", ", cf_ub, ")"))

ft <- tabPred %>%
  dplyr::select(cstatus_agesp_comb, deathrecency_cat, 
                outcome.x, outcome_ci.x, outcome.y, outcome_ci.y, cf, cf_ci) %>%
  flextable() %>%
  merge_v(j = ~ cstatus_agesp_comb + deathrecency_cat) %>%
  set_header_labels(values = c("Age of death", "Death recency (years)", "%", "CI", "%", "CI", "%", "CI")) %>%
  add_header_row(values = c(" ","Omission", "Addition", "Correction factor"), colwidths = c(2, 2, 2,2)) %>%
  set_caption(caption = "Predicted probabilities of omission and addition and correction factors.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft



myres <- myres %>% 
  mutate(omission_lower = ifelse(omission_lower < 0, 0, omission_lower),
         sens_upper = ifelse(sens_upper >1 ,1 , sens_upper)) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(omission_bound = paste0("(", omission_lower, ", ", omission_upper, ")"))%>%
  mutate(sens_bound = paste0("(", sens_lower, ", ", sens_upper, ")")) %>%
  dplyr::select(cstatus_agesp_comb, birthrecency_cat, omission, omission_bound, sensitivity, sens_bound)


ft <- myres %>%
  dplyr::select(cstatus_agesp_comb, birthrecency_cat, omission, omission_bound, sensitivity, sens_bound) %>%
  filter(birthrecency_cat != "15+") %>%
  flextable() %>%
  merge_v(j = ~ cstatus_agesp_comb + birthrecency_cat) %>%
  set_header_labels(values = c("Age of death", "Birth recency (years)", "%", "CI", "%", "CI")) %>%
  add_header_row(values = c(" ","Omission", "Sensitivity"), colwidths = c(2, 2, 2)) %>%
  set_caption(caption = "Logistic regression on DSS deaths omitted from FPH") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-sensitivity-dths-omissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

