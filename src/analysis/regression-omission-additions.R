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
library(geepack)
library(multgee)
#' Inputs
overall <- readRDS("./gen/augment/overallName-recode.rds")
################################################################################

## Omissions
# Subsample: (A) all-women
# Denominator: deaths in DSS
# Exclusions: (we don't want to estimate a coefficient for)
# deaths taking place 15+ years ago
# because no need for correct factor given survey practice of calculating rates 0-4, 5-9, 10-14
# those for children 10+
# because data is too sparse
dat <- overall %>%
  mutate(subsampA = 1,
         subsampC_dss = ifelse(
           # mother's in-migration is more than 15 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 15 & 
             # dss dob is within past 15 years
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 15), 
           1, 0),
         subsampC_sur = ifelse(
           # mother's in-migration is more than 15 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 15 & 
             # validation study dob is within past 15 years
             (!is.na(c220) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 15), 
           1, 0),
         # deaths in dss
         eventDth_dss = ifelse(cstatus_dss == "Died", 1, 0),
         # deaths in survey
         eventDth_sur = ifelse(cstatus_sur == "Died", 1, 0),
         # deaths in either source
         eventDth = ifelse(eventDth_dss == 1 | eventDth_sur == 1, 1, 0),
         # deathrecency < 15 years ago
         happenedRecently = ifelse(deathrecency < 15, 1, 0),
         # children < 10y
         youngerChildren = ifelse(cstatus_agesp_comb != "10+", 1, 0),
         denomA = ifelse(subsampA == 1 & eventDth_dss == 1 & 
                           happenedRecently == 1 & youngerChildren == 1, 1, 0),
         denomC = ifelse((subsampC_dss == 1 | subsampC_sur == 1) & eventDth_sur == 1, 1, 0),
         denomC_multinom = ifelse((subsampC_dss == 1 | subsampC_sur == 1) & (eventDth_sur == 1 | eventDth_dss == 1), 1, 0))


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
datDth$rid_m <- as.factor(datDth$rid_m)

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(!(cstatus_agesp_comb %in% c("5-9"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4")))

## Note: haven't used the datDthyoung dataset yet. I meant to before PAA but did not get to it.
# This should be used for the models with cause.

# null model
m1 <- glm(omission ~ 1, data = datDth, family = binomial())
# age is the main predictor of interest
m2 <- glm(omission ~ cstatus_agesp_comb, data = datDth, family = binomial())
# add controls to age model - does age effect hold after adjustment?
#m3 <- glm(omission ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth, family = binomial())
m3 <- geeglm(omission ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth,
             id = rid_m, family = binomial(link = "logit"))
# add cause to adjusted age model - does cause add anything?
# m4 <- glm(omission ~ cstatus_agesp_comb + cod_cat + deathrecency_cat + magecat2_int, 
#           data = datDth, family = binomial())
m4 <- geeglm(omission ~ cstatus_agesp_comb + cod_cat + deathrecency_cat + magecat2_int, 
             data = datDth,
             id = rid_m, family = binomial(link = "logit"))
# test whether cause interacts with age (binary cause)
m5 <- glm(omission ~ cstatus_agesp_comb * cod_cat + deathrecency_cat + magecat2_int,
          data = datDth, family = binomial())
# m5 <- geeglm(omission ~ cstatus_agesp_comb * cod_cat + deathrecency_cat + magecat2_int, data = datDth,
#              id = rid_m, family = binomial(link = "logit"))
# test whether specific causes within age groups matter
# m6 <- glm(omission ~ cstrata_ac + deathrecency_cat + magecat2_int, 
#           data = datDth, family = binomial())
m6 <- geeglm(omission ~ cstrata_ac + deathrecency_cat + magecat2_int, data = datDth,
             id = rid_m, family = binomial(link = "logit"))

library(pROC)
probs <- predict(m1, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) # 0.5
probs <- predict(m2, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) # 0.5998
probs <- predict(m3, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) # 0.6859
probs <- predict(m4, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) #  0.6903
probs <- predict(m5, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) #  0.6859
probs <- predict(m6, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) #  0.689


anova(m1, m2, test = "Chisq")  # does age matter?
#anova(m2, m3, test = "Chisq")  # do controls matter?
#anova(m3, m4, test = "Chisq")  # does cause add anything after age + controls? no
#anova(m4, m5, test = "Chisq")  # does cause interact with age? no
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
    str_detect(term, "^cstatus_agesp_comb(?!.*×)") ~ "Age-at-death (ref: Neonatal)",
    str_detect(term, "deathrecency") ~ "Recall period of death (ref: 0-4 years)",
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
datDth$rid_m <- as.factor(datDth$rid_m)

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(!(cstatus_agesp_comb %in% c("5-9"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4")))

# null model
m1 <- glm(addition ~ 1, data = datDth, family = binomial())
# age is the main predictor of interest
m2 <- glm(addition ~ cstatus_agesp_comb, data = datDth, family = binomial())
# add controls to age model - does age effect hold after adjustment?
#m3 <- glm(addition ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth, family = binomial())
m3 <- geeglm(addition ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth,
             id = rid_m, family = binomial(link = "logit"))

#anova(m1, m2, test = "Chisq")  # does age matter?
#anova(m2, m3, test = "Chisq")  # do controls matter? yes

probs <- predict(m3, datDth, type = "response")
roc_obj <- roc(datDth$addition, probs)
auc(roc_obj) # 0.6859

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
    str_detect(term, "^cstatus_agesp_comb(?!.*×)") ~ "Age-at-death (ref: Neonatal)",
    str_detect(term, "deathrecency") ~ "Recall period of death (ref: 0-4 years)",
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
  #dplyr::select(group, level, `Model 1.x`, `Model 2.x`, `Model 3.x`, `Model 4`, `Model 5`, `Model 6`,
  #              `Model 1.y`, `Model 2.y`, `Model 3.y`) %>%
  dplyr::select(group, level, `Model 3.x`, `Model 4`, `Model 5`, `Model 6`,`Model 3.y`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  flextable() %>%
  #add_header_row(values = c(" ","Omission", "Addition"), colwidths = c(2, 6, 3)) %>%
  add_header_row(values = c(" ","Omission", "Addition"), colwidths = c(2, 4, 1)) %>%
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


# Correction factor table -------------------------------------------------


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

# tabPredO <- make_table(predO, c("cstatus_agesp_comb", "deathrecency_cat"))
# tabPredA <- make_table(predA, c("cstatus_agesp_comb", "deathrecency_cat"))
# tabPred <- tabPredO %>%
#   left_join(tabPredA, by = c("cstatus_agesp_comb", "deathrecency_cat")) %>%
#   mutate(outcome_lb.x = ifelse(outcome_lb.x < 0, 0, outcome_lb.x),
#          outcome_lb.y = ifelse(outcome_lb.y < 0, 0, outcome_lb.y),
#          sens_upper.x = ifelse(sens_ub.x > 1 ,1 , sens_ub.x),
#          sens_upper.y = ifelse(sens_ub.y > 1 ,1 , sens_ub.y)) %>%
#   mutate(
#     cf = (1 - outcome.y)/(1 - outcome.x),
#     cf_calc1 = (1 - outcome_ub.y) / (1 - outcome_ub.x),
#     cf_calc2 = (1 - outcome_lb.y) / (1 - outcome_lb.x),
#     cf_lb = pmin(cf_calc1, cf_calc2),
#     cf_ub = pmax(cf_calc1, cf_calc2)
#   ) %>%
#   mutate(cf = cf/100,
#          cf_lb = cf_lb/100,
#          cf_ub = cf_ub/100) %>% # divide by 100 before next step
#   mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
#   mutate(outcome_ci.x = paste0("(", outcome_lb.x, ", ", outcome_ub.x, ")"),
#          outcome_ci.y = paste0("(", outcome_lb.y, ", ", outcome_ub.y, ")"),
#          cf_ci = paste0("(", cf_lb, ", ", cf_ub, ")"))
tabPred <- tabPredO %>%
  left_join(tabPredA, by = c("cstatus_agesp_comb", "deathrecency_cat"), suffix = c("_o", "_a")) %>%
  mutate(outcome_lb_o = ifelse(outcome_lb_o < 0, 0, outcome_lb_o),
         outcome_lb_a = ifelse(outcome_lb_a < 0, 0, outcome_lb_a),
         sens_upper_o = ifelse(sens_ub_o > 1 ,1 , sens_ub_o),
         sens_upper_a = ifelse(sens_ub_a > 1 ,1 , sens_ub_a)) %>%
  mutate(
    cf = (1 - outcome_a)/(1 - outcome_o),
    cf_calc1 = (1 - outcome_ub_a) / (1 - outcome_ub_o),
    cf_calc2 = (1 - outcome_lb_a) / (1 - outcome_lb_o),
    cf_lb = pmin(cf_calc1, cf_calc2),
    cf_ub = pmax(cf_calc1, cf_calc2)
  ) %>%
  mutate(cf = cf/100,
         cf_lb = cf_lb/100,
         cf_ub = cf_ub/100) %>% # divide by 100 before next step
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(outcome_ci_o = paste0("(", outcome_lb_o, ", ", outcome_ub_o, ")"),
         outcome_ci_a = paste0("(", outcome_lb_a, ", ", outcome_ub_a, ")"),
         cf_ci = paste0("(", cf_lb, ", ", cf_ub, ")"))
ft <- tabPred %>%
  dplyr::select(cstatus_agesp_comb, deathrecency_cat, 
                outcome_o, outcome_ci_o, outcome_a, outcome_ci_a, 
                cf, cf_ci) %>%
  flextable() %>%
  merge_v(j = ~ cstatus_agesp_comb + deathrecency_cat) %>%
  set_header_labels(values = c("Age-at-death", "Recall period of death (years)", "%", "CI", "%", "CI", "%", "CI")) %>%
  add_header_row(values = c(" ","Omission", "Addition", "Correction factor"), colwidths = c(2, 2, 2, 2)) %>%
  set_caption(caption = "Predicted probabilities of omission and addition and correction factors.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:8, part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft
doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)
output_path <- here::here("gen/figures", "table-correction-fac.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")




tabPred <- tabPredO %>%
  left_join(tabPredA, by = c("cstatus_agesp_comb", "deathrecency_cat"), suffix = c("_o", "_a")) %>%
  mutate(outcome_lb_o = ifelse(outcome_lb_o < 0, 0, outcome_lb_o),
         outcome_lb_a = ifelse(outcome_lb_a < 0, 0, outcome_lb_a),
         sens_upper_o = ifelse(sens_ub_o > 1 ,1 , sens_ub_o),
         sens_upper_a = ifelse(sens_ub_a > 1 ,1 , sens_ub_a)) %>%
  mutate(
    cf_o = 1/(1 - outcome_o),
    cf_calc1 = 1 / (1 - outcome_ub_o),
    cf_calc2 = 1 / (1 - outcome_lb_o),
    cfo_lb = pmin(cf_calc1, cf_calc2),
    cfo_ub = pmax(cf_calc1, cf_calc2),
    cf = (1 - outcome_a)/(1 - outcome_o),
    cf_calc1 = (1 - outcome_ub_a) / (1 - outcome_ub_o),
    cf_calc2 = (1 - outcome_lb_a) / (1 - outcome_lb_o),
    cf_lb = pmin(cf_calc1, cf_calc2),
    cf_ub = pmax(cf_calc1, cf_calc2)
  ) %>%
  mutate(cf_o = cf_o/100,
         cfo_lb = cfo_lb/100,
         cfo_ub = cfo_ub/100,
         cf = cf/100,
         cf_lb = cf_lb/100,
         cf_ub = cf_ub/100) %>% # divide by 100 before next step
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(outcome_ci_o = paste0("(", outcome_lb_o, ", ", outcome_ub_o, ")"),
         outcome_ci_a = paste0("(", outcome_lb_a, ", ", outcome_ub_a, ")"),
         cfo_ci = paste0("(", cfo_lb, ", ", cfo_ub, ")"),
         cf_ci = paste0("(", cf_lb, ", ", cf_ub, ")"))
ft <- tabPred %>%
  dplyr::select(cstatus_agesp_comb, deathrecency_cat, 
                outcome_o, outcome_ci_o, outcome_a, outcome_ci_a, 
                cf_o, cfo_ci,
                cf, cf_ci) %>%
  flextable() %>%
  merge_v(j = ~ cstatus_agesp_comb + deathrecency_cat) %>%
  set_header_labels(values = c("Age-at-death", "Recall period of death (years)", "%", "CI", "%", "CI", "%", "CI", "%", "CI")) %>%
  add_header_row(values = c(" ","Omission", "Addition", "Correction factor (omissions)", "Correction factor (omissions and additions)"), colwidths = c(2, 2, 2, 2, 2)) %>%
  set_caption(caption = "Predicted probabilities of omission and addition and correction factors.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:8, part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft
doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)
output_path <- here::here("gen/figures", "table-correction-fac.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")




tabPred <- tabPredO %>%
  left_join(tabPredA, by = c("cstatus_agesp_comb", "deathrecency_cat")) %>%
  mutate(outcome_lb.x = ifelse(outcome_lb.x < 0, 0, outcome_lb.x),
         outcome_lb.y = ifelse(outcome_lb.y < 0, 0, outcome_lb.y),
         sens_upper.x = ifelse(sens_ub.x > 1 ,1 , sens_ub.x),
         sens_upper.y = ifelse(sens_ub.y > 1 ,1 , sens_ub.y)) %>%
  # mutate(cf = (1 - outcome.y)/(1 - outcome.x),
  #       cf_lb = (1 - outcome_ub.y)/(1 - outcome_ub.x), 
  #       cf_ub = (1 - outcome_lb.y)/(1 - outcome_lb.x)) %>% 
  mutate(
    cf1 = 1/(1 - outcome.x),
    cf_calc1a = 1 / (1 - outcome_ub.x),
    cf_calc2b = 1 / (1 - outcome_lb.x),
    cf_lb1 = pmin(cf_calc1a, cf_calc2b),
    cf_ub1 = pmax(cf_calc1a, cf_calc2b),
    cf = (1 - outcome.y)/(1 - outcome.x),
    cf_calc1 = (1 - outcome_ub.y) / (1 - outcome_ub.x),
    cf_calc2 = (1 - outcome_lb.y) / (1 - outcome_lb.x),
    cf_lb = pmin(cf_calc1, cf_calc2),
    cf_ub = pmax(cf_calc1, cf_calc2)
  ) %>%
  mutate(cf1 = cf1/100,
         cf_lb1 = cf_lb1/100,
         cf_ub1 = cf_ub1/100,
         cf = cf/100,
         cf_lb = cf_lb/100,
         cf_ub = cf_ub/100) %>% # divide by 100 before next step
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(outcome_ci.x = paste0("(", outcome_lb.x, ", ", outcome_ub.x, ")"),
         outcome_ci.y = paste0("(", outcome_lb.y, ", ", outcome_ub.y, ")"),
         cf1_ci = paste0("(", cf_lb1, ", ", cf_ub1, ")"),
         cf_ci = paste0("(", cf_lb, ", ", cf_ub, ")"))

tabPred %>%
  filter(cstatus_agesp_comb == "5-9")

ft <- tabPred %>%
  dplyr::select(cstatus_agesp_comb, deathrecency_cat, 
                outcome.x, outcome_ci.x, outcome.y, outcome_ci.y, 
                cf1, cf1_ci,
                cf, cf_ci) %>%
  flextable() %>%
  merge_v(j = ~ cstatus_agesp_comb + deathrecency_cat) %>%
  set_header_labels(values = c("Age-at-death", "Recall period of death (years)", "%", "CI", "%", "CI", "%", "CI")) %>%
  add_header_row(values = c(" ","Omission", "Addition", "Correction factor"), colwidths = c(2, 2, 2, 2)) %>%
  set_caption(caption = "Predicted probabilities of omission and addition and correction factors.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:8, part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-correction-fac.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# PAA table ---------------------------------------------------------------

datDthyoung <- datDth %>% filter(!(cstatus_agesp_comb %in% c("5-9"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4"))) %>%
  mutate(cstrata_ac = factor(cstrata_ac, levels = c("Neonatal (other)", "Neonatal (birth asphyxia)",
                                                    "Postneonatal (other)", "Postneonatal (RI+con)",
                                                    "1-4 year (other)", "1-4 year (drowning)")) )

#m1 <- glm(omission ~ 1, data = datDth, family = binomial())
#m2 <- glm(omission ~ cstatus_agesp_comb, data = datDth, family = binomial())
m3 <- geeglm(omission ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth,
             id = rid_m, family = binomial(link = "logit"))
m4 <- geeglm(omission ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDthyoung,
             id = rid_m, family = binomial(link = "logit"))
m4b <- geeglm(omission ~ cstatus_agesp_comb + cod_cat + deathrecency_cat + magecat2_int, data = datDthyoung,
             id = rid_m, family = binomial(link = "logit"))
# test whether cause interacts with age (binary cause)
m5 <- glm(omission ~ cstatus_agesp_comb * cod_cat + deathrecency_cat + magecat2_int,
          data = datDthyoung, family = binomial())
m6 <- geeglm(omission ~ cstrata_ac + deathrecency_cat + magecat2_int, data = datDthyoung,
             id = rid_m, family = binomial(link = "logit"))


library(pROC)
probs <- predict(m3, datDth, type = "response")
roc_obj <- roc(datDth$omission, probs)
auc(roc_obj) # 0.686
probs <- predict(m4, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) #  0.683
probs <- predict(m4b, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) #  0.686
probs <- predict(m5, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) #  0.681
probs <- predict(m6, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) #  0.681

modelsO <- list(#"Model 1" = m1, 
                #"Model 2" = m2,
                "Model 3" = m3,
                "Model 4" = m4,
                "Model 4b" = m4b,
                "Model 5" = m5,
                "Model 6" = m6)
tabMod <- msummary(modelsO, output = "data.frame", stars = TRUE, fmt = 2, statistic = NULL)  %>%
  mutate(group = case_when(
    str_detect(term, "Intercept") ~ "Intercept",
    str_detect(term, "^cstatus_agesp_comb(?!.*×)") ~ "Age-at-death (ref: Neonatal)",
    str_detect(term, "deathrecency") ~ "Recall period of death (ref: 0-4 years)",
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




tabMod <- msummary(modelsA, output = "data.frame", stars = TRUE, fmt = 2, statistic = NULL) %>%
  mutate(group = case_when(
    str_detect(term, "Intercept") ~ "Intercept",
    str_detect(term, "^cstatus_agesp_comb(?!.*×)") ~ "Age-at-death (ref: Neonatal)",
    str_detect(term, "deathrecency") ~ "Recall period of death (ref: 0-4 years)",
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

tabMod <- tabModO %>% select(-c(term)) %>%
  full_join(tabModA %>% select(-c(term)), by = c("part", "statistic", "group", "level"))

ft <- tabMod %>%
  dplyr::select(group, level, `Model 3.x`, `Model 4`, `Model 4b`, `Model 5`, `Model 6`,`Model 3.y`) %>%
  rename(Variable = group) %>%
  rename(Value = level) %>%
  #filter(!(Variable %in% c("Cause-group of death (ref: other)", "Age of death  x Cause group"))) %>%
  flextable() %>%
  add_header_row(values = c(" ","Omission", "Addition"), colwidths = c(3, 4, 1)) %>%
  merge_v(j = ~ Variable + Value) %>%
  set_caption(caption = "Logistic regression") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures/paa", "regression2.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Multinomial reg: addition --------------------------------------------------

# Regression on addition of deaths

datDth <- dat %>%
  filter(denomC_multinom == 1 & 
           type %in% c("VS_Match", "VS_NoMatch", "HDSS_NoMatch")) %>%
  mutate(outcome = case_when(
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    type == "HDSS_NoMatch" ~ "Omission",
    TRUE ~ NA
  )) %>%
  mutate(outcome = factor(outcome, levels = c("Omission", "Addition", "Match"))) %>% # last one is reference in nomLORgee
  arrange(recnr) %>%
  as.data.frame()

# set reference categories
unique(datDth$deathrecency_cat)
datDth$deathrecency_cat <- factor(datDth$deathrecency_cat, levels = c("0-4", "5-9", "10-14"))
unique(datDth$cstatus_agesp_comb)
datDth$cstatus_agesp_comb <- factor(datDth$cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4", "5-9"))
datDth$rid_m <- as.factor(datDth$rid_m)

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(!(cstatus_agesp_comb %in% c("5-9"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4")))

# null model
m1 <- nomLORgee(outcome ~ 1,  id = rid_m, data = datDth)

coefs  <- coef(m1)
beta10 <- coefs["beta10"] # omission vs match
beta20 <- coefs["beta20"] # addition vs match
denom <- 1 + exp(beta10) + exp(beta20)
p_match    <- 1 / denom              # reference category
p_omission <- exp(beta10) / denom
p_addition <- exp(beta20) / denom
probs <- c(Match = p_match, Omission = p_omission, Addition = p_addition)
round(probs, 4)
prop.table(table(datDth$outcome))

# age is the main predictor of interest
m2 <- nomLORgee(outcome ~ cstatus_agesp_comb,  id = rid_m, data = datDth)

coefs <- coef(m2)

beta10 <- coefs["beta10"]
beta20 <- coefs["beta20"]
beta11 <- coefs["cstatus_agesp_combPostneonatal:1"]
beta21 <- coefs["cstatus_agesp_combPostneonatal:2"]
beta12 <- coefs["cstatus_agesp_comb1-4:1"]
beta22 <- coefs["cstatus_agesp_comb1-4:2"]

# Neonatal (reference)
denom_neo <- 1 + exp(beta10) + exp(beta20)
p_match_neo    <- 1 / denom_neo
p_omission_neo <- exp(beta10) / denom_neo
p_addition_neo <- exp(beta20) / denom_neo

# Postneonatal
denom_post <- 1 + exp(beta10 + beta11) + exp(beta20 + beta21)
p_match_post    <- 1 / denom_post
p_omission_post <- exp(beta10 + beta11) / denom_post
p_addition_post <- exp(beta20 + beta21) / denom_post

# 1-4
denom_14 <- 1 + exp(beta10 + beta12) + exp(beta20 + beta22)
p_match_14    <- 1 / denom_14
p_omission_14 <- exp(beta10 + beta12) / denom_14
p_addition_14 <- exp(beta20 + beta22) / denom_14

data.frame(
  age_group = c("Neonatal", "Postneonatal", "1-4"),
  Match     = c(p_match_neo, p_match_post, p_match_14),
  Omission  = c(p_omission_neo, p_omission_post, p_omission_14),
  Addition  = c(p_addition_neo, p_addition_post, p_addition_14)
)


# Multi addition: m3 ------------------------------------------------------


# add controls to age model - does age effect hold after adjustment?
m3 <- nomLORgee(outcome ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int,  id = rid_m, data = datDth)

coefs <- coef(m3)

beta10 <- coefs["beta10"]
beta20 <- coefs["beta20"]

# Helper: given additional beta1/beta2 contributions, compute the 3 probabilities
get_probs <- function(b1_add = 0, b2_add = 0) {
  denom <- 1 + exp(beta10 + b1_add) + exp(beta20 + b2_add)
  c(Match    = as.numeric(1 / denom),
    Omission = as.numeric(exp(beta10 + b1_add) / denom),
    Addition = as.numeric(exp(beta20 + b2_add) / denom))
}

# Pull out just the ":1" terms (Omission vs Match) to get predictor term names
terms1 <- names(coefs)[str_detect(names(coefs), ":1$") & !str_detect(names(coefs), "^beta")]

# Build a row for the reference category
ref_row <- data.frame(term = "Reference", t(get_probs()))

# Build a row for every non-reference term
other_rows <- map_dfr(terms1, function(t1) {
  t2 <- str_replace(t1, ":1$", ":2")
  probs <- get_probs(coefs[t1], coefs[t2])
  data.frame(term = str_remove(t1, ":1$"), t(probs))
})

probsTab <- bind_rows(ref_row, other_rows)
names(probsTab) <- c("term", "Match", "Omission", "Addition")
probsTab

probsTab <- probsTab %>%
  mutate(
    group = case_when(
      term == "Reference" ~ "Reference (Neonatal, 0-4 yrs recall, mother 15-24)",
      str_detect(term, "^cstatus_agesp_comb") ~ "Age-at-death (ref: Neonatal)",
      str_detect(term, "deathrecency") ~ "Recall period of death (ref: 0-4 years)",
      str_detect(term, "magecat2") ~ "Mother age (ref: 15-24)",
      TRUE ~ "Other"
    ),
    level = case_when(
      term == "Reference" ~ "Reference",
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
    )
  ) %>%
  select(group, level, Match, Omission, Addition)

probsTab

# Marginalize over mother's age ----------------------------------------------------


# fix both age-at-death and recall period, let mother's age vary as observed
coefs <- coef(m3)
form <- ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int
mm <- model.matrix(form, data = datDth)

beta1_names <- names(coefs)[str_detect(names(coefs), ":1$") | names(coefs) == "beta10"]
beta2_names <- names(coefs)[str_detect(names(coefs), ":2$") | names(coefs) == "beta20"]

beta1 <- coefs[beta1_names]
beta2 <- coefs[beta2_names]

names(beta1) <- str_remove(names(beta1), ":1$")
names(beta1)[names(beta1) == "beta10"] <- "(Intercept)"
names(beta2) <- str_remove(names(beta2), ":2$")
names(beta2)[names(beta2) == "beta20"] <- "(Intercept)"

beta1 <- beta1[colnames(mm)]
beta2 <- beta2[colnames(mm)]

get_marginal_probs_2way <- function(age_level, recency_level) {
  datCF <- datDth
  datCF$cstatus_agesp_comb <- factor(age_level,     levels = levels(datDth$cstatus_agesp_comb))
  datCF$deathrecency_cat   <- factor(recency_level, levels = levels(datDth$deathrecency_cat))
  # magecat2_int left as observed for every row -> averaged over in the mean() below
  
  mmCF <- model.matrix(form, data = datCF)
  mmCF <- mmCF[, colnames(mm)]
  
  lp1 <- mmCF %*% beta1
  lp2 <- mmCF %*% beta2
  denom <- 1 + exp(lp1) + exp(lp2)
  
  c(Match    = mean(1 / denom),
    Omission = mean(exp(lp1) / denom),
    Addition = mean(exp(lp2) / denom))
}

# Run across only the combinations you want
combos <- tribble(
  ~age_level,      ~recency_level,
  "Neonatal",      "0-4",
  "Neonatal",      "5-9",
  "Neonatal",      "10-14",
  "Postneonatal",  "0-4",
  "Postneonatal",  "5-9",
  "Postneonatal",  "10-14",
  "1-4",           "0-4",
  "1-4",           "5-9",
  "1-4",           "10-14",
  "5-9",           "0-4",
  "5-9",           "5-9",
  "5-9",           "10-14"
)

marginalTab <- pmap_dfr(combos, function(age_level, recency_level) {
  probs <- get_marginal_probs_2way(age_level, recency_level)
  data.frame(age_group = age_level, recall_period = recency_level, t(probs))
})

marginalTab


# marginalize with ci -----------------------------------------------------

coefs <- coef(m3)

# multgee stores the robust (sandwich) covariance matrix — check it exists
str(m3$robust.variance)  # should be a square matrix matching length(coefs)
vcovM <- m3$robust.variance

library(numDeriv)

form <- ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int
mm <- model.matrix(form, data = datDth)

beta1_names <- names(coefs)[str_detect(names(coefs), ":1$") | names(coefs) == "beta10"]
beta2_names <- names(coefs)[str_detect(names(coefs), ":2$") | names(coefs) == "beta20"]

# function of the FULL coefficient vector (named, same order as coefs)
# returns c(Match, Omission, Addition) for a given counterfactual dataset
pred_fun <- function(par, datCF) {
  b1 <- par[beta1_names]; names(b1) <- str_remove(names(b1), ":1$")
  names(b1)[names(b1) == "beta10"] <- "(Intercept)"
  b2 <- par[beta2_names]; names(b2) <- str_remove(names(b2), ":2$")
  names(b2)[names(b2) == "beta20"] <- "(Intercept)"
  
  b1 <- b1[colnames(mm)]
  b2 <- b2[colnames(mm)]
  
  mmCF <- model.matrix(form, data = datCF)
  mmCF <- mmCF[, colnames(mm)]
  
  lp1 <- mmCF %*% b1
  lp2 <- mmCF %*% b2
  denom <- 1 + exp(lp1) + exp(lp2)
  
  c(Match    = mean(1 / denom),
    Omission = mean(exp(lp1) / denom),
    Addition = mean(exp(lp2) / denom))
}

get_marginal_probs_ci <- function(age_level, recency_level) {
  datCF <- datDth
  datCF$cstatus_agesp_comb <- factor(age_level,     levels = levels(datDth$cstatus_agesp_comb))
  datCF$deathrecency_cat   <- factor(recency_level, levels = levels(datDth$deathrecency_cat))
  
  probs <- pred_fun(coefs, datCF)
  
  # numerical gradient: 3 x length(coefs) matrix
  J <- jacobian(function(par) pred_fun(par, datCF), coefs)
  
  varprobs <- J %*% vcovM %*% t(J)
  se <- sqrt(diag(varprobs))
  
  data.frame(
    outcome = c("Match", "Omission", "Addition"),
    est     = probs,
    se      = se,
    ci_low  = pmax(probs - 1.96 * se, 0),
    ci_high = pmin(probs + 1.96 * se, 1)
  )
}

combos <- tribble(
  ~age_level,      ~recency_level,
  "Neonatal",      "0-4",
  "Neonatal",      "5-9",
  "Neonatal",      "10-14",
  "Postneonatal",  "0-4",
  "Postneonatal",  "5-9",
  "Postneonatal",  "10-14",
  "1-4",           "0-4",
  "1-4",           "5-9",
  "1-4",           "10-14",
  "5-9",           "0-4",
  "5-9",           "5-9",
  "5-9",           "10-14"
)

marginalCI <- pmap_dfr(combos, function(age_level, recency_level) {
  res <- get_marginal_probs_ci(age_level, recency_level)
  res$age_group <- age_level
  res$recall_period <- recency_level
  res
})

# wide: one column per outcome, with a "point estimate" row and "CI" row per combo
wideCI <- marginalCI %>%
  mutate(
    point = sprintf("%.2f", est),
    ci    = sprintf("(%.2f, %.2f)", ci_low, ci_high)
  ) %>%
  select(age_group, recall_period, outcome, point, ci) %>%
  pivot_longer(cols = c(point, ci), names_to = "rowtype", values_to = "value") %>%
  pivot_wider(names_from = outcome, values_from = value) %>%
  arrange(age_group, recall_period, desc(rowtype == "point")) %>%
  mutate(age_group = factor(age_group, levels = c("Neonatal", "Postneonatal", "1-4", "5-9")),
         recall_period = factor(recall_period, levels = c("0-4", "5-9", "10-14")),
         rowtype = factor(rowtype, levels = c("point", "ci"))) %>%
  arrange(age_group, recall_period, rowtype)

wideCI


# flex table --------------------------------------------------------------------

library(dplyr)
library(stringr)
library(tidyr)
library(flextable)

tidy_nomLORgee <- function(model, model_name) {
  s <- summary(model)$coefficients
  df <- as.data.frame(s)
  df$term_raw <- rownames(df)
  names(df) <- c("estimate", "se", "z", "p", "term_raw")
  
  df <- df %>%
    mutate(
      contrast = case_when(
        term_raw == "beta10" | str_detect(term_raw, ":1$") ~ "Omission vs Match",
        term_raw == "beta20" | str_detect(term_raw, ":2$") ~ "Addition vs Match",
        TRUE ~ NA_character_
      ),
      term = str_remove(term_raw, ":1$|:2$"),
      term = case_when(
        term_raw == "beta10" ~ "(Intercept)",
        term_raw == "beta20" ~ "(Intercept)",
        TRUE ~ term
      ),
      or = exp(estimate),
      stars = case_when(
        p < .001 ~ "***",
        p < .01  ~ "**",
        p < .05  ~ "*",
        p < .1   ~ "+",
        TRUE ~ ""
      ),
      model = model_name
    )
  df
}

tidyM1 <- tidy_nomLORgee(m1, "Model 1")
tidyM2 <- tidy_nomLORgee(m2, "Model 2")
tidyM3 <- tidy_nomLORgee(m3, "Model 3")

allTidy <- bind_rows(tidyM1, tidyM2, tidyM3)

# add group labels
allTidy <- allTidy %>%
  mutate(
    group = case_when(
      term == "(Intercept)" ~ "Intercept",
      str_detect(term, "^cstatus_agesp_comb") ~ "Age-at-death (ref: Neonatal)",
      str_detect(term, "deathrecency") ~ "Recall period of death (ref: 0-4 years)",
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
    )
  )

# pivot and separate OR and SE columns
wideOR <- allTidy %>%
  filter(!is.na(contrast)) %>%
  mutate(col = paste(model, contrast, sep = " - "),
         cellval = sprintf("%.2f%s", or, stars)) %>%
  select(group, level, col, cellval) %>%
  pivot_wider(names_from = col, values_from = cellval)

wideSE <- allTidy %>%
  filter(!is.na(contrast)) %>%
  mutate(col = paste(model, contrast, sep = " - "),
         cellval = sprintf("(%.2f)", se)) %>%
  select(group, level, col, cellval) %>%
  pivot_wider(names_from = col, values_from = cellval)

col_order <- c("Model 1 - Omission vs Match", "Model 1 - Addition vs Match",
               "Model 2 - Omission vs Match", "Model 2 - Addition vs Match",
               "Model 3 - Omission vs Match", "Model 3 - Addition vs Match")

wideOR <- wideOR %>% select(group, level, any_of(col_order))
wideSE <- wideSE %>% select(group, level, any_of(col_order))

# preserve group order as they first appear
group_order <- unique(allTidy$group)

build_stacked <- function(g) {
  or_rows <- wideOR %>% filter(group == g)
  se_rows <- wideSE %>% filter(group == g)
  
  # group header row: group name in Variable, blank Value, blank everywhere else
  header_row <- as.data.frame(matrix("", nrow = 1, ncol = ncol(or_rows)))
  names(header_row) <- names(or_rows)
  header_row$group <- g
  header_row$level <- ""
  
  out <- header_row
  for (i in seq_len(nrow(or_rows))) {
    coef_row <- or_rows[i, ]
    coef_row$group <- ""
    coef_row$level <- paste0("   ", coef_row$level)  # indent
    
    se_row <- se_rows[i, ]
    se_row$group <- ""
    se_row$level <- ""
    
    out <- bind_rows(out, coef_row, se_row)
  }
  out
}

stackedTab <- purrr::map_dfr(group_order, build_stacked)

# flex table
ftOR <- stackedTab %>%
  rename(Variable = group, Value = level) %>%
  flextable() %>%
  set_caption(caption = "Multinomial GEE: Odds ratios (SE) for outcome relative to Match") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  bold(i = ~ Value == "", j = "Variable", bold = TRUE) %>%
  add_footer_lines("OR (SE). + p<0.1, * p<0.05, ** p<0.01, *** p<0.001") %>%
  autofit()

ftOR


#' OR = 0.61 for Postneonatal, Omission vs. Match

# This says: compared to neonatal deaths, postneonatal deaths have 61% of the odds of being an Omission (rather than a Match) in the FPH — holding other covariates in the model constant.
# 
# Since 0.61 < 1, this means the odds of omission relative to a correct match are lower for postneonatal deaths than neonatal deaths. In other words: neonatal deaths are more likely to be omitted from the FPH (relative to being correctly matched) than postneonatal deaths are.
# 
# You could express it as: "the odds of a death being omitted from the FPH (rather than correctly matched) were 39% lower (OR = 0.61) for postneonatal deaths compared to neonatal deaths."
# 
# OR = 0.52 for Postneonatal, Addition vs. Match
# 
# Same logic, different contrast: compared to neonatal deaths, postneonatal deaths have 52% of the odds of being an Addition (rather than a Match).
# 
# So: "the odds of a death being an addition to the FPH (i.e., appearing in the FPH without a matching DSS record, rather than being correctly matched) were 48% lower (OR = 0.52) for postneonatal deaths compared to neonatal deaths."
# 
# Comparing the two (0.61 vs. 0.52)
# 
# Both coefficients point the same direction — postneonatal deaths are less prone to both types of discrepancy (omission and addition) relative to neonatal deaths, when each is compared against a correct match. The postneonatal reduction is somewhat larger for Addition (48% lower odds) than for Omission (39% lower odds), meaning the age-at-death gradient is a bit steeper for the addition-type discrepancy than the omission-type one. But you'd want to check whether that difference between 0.61 and 0.52 is itself statistically meaningful — these are two separate contrasts from the same model, not directly tested against each other, so the difference in magnitude is descriptive unless you formally test it (e.g., via a Wald test comparing the two coefficients, or refitting with a different reference to get that direct contrast).

