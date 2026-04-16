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

# Assign denominator: 
# D - Live births in DSS, only matched live births in FPH
dat <- overall %>%
  mutate(denomD = ifelse((!is.na(pregout_dss) & pregout_dss == "Live birth")  |
                           (!is.na(c223) & c223 == "Live birth" & type == "VS_Match"), 1, 0)
  )


# Regression: omission of death ---------------------------------------

# Denominator is deaths in DSS
# No need to limit it to C (women with ongoing residency in past 10 years)
# because here we are using DSS as reference and just seeing if reported in FPH
datDth <- dat %>%
  filter(denomD == 1 & cstatus_dss == "Died" & cstatus_agesp_dss != "10+") %>%  
  mutate(omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  mutate(cod_cat = ifelse(cstrata_c %in% c("Drowning", "Birth asphyxia", "RI and congenital"), "leading", "other"))

# set reference categories
unique(datDth$cstatus_agesp_dss)
datDth$cstatus_agesp_dss <- factor(datDth$cstatus_agesp_dss, levels = c("Neonatal", "Postneonatal", "1-4", "5-9")) #, "10+"))
unique(datDth$cstrata_ac)
datDth$cstrata_ac <- factor(datDth$cstrata_ac, 
                            levels = c("Neonatal (other)" , "Neonatal (birth asphyxia)", 
                                       "Postneonatal (other)", "Postneonatal (RI+con)", 
                                       "1-4 year (other)", "1-4 year (drowning)" ,
                                       "5-9 year")) #, "10+"))
datDth$cod_cat <- factor(datDth$cod_cat, levels = c("other", "leading"))

# dataset without 5-9 for when grouping leading and other
datDthyoung <- datDth %>% filter(cstatus_agesp_dss %in% c("5-9", "10+")) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, levels = c("Neonatal", "Postneonatal", "1-4")))

# null model
m1 <- glm(omission ~ 1, data = subset(datDth, !is.na(cstatus_agesp_dss)), family = binomial())
# age is the main predictor of interest
m2 <- glm(omission ~ cstatus_agesp_dss, data = datDth, family = binomial())
# add controls to age model - does age effect hold after adjustment?
m3 <- glm(omission ~ cstatus_agesp_dss + birthrecency_cat + magecat2_int, data = datDth, family = binomial())
# but lets use birthrecency because it matches better with how to correct a survey
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
    term == "cstatus_agesp_dss10+" ~ "10+ years",
    term == "cstrata_acNeonatal (birth asphyxia)" ~ "Neonatal - birth asphyxia",
    term == "cstrata_acPostneonatal (RI+con)" ~ "Postneonatal - RI+con",
    term == "cstrata_acPostneonatal (other)" ~ "Postneonatal - other",
    term == "cstrata_ac1-4 year (drowning)" ~ "1-4 years - drowning",
    term == "cstrata_ac1-4 year (other)" ~ "1-4 years - other",
    term == "cstrata_ac5-9 year" ~ "5-9 years",
    term == "cstrata_ac10+" ~ "10+ years",
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
  set_caption(caption = "Logistic regression on DSS deaths omitted from FPH") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() 
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-regression-dths-omissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


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

#make_table(results_age, "cstatus_agesp_dss")

myres <- make_table(results_agebr, c("cstatus_agesp_dss", "birthrecency_cat"))
myres <- myres %>% 
  mutate(omission_lower = ifelse(omission_lower < 0, 0, omission_lower),
         sens_upper = ifelse(sens_upper >1 ,1 , sens_upper)) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.2f", .x * 100))) %>%
  mutate(omission_bound = paste0("(", omission_lower, ", ", omission_upper, ")"))%>%
  mutate(sens_bound = paste0("(", sens_lower, ", ", sens_upper, ")")) %>%
  dplyr::select(cstatus_agesp_dss, birthrecency_cat, omission, omission_bound, sensitivity, sens_bound)


ft <- myres %>%
  dplyr::select(cstatus_agesp_dss, birthrecency_cat, omission, omission_bound, sensitivity, sens_bound) %>%
  filter(birthrecency_cat != "15+") %>%
  flextable() %>%
  merge_v(j = ~ cstatus_agesp_dss + birthrecency_cat) %>%
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

