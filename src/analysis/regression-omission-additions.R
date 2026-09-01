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
library(gt)
#' Inputs
overall <- readRDS("./gen/augment/overallName-recode.rds")
################################################################################

# initially was doing omissions for subsample A (all-women) with denominator of deaths in DSS
# now doing multinomial for subsample C (recent-births)
# Exclusions: (we don't want to estimate a coefficient for)
# deaths taking place 15+ years ago (because no need for correct factor given survey practice of calculating rates 0-4, 5-9, 10-14y)
# children 10+ (because data is too sparse)
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
         denomA = ifelse(subsampA == 1 & eventDth_dss == 1 
                         #& happenedRecently == 1 & youngerChildren == 1
                           , 1, 0),
         denomC = ifelse((subsampC_dss == 1 | subsampC_sur == 1) & eventDth_sur == 1, 1, 0),
         denomC_multinom = ifelse((subsampC_dss == 1 | subsampC_sur == 1) & 
                                    (eventDth_sur == 1 | eventDth_dss == 1), 1, 0))


# Logistic ----------------------------------------------------------------

# filter out additions so it is only omission or match
datDth <- dat %>%
  filter(denomA == 1) %>%  
  filter(type %in% c("HDSS_NoMatch", "VS_Match")) %>%
  mutate(omission = ifelse(type == "HDSS_NoMatch", 1, 0)) %>%
  mutate(cod_cat = ifelse(cstrata_c %in% c("Drowning", "Birth asphyxia", "RI and congenital"), "leading", "other"))

# set reference categories
unique(datDth$deathrecency_cat) #  0-4 5-9 10-14  
datDth$deathrecency_cat <- factor(datDth$deathrecency_cat, levels = c("0-4", "5-9", "10-14", "15+"))
unique(datDth$cstatus_agesp_comb)
table(datDth$cstatus_agesp_comb)
datDth$cstatus_agesp_comb <- factor(datDth$cstatus_agesp_comb, 
                                    levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"))
unique(datDth$cstrata_ac)
datDth$cstrata_ac <- factor(datDth$cstrata_ac, 
                            levels = c("Neonatal (other)" , "Neonatal (birth asphyxia)", 
                                       "Postneonatal (other)", "Postneonatal (RI+con)", 
                                       "1-4 years (other)", 
                                       "1-4 years (drowning)" ,
                                       "5-9 years", "10+ years"))
datDth$cod_cat <- factor(datDth$cod_cat, levels = c("other", "leading"))
datDth$rid_m <- as.factor(datDth$rid_m)

nrow(subset(datDth,  is.na(omission)))
nrow(subset(datDth,  is.na(cod_cat)))
nrow(subset(datDth,  is.na(deathrecency_cat)))
nrow(subset(datDth,  is.na(cstatus_agesp_comb)))
nrow(subset(datDth,  is.na(cstrata_ac)))

# dataset without 5-9 for when grouping leading and other
# also drop events 15+ years ago
datDthyoung <- datDth %>% 
  filter(!(cstatus_agesp_comb %in% c("10+ years"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, 
                                     levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years"))) %>%
  filter(!(deathrecency_cat %in% c("15+"))) %>%
  mutate(deathrecency_cat = factor(deathrecency_cat, levels = c("0-4", "5-9", "10-14")))
datDthyounger <- datDth %>% 
  filter(!(cstatus_agesp_comb %in% c("5-9 years", "10+ years"))) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4 years"))) %>%
  filter(!(deathrecency_cat %in% c("15+"))) %>%
  mutate(deathrecency_cat = factor(deathrecency_cat, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(cstrata_ac = factor(cstrata_ac, levels = c("Neonatal (other)", "Neonatal (birth asphyxia)",
                                                    "Postneonatal (other)", "Postneonatal (RI+con)",
                                                    "1-4 years (other)", "1-4 years (drowning)")))

# null model
m0 <- glm(omission ~ 1, data = datDthyoung, family = binomial())
summary(m0)
m1 <- glm(omission ~ cstatus_agesp_comb, data = datDthyoung, family = binomial())
summary(m1)
# age is the main predictor of interest
m2 <- glm(omission ~ deathrecency_cat, data = datDthyoung, family = binomial())
summary(m2)
# add controls to age model - does age effect hold after adjustment?
#m3 <- glm(omission ~ cstatus_agesp_comb + deathrecency_cat + magecat2_int, data = datDth, family = binomial())
m3 <- geeglm(omission ~ cstatus_agesp_comb + deathrecency_cat, data = datDthyoung,
             id = rid_m, family = binomial(link = "logit"))
summary(m3)
# add cause to adjusted age model - does cause add anything?
# m4 <- glm(omission ~ cstatus_agesp_comb + cod_cat + deathrecency_cat + magecat2_int, 
#           data = datDth, family = binomial())
m4 <- geeglm(omission ~ cstatus_agesp_comb + cod_cat, data = datDthyoung,
             id = rid_m, family = binomial(link = "logit"))
summary(m4)
# test whether cause interacts with age (binary cause)
m5 <- glm(omission ~ cstatus_agesp_comb * cod_cat,
          data = datDthyounger, family = binomial())
summary(m5)
# m5 <- geeglm(omission ~ cstatus_agesp_comb * cod_cat + deathrecency_cat + magecat2_int, data = datDth,
#              id = rid_m, family = binomial(link = "logit"))
# test whether specific causes within age groups matter
# m6 <- glm(omission ~ cstrata_ac + deathrecency_cat + magecat2_int, 
#           data = datDth, family = binomial())
m6 <- geeglm(omission ~ cstrata_ac, data = datDthyounger,
             id = rid_m, family = binomial(link = "logit"))
summary(m6)

library(pROC)
probs <- predict(m1, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) # 0.5
probs <- predict(m2, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) # 0.5998
probs <- predict(m3, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) # 0.6859
probs <- predict(m4, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) #  0.6903
probs <- predict(m5, datDthyoung, type = "response")
roc_obj <- roc(datDthyoung$omission, probs)
auc(roc_obj) #  0.6859
probs <- predict(m6, datDthyounger, type = "response")
roc_obj <- roc(datDthyounger$omission, probs)
auc(roc_obj) #  0.689



#anova(m1, m2, test = "Chisq")  # does age matter?
#anova(m2, m3, test = "Chisq")  # do controls matter?
#anova(m3, m4, test = "Chisq")  # does cause add anything after age + controls? no
#anova(m4, m5, test = "Chisq")  # does cause interact with age? no
#anova(m3, m6, test = "Chisq")  # do specific cause*age cells add anything beyond age + controls? no
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
    str_detect(term, "magecat3") ~ "Mother age (ref: 15-24)",
    str_detect(term, "cstatus_agesp_combPostneonatal × cod_catleading") ~ 
      "Age of death  x Cause group",
    str_detect(term, "cstatus_agesp_comb1-4 years × cod_catleading") ~ 
      "Age of death  x Cause group",
    str_detect(term, "cod_catleading") ~ "Cause-group of death (ref: other)",
    str_detect(term, "cstrata_ac") ~ "Cause of death (ref: Neonatal - other)",
    TRUE ~ "Other"
  ),
  level = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "deathrecency_cat5-9" ~ "5-9",
    term == "deathrecency_cat10-14" ~ "10-14",
    term == "magecat3_int25-34" ~ "25-34",
    term == "magecat3_int35+" ~ "35+",
    term == "cstatus_agesp_combPostneonatal" ~ "Postneonatal",
    term == "cstatus_agesp_comb1-4 years" ~ "1-4 years",
    term == "cstatus_agesp_comb5-9 years" ~ "5-9 years",
    term == "cstatus_agesp_comb10+ years" ~ "10+ years",
    term == "cstrata_acNeonatal (birth asphyxia)" ~ "Neonatal - birth asphyxia",
    term == "cstrata_acPostneonatal (RI+con)" ~ "Postneonatal - RI+con",
    term == "cstrata_acPostneonatal (other)" ~ "Postneonatal - other",
    term == "cstrata_ac1-4 years (drowning)" ~ "1-4 years - drowning",
    term == "cstrata_ac1-4 years (other)" ~ "1-4 years - other",
    term == "cstrata_ac5-9 years" ~ "5-9 years",
    term == "cstrata_ac10+ years" ~ "10+ years",
    term == "cod_catleading" ~ "Leading cause",
    term == "cstatus_agesp_combPostneonatal × cod_catleading" ~ "Postneonatal x leading cause",
    term == "cstatus_agesp_comb1-4 years × cod_catleading" ~ "1-4 years x leading cause",
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

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-regression-dths.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Multinomial -------------------------------------------------------------

# Fit model
moddat <- dat %>%
  filter(denomC_multinom == 1 & 
           type %in% c("VS_Match", "VS_NoMatch", "HDSS_NoMatch")) %>%
  mutate(outcome = case_when(
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    type == "HDSS_NoMatch" ~ "Omission",
    TRUE ~ NA
  )) %>%
  arrange(recnr) %>%
  as.data.frame()

# set outcome reference category
# last one is reference in nomLORgee
moddat$outcome <- factor(moddat$outcome, levels = c("Omission", "Addition", "Match"))

# set covariate reference categories
# first one is reference
unique(moddat$deathrecency_cat)
moddat$deathrecency_cat <- factor(moddat$deathrecency_cat, levels = c("0-4", "5-9", "10-14"))
unique(moddat$cstatus_agesp_comb)
moddat$cstatus_agesp_comb <- factor(moddat$cstatus_agesp_comb, levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years"))
moddat$rid_m <- as.factor(moddat$rid_m)

m1 <- nomLORgee(outcome ~ 1,  id = rid_m, data = moddat)
m2 <- nomLORgee(outcome ~ cstatus_agesp_comb,  id = rid_m, data = moddat)
m3 <- nomLORgee(outcome ~ cstatus_agesp_comb + deathrecency_cat,  id = rid_m, data = moddat)

table(moddat$cstatus_agesp_comb, moddat$outcome)
table(moddat$cstatus_agesp_comb, moddat$deathrecency_cat, moddat$outcome)

# "Models were fit assuming additive effects of age-at-death and recall period on the log-odds scale; sparse cell counts precluded reliable estimation of an interaction term."

# Multi regression table --------------------------------------------------------

add_group_level <- function(tab, model) {
  predictor_vars <- all.vars(formula(model))[-1]
  predictor_vars <- predictor_vars[predictor_vars != "1"]
  
  if (length(predictor_vars) == 0) {
    tab$group <- "Reference"
    tab$level <- "Reference"
    return(tab)
  }
  
  vars_sorted <- predictor_vars[order(-nchar(predictor_vars))]
  
  parse_one <- function(term) {
    if (term == "Reference") return(c(group = "Reference", level = "Reference"))
    hit <- vars_sorted[str_starts(term, vars_sorted)][1]
    if (is.na(hit)) return(c(group = "Other", level = term))
    c(group = hit, level = str_remove(term, fixed(hit)))
  }
  
  parsed <- map_dfr(tab$term, ~ as.data.frame(t(parse_one(.x))))
  bind_cols(tab, parsed)
}

get_rrr_table <- function(model, model_name) {
  s <- summary(model)$coefficients
  coef_tab <- as.data.frame(s)
  coef_tab$term_raw <- rownames(coef_tab)
  rownames(coef_tab) <- NULL
  
  coef_tab <- coef_tab %>%
    rename(estimate = Estimate, se = `san.se`, pval = `Pr(>|san.z|)`) %>%
    filter(!str_detect(term_raw, "^beta")) %>%
    mutate(
      eqn  = case_when(
        str_detect(term_raw, ":1$") ~ "Omission vs Match",
        str_detect(term_raw, ":2$") ~ "Addition vs Match"
      ),
      term = str_remove(term_raw, ":[12]$"),
      RRR  = exp(estimate),
      lci  = exp(estimate - 1.96 * se),
      uci  = exp(estimate + 1.96 * se),
      rrr_ci = sprintf("%.2f (%.2f\u2013%.2f)", RRR, lci, uci),
      model = model_name
    )
  
  coef_tab %>% add_group_level(model) %>%
    select(model, group, level, eqn, rrr_ci, pval)
}

models <- list(m2 = m2, m3 = m3)
rrr_all <- imap_dfr(models, ~ get_rrr_table(.x, .y))
rrr_all

rrr_stacked <- rrr_all %>%
  select(model, group, level, eqn, rrr_ci) %>%
  pivot_wider(names_from = eqn, values_from = rrr_ci)
rrr_stacked

# RRR < 1 means that predictor category is less likely to result in Omission/Addition (relative to match) relative to reference category
# RRR > 1 means more likely to result in Omission/Addition, relative to match relative to reference category
rrr_stacked %>%
  gt(groupname_col = "model") %>%
  cols_label(group = "Variable", level = "") %>%
  sub_missing(missing_text = "\u2014") %>%
  tab_footnote("Reference outcome: Match. RRR (95% CI) shown.")

# Multi predicted probs -------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

get_predicted_probs_grid <- function(model, model_name, nsim = 2000, seed = 123) {
  set.seed(seed)
  coefs <- coef(model)
  
  # multgee stores robust covariance - try vcov() first, fall back if needed
  vc <- tryCatch(vcov(model), error = function(e) model$robust.variance)
  
  beta10_name <- "beta10"
  beta20_name <- "beta20"
  
  # identify the two predictor variables and their levels from the model formula
  predictor_vars <- all.vars(formula(model))[-1]
  predictor_vars <- predictor_vars[predictor_vars != "1"]
  
  # pull factor levels straight from the model's data
  mf <- model.frame(model)
  var_levels <- map(predictor_vars, function(v) levels(as.factor(mf[[v]])))
  names(var_levels) <- predictor_vars
  
  # all combinations of levels across the predictors
  grid <- expand.grid(var_levels, stringsAsFactors = FALSE)
  
  # helper: find the coefficient name matching a given var + level, for eqn 1 or 2
  get_term_name <- function(var, level, eqn) {
    ref_level <- var_levels[[var]][1]
    if (level == ref_level) return(NA_character_)  # reference = no term, contributes 0
    paste0(var, level, ":", eqn)
  }
  
  # simulate coefficient draws
  sim_coefs <- MASS::mvrnorm(nsim, mu = coefs, Sigma = vc)
  
  # for each grid row, compute lp1/lp2 for every simulated draw, then summarize
  results <- pmap_dfr(grid, function(...) {
    row <- list(...)
    terms1 <- map_chr(predictor_vars, ~ get_term_name(.x, row[[.x]], 1))
    terms2 <- map_chr(predictor_vars, ~ get_term_name(.x, row[[.x]], 2))
    terms1 <- terms1[!is.na(terms1)]
    terms2 <- terms2[!is.na(terms2)]
    
    lp1 <- sim_coefs[, beta10_name] + rowSums(sim_coefs[, terms1, drop = FALSE])
    lp2 <- sim_coefs[, beta20_name] + rowSums(sim_coefs[, terms2, drop = FALSE])
    
    denom <- 1 + exp(lp1) + exp(lp2)
    p_match    <- 1 / denom
    p_omission <- exp(lp1) / denom
    p_addition <- exp(lp2) / denom
    
    tibble(
      !!!row,
      model = model_name,
      Match_est    = mean(p_match),    Match_lci    = quantile(p_match, .025),    Match_uci    = quantile(p_match, .975),
      Omission_est = mean(p_omission), Omission_lci = quantile(p_omission, .025), Omission_uci = quantile(p_omission, .975),
      Addition_est = mean(p_addition), Addition_lci = quantile(p_addition, .025), Addition_uci = quantile(p_addition, .975)
    )
  })
  
  results
}


# multi predicted probs for age -------------------------------------------

probs_grid_m2 <- get_predicted_probs_grid(m2, "m2")

probs_grid_tab <- probs_grid_m2 %>%
  mutate(
    CF1         = sprintf("%.2f", round(1/(1-Omission_est), 2)),
    CF1_lci     = sprintf("%.2f", round(1/(1-Omission_lci), 2)),
    CF1_uci     = sprintf("%.2f", round(1/(1-Omission_uci), 2)),
    CF2         = sprintf("%.2f", round((1-Addition_est)/(1-Omission_est), 2)),
    CF2_lci     = sprintf("%.2f", round((1-Addition_lci)/(1-Omission_lci), 2)),
    CF2_uci     = sprintf("%.2f", round((1-Addition_uci)/(1-Omission_uci), 2)),
    Match_est    = sprintf("%.2f", round(Match_est*100, 2)),
    Match_lci    = sprintf("%.2f",round(Match_lci*100, 2)),
    Match_uci    = sprintf("%.2f",round(Match_uci*100, 2)),
    Omission_est = sprintf("%.2f",round(Omission_est*100, 2)),
    Omission_lci = sprintf("%.2f",round(Omission_lci*100, 2)),
    Omission_uci = sprintf("%.2f",round(Omission_uci*100, 2)),
    Addition_est = sprintf("%.2f",round(Addition_est*100, 2)),
    Addition_lci = sprintf("%.2f",round(Addition_lci*100, 2)),
    Addition_uci = sprintf("%.2f",round(Addition_uci*100, 2)),
    Match_ci = paste0("(", Match_lci, ", ", Match_uci, ")"),
    Omission_ci = paste0("(", Omission_lci, ", ", Omission_uci, ")"),
    Addition_ci = paste0("(", Addition_lci, ", ", Addition_uci, ")"),
    CF1_ci = paste0("(", CF1_lci, ", ", CF1_uci, ")"),
    CF2_ci = paste0("(", CF2_lci, ", ", CF2_uci, ")")
  ) 
probs_grid_coef <-  probs_grid_tab %>%
  dplyr::select(cstatus_agesp_comb, Match_est, Omission_est, Addition_est,
                CF1, CF2) %>%
  mutate(rank = "A")
probs_grid_ci <-  probs_grid_tab %>%
  dplyr::select(cstatus_agesp_comb, Match_ci,  Omission_ci, Addition_ci,
                CF1_ci, CF2_ci) %>%
  mutate(rank = "B") %>%
  rename(Match_est = Match_ci,
         Omission_est = Omission_ci,
         Addition_est = Addition_ci,
         CF1 = CF1_ci,
         CF2 = CF2_ci)

probs_grid_dsply <- probs_grid_coef %>%
  bind_rows(probs_grid_ci) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, 
                                     levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years"))) %>%
  arrange(cstatus_agesp_comb, rank) %>%
  select(-rank)

table_with_headers <- probs_grid_dsply %>%
  group_split(cstatus_agesp_comb, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      cstatus_agesp_comb = unique(df$cstatus_agesp_comb),
      Match_est = "",
      Omission_est = "", 
      Addition_est = "",
      CF1 = "",
      CF2 = "")
    bind_rows(header_row, df)
  })
table_with_headers$cstatus_agesp_comb <- as.character(table_with_headers$cstatus_agesp_comb)
table_with_headers$cstatus_agesp_comb[table_with_headers$Match_est != ""] <- ""

ft <- table_with_headers %>%
  flextable() %>%
  set_header_labels(values = c("Age-at-death", "% match (95% CI)",
                               "% omission (95% CI)", "% addition (95% CI)",
                               "CF 1 (95% CI)", "CF 2 (95% CI)")) %>%
  set_caption(caption = "Predicted probabilities of omission and addition by age group and recall period of death from the final models, with corresponding correction factors (CF) and 95% confidence intervals (CI).") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "center", j = c(2:6), part = "all") %>%
  align(align = "left", j = c(1), part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-multi-regression-age.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# multi predicted probs for age/period ------------------------------------



# To obtain 95% confidence intervals for predicted probabilities, we used a parametric bootstrap (simulation-based) approach. Because predicted probabilities are a nonlinear transformation of the model's log-odds coefficients, standard errors from the model cannot be directly translated into valid confidence intervals for probabilities. We therefore drew 2,000 simulated coefficient sets from a multivariate normal distribution defined by the model's estimated coefficients and their covariance matrix, calculated the predicted probability for each simulated draw, and took the 2.5th and 97.5th percentiles of the resulting distribution as the 95% confidence interval
# Predicted probabilities with 95% CIs derived via parametric bootstrap (2,000 simulations from the model's coefficient covariance matrix).

probs_grid_m3 <- get_predicted_probs_grid(m3, "m3")

probs_grid_tab <- probs_grid_m3 %>%
  mutate(
    CF1         = sprintf("%.2f", round(1/(1-Omission_est), 2)),
    CF1_lci     = sprintf("%.2f", round(1/(1-Omission_lci), 2)),
    CF1_uci     = sprintf("%.2f", round(1/(1-Omission_uci), 2)),
    CF2         = sprintf("%.2f", round((1-Addition_est)/(1-Omission_est), 2)),
    CF2_lci     = sprintf("%.2f", round((1-Addition_lci)/(1-Omission_lci), 2)),
    CF2_uci     = sprintf("%.2f", round((1-Addition_uci)/(1-Omission_uci), 2)),
    Match_est    = sprintf("%.2f", round(Match_est*100, 2)),
    Match_lci    = sprintf("%.2f",round(Match_lci*100, 2)),
    Match_uci    = sprintf("%.2f",round(Match_uci*100, 2)),
    Omission_est = sprintf("%.2f",round(Omission_est*100, 2)),
    Omission_lci = sprintf("%.2f",round(Omission_lci*100, 2)),
    Omission_uci = sprintf("%.2f",round(Omission_uci*100, 2)),
    Addition_est = sprintf("%.2f",round(Addition_est*100, 2)),
    Addition_lci = sprintf("%.2f",round(Addition_lci*100, 2)),
    Addition_uci = sprintf("%.2f",round(Addition_uci*100, 2)),
    Match_ci = paste0("(", Match_lci, ", ", Match_uci, ")"),
    Omission_ci = paste0("(", Omission_lci, ", ", Omission_uci, ")"),
    Addition_ci = paste0("(", Addition_lci, ", ", Addition_uci, ")"),
    CF1_ci = paste0("(", CF1_lci, ", ", CF1_uci, ")"),
    CF2_ci = paste0("(", CF2_lci, ", ", CF2_uci, ")")
  ) 

probs_grid_coef <-  probs_grid_tab %>%
  dplyr::select(cstatus_agesp_comb, deathrecency_cat,
                Match_est, Omission_est, Addition_est,
                CF1, CF2) %>%
  mutate(rank = "A")
probs_grid_ci <-  probs_grid_tab %>%
  dplyr::select(cstatus_agesp_comb,  deathrecency_cat,
                Match_ci,  Omission_ci, Addition_ci,
                CF1_ci, CF2_ci) %>%
  mutate(rank = "B") %>%
  rename(Match_est = Match_ci,
         Omission_est = Omission_ci,
         Addition_est = Addition_ci,
         CF1 = CF1_ci,
         CF2 = CF2_ci)


probs_grid_dsply <- probs_grid_coef %>%
  bind_rows(probs_grid_ci) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb, 
                                     levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years")),
         deathrecency_cat = factor(deathrecency_cat, levels = c("0-4", "5-9", "10-14"))) %>%
  arrange(cstatus_agesp_comb, deathrecency_cat, rank) %>%
  select(-rank)

table_with_headers <- probs_grid_dsply %>%
  group_split(cstatus_agesp_comb, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      cstatus_agesp_comb = unique(df$cstatus_agesp_comb),
      deathrecency_cat = "",
      Match_est = "",
      Omission_est = "", 
      Addition_est = "",
      CF1 = "",
      CF2 = "")
    bind_rows(header_row, df)
  })
table_with_headers$cstatus_agesp_comb <- as.character(table_with_headers$cstatus_agesp_comb)
table_with_headers$cstatus_agesp_comb[table_with_headers$deathrecency_cat != ""] <- ""

ft <- table_with_headers %>%
  flextable() %>%
  set_header_labels(values = c("Age-at-death", 
  "Recall period of death (years prior to validation study)", "% match (95% CI)",
  "% omission (95% CI)", "% addition (95% CI)", "CF 1 (95% CI)", "CF 2 (95% CI)")) %>%
  set_caption(caption = "Predicted probabilities of omission and addition by age group and recall period of death from the final models, with corresponding correction factors (CF) and 95% confidence intervals (CI).") %>%
  merge_v(j = ~ deathrecency_cat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "center", j = c(3:7), part = "all") %>%
  align(align = "left", j = c(1,2), part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-multi-regression-age-period.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

