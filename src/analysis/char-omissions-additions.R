################################################################################
#' @description analyse characteristics associated with omission and addition of deaths
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

# Omissions and additions of deaths
# Subsamples: (A) all-women, (C) recent-births
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


# Sample A (omissions and matches) ----------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat3_int", "meducat_sur",
  "hhsizecat_sur",
  "intinterupt_sur", "observer_sur", "intcoop_sur", "otherwork_sur",
  "breakdown_sur", "support_sur",
  "cstatus_agesp_comb", #"cstrata_ac"
  "cstrata_ac_neo", "cstrata_ac_pneo", "cstrata_ac_1to4"
)

datDth <- dat %>%
  filter(denomA == 1) %>%
  mutate(type = case_when(
    type == "VS_Match" ~ "Match",
    type == "HDSS_NoMatch" ~ "Omission",
    TRUE ~ NA
  ))  %>% 
  mutate(birthrecency_cat = factor(birthrecency_cat, levels = c("0-4", "5-9", "10-14", "15+"))) %>%
  mutate(deathrecency_cat = factor(deathrecency_cat, levels = c("0-4", "5-9", "10-14", "15+"))) %>%
  mutate(cstrata_ac_neo = case_when(
    cstrata_ac == "Neonatal (other)" ~ "Other",
    cstrata_ac == "Neonatal (birth asphyxia)" ~ "Birth asphyxia",
    TRUE ~ "Not applicable"
  ),
  cstrata_ac_pneo = case_when(
    cstrata_ac == "Postneonatal (other)" ~ "Other",
    cstrata_ac == "Postneonatal (RI+con)" ~ "RI+con",
    TRUE ~ "Not applicable"
  ),
  cstrata_ac_1to4 = case_when(
    cstrata_ac == "1-4 years (other)" ~ "Other",
    cstrata_ac == "1-4 years (drowning)" ~ "Drowning",
    TRUE ~ "Not applicable"
  )) %>%
  select(type, all_of(vars))
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
  mutate(per = paste0("(",sprintf("%.2f", round(n / sum(n)*100, 2)), ")"),
         n = as.character(n)) %>% 
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  mutate(n_Match = ifelse(is.na(n_Match), 0, n_Match),
         per_Match = ifelse(is.na(per_Match), "(0.00)", per_Match),
         n_Omission = ifelse(is.na(n_Omission), 0, n_Omission),
         per_Omission = ifelse(is.na(per_Omission), "(0.00)", per_Omission)) %>%
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission) 

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  mydat <- datDth
  
  # drop missing values from chi-squared
  # some variables added after beginning of data collection, don't want to count these as missing
  mydat <- mydat[!(mydat[[v]] == "Missing"),]
  
  # drop "Not applicable" for the COD cases that are in other ages
  mydat <- mydat[!(mydat[[v]] == "Not applicable"),]
  
  if(v == "cstrata_ac"){
    #mydat <- mydat[!(mydat[[v]] == "5-9 year"),]
    #mydat <- mydat[!(mydat[[v]] == "10+"),]
    # don't assess additions for COD
    mydat <- subset(mydat, type != "Addition")
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
tabChi
tabChi <- tabChi %>%
  mutate(pvalcat = sprintf("%.2f", round(p_value, 2))) %>%
  mutate(pvalcat = ifelse(p_value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat)) 


# add total row
tabDthtot <- dat %>%
  filter(denomA == 1) %>%
  mutate(type = case_when(
    type == "VS_Match" ~ "Match",
    type == "HDSS_NoMatch" ~ "Omission",
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
  mutate(per = paste0("(",sprintf("%.2f", round(n / sum(n)*100, 2)), ")"),
         n = as.character(n)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission) 
tabDth <- tabDth %>%
  bind_rows(tabDthtot)

# merge on chi-squared
tabDth <- tabDth %>%
  left_join(tabChi %>% select(variable, pvalcat))

# Sample A order variables and values -------------------------------------------------------------------

# household level
v_hh <- c("hhsizecat_sur", "observer_sur", "intinterupt_sur", "intcoop_sur", "breakdown_sur",
          "otherwork_sur", "support_sur")
# women-level
v_wom <- c("magecat3_int", "meducat_sur", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "deathrecency_cat", "cstatus_agesp_comb", #"cstrata_ac")
          "cstrata_ac_neo", "cstrata_ac_pneo", "cstrata_ac_1to4")
v_all <- c(v_hh, v_wom, v_ch, "total")
length(v_all) == length(unique(tabDth$variable)) # TRUE
df_varrank <- data.frame(variable = v_all,
                         variablerank = 1:length(v_all))


unique(subset(tabDth, variable == "breakdown_sur")$value)

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
    variable == "hhsizecat_sur" & value == "1-3" ~ 1,
    variable == "hhsizecat_sur" & value == "4-6" ~ 2,
    variable == "hhsizecat_sur" & value == "7+" ~ 3,
    variable == "meducat_sur" & value == "Missing" ~ 1,
    variable == "meducat_sur" & value == "None" ~ 2,
    variable == "meducat_sur" & value == "Primary" ~ 3,
    variable == "meducat_sur" & value == "Secondary or higher" ~ 4,
    variable == "observer_sur" & value == "Missing" ~ 1,
    variable == "observer_sur" & value == "No" ~ 1,
    variable == "observer_sur" & value == "Yes" ~ 2,
    variable == "intinterupt_sur" & value == "Missing" ~ 1,
    variable == "intinterupt_sur" & value == "No" ~ 2,
    variable == "intinterupt_sur" & value == "Yes" ~ 3,
    variable == "intcoop_sur" & value == "Normal" ~ 1,
    variable == "intcoop_sur" & value == "Good or very good" ~ 2,
    variable == "breakdown_sur" & value == "Missing" ~ 1,
    variable == "breakdown_sur" & value == "No" ~ 2,
    variable == "breakdown_sur" & value == "Yes" ~ 3,
    variable == "otherwork_sur" & value == "Missing" ~ 1,
    variable == "otherwork_sur" & value == "No" ~ 2,
    variable == "otherwork_sur" & value == "Yes" ~ 3,
    variable == "support_sur" & value == "Missing" ~ 1,
    variable == "support_sur" & value == "No" ~ 2,
    variable == "support_sur" & value == "Yes" ~ 3,
    variable == "cstatus_agesp_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_agesp_comb" & value == "Neonatal" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Postneonatal" ~ 3,
    variable == "cstatus_agesp_comb" & value == "1-4 years" ~ 4,
    variable == "cstatus_agesp_comb" & value == "5-9 years" ~ 5,
    variable == "cstatus_agesp_comb" & value == "10+ years" ~ 6,
    # variable == "cstrata_ac" & value == "Surviving" ~ 1,
    # variable == "cstrata_ac" & value == "Neonatal (birth asphyxia)" ~ 2,
    # variable == "cstrata_ac" & value == "Neonatal (other)" ~ 3,
    # variable == "cstrata_ac" & value == "Neonatal (unknown)" ~ 4,
    # variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 5,
    # variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 6,
    # variable == "cstrata_ac" & value == "Postneonatal (unknown)" ~ 7,
    # variable == "cstrata_ac" & value == "1-4 years (drowning)" ~ 8,
    # variable == "cstrata_ac" & value == "1-4 years (other)" ~ 9,
    # variable == "cstrata_ac" & value == "1-4 years (unknown)" ~ 9,
    # variable == "cstrata_ac" & value == "5-9 years" ~ 10,
    # variable == "cstrata_ac" & value == "10+ years" ~ 11,
    variable == "cstrata_ac_neo" & value == "Birth asphyxia" ~ 1,
    variable == "cstrata_ac_neo" & value == "Other" ~ 2,
    variable == "cstrata_ac_pneo" & value == "RI+con" ~ 1,
    variable == "cstrata_ac_pneo" & value == "Other" ~ 2,
    variable == "cstrata_ac_1to4" & value == "Drowning" ~ 1,
    variable == "cstrata_ac_1to4" & value == "Other" ~ 2,
    TRUE ~ 1
  )) %>%
  arrange(variablerank, valuerank)

tabDth <- tabDth %>%
  #filter(!(value %in% c("Neonatal (unknown)", "Postneonatal (unknown)", "1-4 years (unknown)")))
  filter(!(value %in% c("Not applicable")))

# clean up variable names
tabDth <- tabDth %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "observer_sur"  ~ "Interview observed by others",
    variable == "intinterupt_sur"  ~ "Interview interrupted by others",
    variable == "intcoop_sur"  ~ "Respondent cooperation",
    variable == "breakdown_sur"  ~ "Respondent emotional breakdown",
    variable == "otherwork_sur"  ~ "Respondent conducting other work during interview",
    variable == "support_sur"  ~ "Respondent received support from others",
    variable == "magecat3_int" ~ "Mother age",
    variable == "meducat_sur" ~ "Mother education",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recall period (years)",
    variable == "deathrecency_cat"   ~ "Death recall period (years)",
    variable == "cstatus_agesp_comb"   ~ "Age-at-death",
    #variable == "cstrata_ac"   ~ "Age-specific cause of death",
    variable == "cstrata_ac_neo"   ~ "Neonatal cause of death",
    variable == "cstrata_ac_pneo"   ~ "Postneonatal cause of death",
    variable == "cstrata_ac_1to4"   ~ "1-4 years cause of death",
    variable == "total"   ~ "Total",
  )) %>%
  select(-c(variablerank, valuerank)) 
tabDth$n_Match[is.na(tabDth$n_Match)] <- 0
tabDth$n_Omission[is.na(tabDth$n_Omission)] <- 0
tabDth$per_Match[is.na(tabDth$per_Match)] <- "(0.00)"
tabDth$per_Omission[is.na(tabDth$per_Omission)] <- "(0.00)"


# add variables as their own row
tabDth <- tabDth %>%
  mutate(variable = factor(variable, levels = unique(variable)))
table_with_headers <- tabDth %>%
  group_split(variable, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      variable = unique(df$variable),
      value = "",
      n_Match = "", per_Match = "",
      n_Omission = "", per_Omission = "",
      pvalcat = ""
    )
    bind_rows(header_row, df)
  })
table_with_headers$variable <- as.character(table_with_headers$variable)
table_with_headers$variable[table_with_headers$value != ""] <- ""
table_with_headers <- table_with_headers[-(nrow(table_with_headers)-1),]

ft <- table_with_headers %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "(%)", "N", "(%)",  "p-value")) %>%
  add_header_row(values = c(" ", "Match", "Omission", ""), colwidths = c(2, 2, 2, 1)) %>%
  #set_caption(caption = "Characteristics of FPH deaths by reporting in DSS (ie, matches and additions)") %>%
  merge_v(j = ~ pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = c(2,3,5,7), part = "all") %>%
  align(align = "left", j = c(1,4,6), part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-dths-char-sampA.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Sample C (omissions, matches, additions): Counts, per, chi-squared ------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat", "deathrecency_cat",
  "magecat3_int", "meducat_sur",
  "hhsizecat_sur",
  "intinterupt_sur", "observer_sur", "intcoop_sur", "otherwork_sur",
  "breakdown_sur", "support_sur",
  "cstatus_agesp_comb", #"cstrata_ac"
  "cstrata_ac_neo", "cstrata_ac_pneo", "cstrata_ac_1to4"
)

datDth <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "VS_NoMatch" ~ "Addition",
    type == "VS_Match" ~ "Match",
    type == "HDSS_NoMatch" ~ "Omission",
    TRUE ~ NA
  ))  %>% 
  mutate(birthrecency_cat = factor(birthrecency_cat, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(deathrecency_cat = factor(deathrecency_cat, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(cstrata_ac_neo = case_when(
    cstrata_ac == "Neonatal (other)" ~ "Other",
    cstrata_ac == "Neonatal (birth asphyxia)" ~ "Birth asphyxia",
    TRUE ~ "Not applicable"
  ),
  cstrata_ac_pneo = case_when(
    cstrata_ac == "Postneonatal (other)" ~ "Other",
    cstrata_ac == "Postneonatal (RI+con)" ~ "RI+con",
    TRUE ~ "Not applicable"
  ),
  cstrata_ac_1to4 = case_when(
    cstrata_ac == "1-4 years (other)" ~ "Other",
    cstrata_ac == "1-4 years (drowning)" ~ "Drowning",
    TRUE ~ "Not applicable"
  )) %>%
  select(type, all_of(vars))
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
  mutate(per = paste0("(",sprintf("%.2f", round(n / sum(n)*100, 2)), ")"),
         n = as.character(n)) %>% 
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  mutate(n_Match = ifelse(is.na(n_Match), 0, n_Match),
         per_Match = ifelse(is.na(per_Match), "(0.00)", per_Match),
         n_Omission = ifelse(is.na(n_Omission), 0, n_Omission),
         per_Omission = ifelse(is.na(per_Omission), "(0.00)", per_Omission),
         n_Addition = ifelse(is.na(n_Addition), 0, n_Addition),
         per_Addition = ifelse(is.na(per_Addition), "(0.00)", per_Addition)) %>%
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition) 

# remove cod for additions
tabDth$n_Addition[tabDth$variable == "cstrata_ac"] <- "-"
tabDth$per_Addition[tabDth$variable == "cstrata_ac"] <- "-"

# chi-squared
tabChi <- map_dfr(vars, function(v) {
  
  mydat <- datDth
  
  # drop missing values from chi-squared
  # some variables added after beginning of data collection, don't want to count these as missing
  mydat <- mydat[!(mydat[[v]] == "Missing"),]
  
  # drop "Not applicable" for the COD cases that are in other ages
  mydat <- mydat[!(mydat[[v]] == "Not applicable"),]
  
  if(v == "cstrata_ac"){
    #mydat <- mydat[!(mydat[[v]] == "5-9 year"),]
    #mydat <- mydat[!(mydat[[v]] == "10+"),]
    # don't assess additions for COD
    mydat <- subset(mydat, type != "Addition")
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
tabChi
tabChi <- tabChi %>%
  mutate(pvalcat = sprintf("%.2f", round(p_value, 2))) %>%
  mutate(pvalcat = ifelse(p_value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p_value <= 0.01, "<0.01", pvalcat)) 


# add total row
tabDthtot <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, addition from validation study
    type == "VS_Match" ~ "Match",
    type == "HDSS_NoMatch" ~ "Omission",
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
  mutate(per = paste0("(",sprintf("%.2f", round(n / sum(n)*100, 2)), ")"),
         n = as.character(n)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(variable, value), names_from = type, values_from = c(n, per)) %>%
  select(variable, value, n_Match, per_Match, n_Omission, per_Omission, n_Addition, per_Addition) 
tabDth <- tabDth %>%
  bind_rows(tabDthtot)

# merge on chi-squared
tabDth <- tabDth %>%
  left_join(tabChi %>% select(variable, pvalcat))


# Sample C order variables and values -------------------------------------------------------------------

# household level
v_hh <- c("hhsizecat_sur", "observer_sur", "intinterupt_sur", "intcoop_sur", "breakdown_sur",
          "otherwork_sur", "support_sur")
# women-level
v_wom <- c("magecat3_int", "meducat_sur", "paritymaxcat_comb")
# child-level
v_ch <- c("birthorder_cat_comb", "birthrecency_cat", "deathrecency_cat", "cstatus_agesp_comb", #"cstrata_ac")
          "cstrata_ac_neo", "cstrata_ac_pneo", "cstrata_ac_1to4")
v_all <- c(v_hh, v_wom, v_ch, "total")
length(v_all) == length(unique(tabDth$variable)) # TRUE
df_varrank <- data.frame(variable = v_all,
                         variablerank = 1:length(v_all))


unique(subset(tabDth, variable == "breakdown_sur")$value)

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
    variable == "hhsizecat_sur" & value == "1-3" ~ 1,
    variable == "hhsizecat_sur" & value == "4-6" ~ 2,
    variable == "hhsizecat_sur" & value == "7+" ~ 3,
    variable == "meducat_sur" & value == "Missing" ~ 1,
    variable == "meducat_sur" & value == "None" ~ 2,
    variable == "meducat_sur" & value == "Primary" ~ 3,
    variable == "meducat_sur" & value == "Secondary or higher" ~ 4,
    variable == "observer_sur" & value == "Missing" ~ 1,
    variable == "observer_sur" & value == "No" ~ 1,
    variable == "observer_sur" & value == "Yes" ~ 2,
    variable == "intinterupt_sur" & value == "Missing" ~ 1,
    variable == "intinterupt_sur" & value == "No" ~ 2,
    variable == "intinterupt_sur" & value == "Yes" ~ 3,
    variable == "intcoop_sur" & value == "Normal" ~ 1,
    variable == "intcoop_sur" & value == "Good or very good" ~ 2,
    variable == "breakdown_sur" & value == "Missing" ~ 1,
    variable == "breakdown_sur" & value == "No" ~ 2,
    variable == "breakdown_sur" & value == "Yes" ~ 3,
    variable == "otherwork_sur" & value == "Missing" ~ 1,
    variable == "otherwork_sur" & value == "No" ~ 2,
    variable == "otherwork_sur" & value == "Yes" ~ 3,
    variable == "support_sur" & value == "Missing" ~ 1,
    variable == "support_sur" & value == "No" ~ 2,
    variable == "support_sur" & value == "Yes" ~ 3,
    variable == "cstatus_agesp_comb" & value == "Surviving" ~ 1,
    variable == "cstatus_agesp_comb" & value == "Neonatal" ~ 2,
    variable == "cstatus_agesp_comb" & value == "Postneonatal" ~ 3,
    variable == "cstatus_agesp_comb" & value == "1-4 years" ~ 4,
    variable == "cstatus_agesp_comb" & value == "5-9 years" ~ 5,
    variable == "cstatus_agesp_comb" & value == "10+ years" ~ 6,
    # variable == "cstrata_ac" & value == "Surviving" ~ 1,
    # variable == "cstrata_ac" & value == "Neonatal (birth asphyxia)" ~ 2,
    # variable == "cstrata_ac" & value == "Neonatal (other)" ~ 3,
    # variable == "cstrata_ac" & value == "Neonatal (unknown)" ~ 4,
    # variable == "cstrata_ac" & value == "Postneonatal (RI+con)" ~ 5,
    # variable == "cstrata_ac" & value == "Postneonatal (other)" ~ 6,
    # variable == "cstrata_ac" & value == "Postneonatal (unknown)" ~ 7,
    # variable == "cstrata_ac" & value == "1-4 years (drowning)" ~ 8,
    # variable == "cstrata_ac" & value == "1-4 years (other)" ~ 9,
    # variable == "cstrata_ac" & value == "1-4 years (unknown)" ~ 9,
    # variable == "cstrata_ac" & value == "5-9 years" ~ 10,
    # variable == "cstrata_ac" & value == "10+ years" ~ 11,
    variable == "cstrata_ac_neo" & value == "Birth asphyxia" ~ 1,
    variable == "cstrata_ac_neo" & value == "Other" ~ 2,
    variable == "cstrata_ac_pneo" & value == "RI+con" ~ 1,
    variable == "cstrata_ac_pneo" & value == "Other" ~ 2,
    variable == "cstrata_ac_1to4" & value == "Drowning" ~ 1,
    variable == "cstrata_ac_1to4" & value == "Other" ~ 2,
    TRUE ~ 1
  )) %>%
  arrange(variablerank, valuerank)

tabDth <- tabDth %>%
  #filter(!(value %in% c("Neonatal (unknown)", "Postneonatal (unknown)", "1-4 years (unknown)")))
  filter(!(value %in% c("Not applicable")))

# clean up variable names
tabDth <- tabDth %>%
  mutate(variable = case_when(
    variable == "hhsizecat_sur" ~ "Household size",
    variable == "observer_sur"  ~ "Interview observed by others",
    variable == "intinterupt_sur"  ~ "Interview interrupted by others",
    variable == "intcoop_sur"  ~ "Respondent cooperation",
    variable == "breakdown_sur"  ~ "Respondent emotional breakdown",
    variable == "otherwork_sur"  ~ "Respondent conducting other work during interview",
    variable == "support_sur"  ~ "Respondent received support from others",
    variable == "magecat3_int" ~ "Mother age",
    variable == "meducat_sur" ~ "Mother education",
    variable == "paritymaxcat_comb" ~ "Mother parity",
    variable == "birthorder_cat_comb" ~ "Birth order",
    variable == "birthrecency_cat"   ~ "Birth recall period (years)",
    variable == "deathrecency_cat"   ~ "Death recall period (years)",
    variable == "cstatus_agesp_comb"   ~ "Age-at-death",
    #variable == "cstrata_ac"   ~ "Age-specific cause of death",
    variable == "cstrata_ac_neo"   ~ "Neonatal cause of death",
    variable == "cstrata_ac_pneo"   ~ "Postneonatal cause of death",
    variable == "cstrata_ac_1to4"   ~ "1-4 years cause of death",
    variable == "total"   ~ "Total",
  )) %>%
  select(-c(variablerank, valuerank)) 
tabDth$n_Match[is.na(tabDth$n_Match)] <- 0
tabDth$n_Omission[is.na(tabDth$n_Omission)] <- 0
tabDth$n_Addition[is.na(tabDth$n_Addition)] <- 0
tabDth$per_Match[is.na(tabDth$per_Match)] <- "(0.00)"
tabDth$per_Omission[is.na(tabDth$per_Omission)] <- "(0.00)"
tabDth$per_Addition[is.na(tabDth$per_Addition)] <- "(0.00)"

# add variables as their own row
tabDth <- tabDth %>%
  mutate(variable = factor(variable, levels = unique(variable)))
table_with_headers <- tabDth %>%
  group_split(variable, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      variable = unique(df$variable),
      value = "",
      n_Match = "", per_Match = "",
      n_Omission = "", per_Omission = "",
      n_Addition = "", per_Addition = "",
      pvalcat = ""
    )
    bind_rows(header_row, df)
  })
table_with_headers$variable <- as.character(table_with_headers$variable)
table_with_headers$variable[table_with_headers$value != ""] <- ""
table_with_headers <- table_with_headers[-(nrow(table_with_headers)-1),]

ft <- table_with_headers %>%
  flextable() %>%
  set_header_labels(values = c("Variable", "Value", "N", "(%)", "N", "(%)", "N", "(%)", "p-value")) %>%
  add_header_row(values = c(" ", "Match", "Omission", "Addition", ""), colwidths = c(2, 2, 2, 2, 1)) %>%
  #set_caption(caption = "Characteristics of FPH deaths by reporting in DSS (ie, matches and additions)") %>%
  merge_v(j = ~ pvalcat) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = c(2,3,5,7,9), part = "all") %>%
  align(align = "left", j = c(1,4,6,8), part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-dths-char-sampC.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

