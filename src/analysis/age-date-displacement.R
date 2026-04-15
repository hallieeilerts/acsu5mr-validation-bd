################################################################################
#' @description analyse the overallDob file, event level, denominator E (matched pregnancies)
#' Assess:
#' dob/dod/aod displacement
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
overallDob <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Denominator
# E - matched pregnancies only
dat <- subset(overallDob, type == "VS_Match")

nrow(dat) # 1968

# DOB displacement --------------------------------------------------------

vars <- c(
  "birthorder_cat_comb", "paritymaxcat_comb", "birthrecency_cat",
  "magecat2_int", "hhsizecat_sur", "hhassets_sur", 
  "cstatus_comb", "cstatus_agesp_comb", "cstrata_ac"
)

datLong <- dat %>%
  mutate(dif = as.numeric(dob_c_dss - c220)) %>%
  select(dif, all_of(vars)) %>% # refactor birthrecency_cat because 15+ category is causing issue with chi-square test
  pivot_longer(
    cols = -dif,
    names_to = "variable",
    values_to = "value"
  ) 

datLong <- datLong %>%
  mutate(variable = as.character(variable)) %>%
  mutate(
    value_ordered = case_when(
      variable == "birthorder_cat_comb" ~
        paste0(match(value, c("4+", "3", "2", "1")), "_", value),
      
      variable == "birthrecency_cat" ~
        paste0(match(value, c("15+", "10-14", "5-9", "0-4")), "_", value),
      
      variable == "magecat2_int" ~
        paste0(match(value, c("45+", "40-44", "35-39", "30-34", "25-29", "15-24")), "_", value),
      
      variable == "cstatus_comb" ~
        paste0(match(value, c("Died", "Surviving")), "_", value),
      
      variable == "cstatus_agesp_comb" ~
        paste0(match(value, c("10+","5-9","1-4","Postneonatal","Neonatal","Surviving")), "_", value),
      
      variable == "cstrata_ac" ~
        paste0(match(value, c("10+","5-9 year", "1-4 year (other)", "1-4 year (drowning)",
                              "Postneonatal (other)", "Postneonatal (RI+con)",
                              "Neonatal (other)", "Neonatal (birth asphyxia)", "Surviving")), "_", value),
                     
      variable == "hhassets_sur" ~ 
        paste0(match(value, c("5th quintile", "4th quintile", "3rd quintile", "2nd quintile", "1st quintile")), "_", value),
      
      variable == "hhsizecat_sur" ~
        paste0(match(value, c("Large","Medium","Small")), "_", value),
      
      variable == "hhassets_sur" ~
        paste0(match(value, c("5th quintile","4th quintile","3rd quintile","2nd quintile","1st quintile")), "_", value),
      
      TRUE ~ value
    )
  )

ggplot(datLong) +
  geom_boxplot(aes(x = value_ordered, y = dif)) +
  facet_wrap(~variable, scales = "free_y") +
  coord_flip(ylim = c(-500,500))



# AOD in FPH, DOD in HDSS -------------------------------------------------


# Shifts in age group of death --------------------------------------------



