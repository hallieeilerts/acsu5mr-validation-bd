################################################################################
#' @description analyse omissions of live births and deaths
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
library(officer)
library(flextable)
#' Inputs
overall <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Assign denominator
# E - matched pregnancies only (deaths)
dat <- subset(overall, type == "VS_Match" & (cstatus_dss == "Died" | cstatus_sur == "Died"))

# age transfer
dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>% # drop these. presumably dss hasn't captured death yet, but will
  group_by(cstatus_agesp_dss, cstatus_agesp_sur) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) 

# Deaths transferring OUT of each age group (DSS is reference)
transfers_out <- dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  mutate(transfer_out = cstatus_agesp_dss != cstatus_agesp_sur) %>%
  group_by(cstatus_agesp_dss) %>%
  summarise(
    n_total     = n(),
    n_out       = sum(transfer_out),
    p_out       = n_out / n_total
  )

# Deaths transferring INTO each age group (FPH is reference)
transfers_in <- dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  mutate(transfer_in = cstatus_agesp_dss != cstatus_agesp_sur) %>%
  group_by(cstatus_agesp_sur) %>%
  summarise(
    n_total_fph = n(),
    n_in        = sum(transfer_in),
    p_in        = n_in / n_total_fph
  ) %>%
  rename(cstatus_agesp_dss = cstatus_agesp_sur)  # rename for easy joining

# Join together
transfers <- transfers_out %>%
  left_join(transfers_in, by = "cstatus_agesp_dss")
transfers


age_transfer_table <- dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    TRUE ~ "transfer_out"
  )) %>%
  group_by(cstatus_agesp_dss) %>%
  summarise(
    n_total    = n(),
    n_correct  = sum(cstatus_agesp_dss == cstatus_agesp_sur),
    n_out      = sum(cstatus_agesp_dss != cstatus_agesp_sur),
    .groups = "drop"
  ) %>%
  # transfers IN: FPH puts a death into this group but DSS disagrees
  left_join(
    dat %>%
      filter(cstatus_agesp_dss != "Surviving",
             cstatus_agesp_dss != cstatus_agesp_sur) %>%
      group_by(cstatus_agesp_sur) %>%
      summarise(n_in = n(), .groups = "drop") %>%
      rename(cstatus_agesp_dss = cstatus_agesp_sur),
    by = "cstatus_agesp_dss"
  ) %>%
  replace_na(list(n_in = 0)) %>%
  mutate(
    pct_correct = n_correct / n_total * 100,
    pct_out     = n_out     / n_total * 100,
    pct_in      = n_in      / n_total * 100   # as % of DSS total for comparability
  ) %>%
  select(
    `Age group`       = cstatus_agesp_dss,
    `N (DSS)`         = n_total,
    `Correct n (%)`   = n_correct,
    `Transferred out n (%)` = n_out,
    `Transferred in n (%)`  = n_in,
    pct_correct, pct_out, pct_in
  ) %>%
  mutate(
    `Correct n (%)`         = sprintf("%d (%.1f%%)", `Correct n (%)`,   pct_correct),
    `Transferred out n (%)` = sprintf("%d (%.1f%%)", `Transferred out n (%)`, pct_out),
    `Transferred in n (%)`  = sprintf("%d (%.1f%%)", `Transferred in n (%)`,  pct_in)
  ) %>%
  select(-pct_correct, -pct_out, -pct_in)
age_transfer_table


# The more important point is that transfers in and out are roughly symmetric for most groups (e.g. 5-9 loses some to 1-4 and gains some from 1-4), so they'll largely cancel in the correction factor. Given the omission and addition rates are likely much larger sources of error, age transfer adjustment would add complexity without meaningfully changing your corrected rates — worth a footnote but not a formal correction.




# date transfer -----------------------------------------------------------

# date transfer
dat %>%
  filter(cstatus_agesp_dss ==  cstatus_agesp_sur) %>% # same age group, matched deaths. pure date misclassification, not age.
  group_by(deathrecency_cat_dss, deathrecency_cat_sur) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) 

date_transfer_table <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age group only
  group_by(deathrecency_cat_dss) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(deathrecency_cat_dss == deathrecency_cat_sur),
    n_out     = sum(deathrecency_cat_dss != deathrecency_cat_sur),
    .groups = "drop"
  ) %>%
  left_join(
    dat %>%
      filter(cstatus_agesp_dss == cstatus_agesp_sur,
             deathrecency_cat_dss != deathrecency_cat_sur) %>%
      group_by(deathrecency_cat_sur) %>%
      summarise(n_in = n(), .groups = "drop") %>%
      rename(deathrecency_cat_dss = deathrecency_cat_sur),
    by = "deathrecency_cat_dss"
  ) %>%
  replace_na(list(n_in = 0)) %>%
  mutate(
    pct_correct = n_correct / n_total * 100,
    pct_out     = n_out     / n_total * 100,
    pct_in      = n_in      / n_total * 100
  ) %>%
  mutate(
    `Correct n (%)`         = sprintf("%d (%.1f%%)", n_correct, pct_correct),
    `Transferred out n (%)` = sprintf("%d (%.1f%%)", n_out,     pct_out),
    `Transferred in n (%)` = sprintf("%d (%.1f%%)", n_in,      pct_in)
  ) %>%
  select(
    `Period`                = deathrecency_cat_dss,
    `N (DSS)`               = n_total,
    `Correct n (%)`,
    `Transferred out n (%)`,
    `Transferred in n (%)`
  )
date_transfer_table 


# The key observation is that all transfers are to adjacent periods only — no deaths are jumping two periods, which confirms this is genuine boundary date uncertainty (deaths occurring near the cutoff between periods) rather than systematic misreporting in one direction. Boundary uncertainty like this will always be roughly symmetric by nature, so in and out largely cancel.
# Conclusion is the same as age transfers — worth documenting in a table but formal correction would add complexity for negligible gain. Your omission and addition corrections will dominate.


# The Only Scenario Birth Transfer Matters
# The edge case where birth transfer does matter is if a child is born just inside your outer recall boundary (e.g. 13.5 years ago), and FPH pushes the birth to 15+ years ago — making the child disappear from the dataset entirely. But that's really a record inclusion issue, not a period misclassification issue, and it would be partially captured by your omission model anyway since the death effectively vanishes from the FPH count.
# Conclusion
# Ignore birth date transfers entirely. Your death recency transfer analysis is sufficient and correct as-is. The work you already did stands — death date transfer is the right mechanism, it's approximately symmetric, and no correction is needed.

dat %>%
  filter(cstatus_agesp_dss != "Surviving",
         cstatus_agesp_dss == cstatus_agesp_sur) %>%  # matched deaths, same age group
  mutate(birth_transfer_out = birthrecency_cat_sur != birthrecency_cat_dss) %>%  
  # or however you define "outside window" — depends on your variable coding
  group_by(birthrecency_cat_dss) %>%
  summarise(
    n_total = n(),
    n_transferred = sum(birth_transfer_out),
    pct = n_transferred / n_total * 100
  )



