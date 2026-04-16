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

# Subsample: matched events (D) (deaths only)
dat <- subset(overall, type == "VS_Match" & (cstatus_dss == "Died" | cstatus_sur == "Died"))

# Age of death transfers --------------------------------------------------

# age transfer
dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>% # drop cases where FPH says died and DSS does not. presumably dss hasn't captured death yet, but will
  group_by(cstatus_agesp_dss, cstatus_agesp_sur) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) 

# Deaths transferring out of each age group (DSS is reference)
transfers_out <- dat %>%
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
  )

# Deaths transferring into each age group from FPH (DSS is reference)
transfers_in <- dat %>%
    filter(cstatus_agesp_dss != "Surviving") %>%
    filter(cstatus_agesp_dss != cstatus_agesp_sur) %>%
    group_by(cstatus_agesp_sur) %>%
    summarise(n_in = n(), .groups = "drop") %>%
    rename(cstatus_agesp_dss = cstatus_agesp_sur)

# combine
tabAODtran <- transfers_out %>%
  left_join(transfers_in, by = "cstatus_agesp_dss") %>%
  replace_na(list(n_in = 0)) %>% 
  filter(cstatus_agesp_dss != "10+") %>%
  bind_rows( # add total
    summarise(.,
              cstatus_agesp_dss = "Total",
              n_total   = sum(n_total, na.rm = TRUE),
              n_correct = sum(n_correct, na.rm = TRUE),
              n_out     = sum(n_out, na.rm = TRUE),
              n_in      = sum(n_in, na.rm = TRUE)
    )
  ) %>%
  mutate( # expressing as % of DSS total for comparability
    per_correct = n_correct/n_total*100,
    per_out     = n_out/n_total*100,
    per_in      = n_in/n_total*100 
  ) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
              levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "Total"))) %>%
  arrange(cstatus_agesp_dss) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(cstatus_agesp_dss, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in)
ft <- flextable(tabAODtran) %>%
  set_header_labels(values = c("Age group of death", "N DSS", "N", "%","N", "%", "N", "%")) %>%
  add_header_row(values = c(" ", "Agreement", "FPH transferred out", "FPH transferred in"), colwidths = c(2, 2, 2, 2)) %>%
  #set_caption(caption = "Age of death transfers for deaths matched between DSS and FPH (reference = DSS).") %>%
  set_caption(caption = "Age-at-death misclassification. Deaths in the DSS that were correctly matched to the same age group in the FPH, transferred out or transferred in.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabAODtran), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-aod-transfers.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# transfers in and out are roughly symmetric for most groups (e.g. 5-9 loses some to 1-4 and gains some from 1-4), so they'll largely cancel in the correction factor
# Given the omission and addition rates are likely much larger sources of error, age transfer adjustment would add complexity without meaningfully changing the corrected rates


# Period of death transfer ------------------------------------------------

# Deaths transferring OUT of each recall period (DSS is reference)
# Note: limit the subsample to matched deaths where the age group at death is correctly classified across sources, thus excluding cases where age-at-death misclassification may have shifted deaths into a different recall period.


transfers_out <- dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  group_by(deathrecency_cat_dss) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(deathrecency_cat_dss == deathrecency_cat_sur),
    n_out     = sum(deathrecency_cat_dss != deathrecency_cat_sur),
    .groups = "drop"
  )

transfers_in <- dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(deathrecency_cat_dss = deathrecency_cat_sur)

# combine
tabPeriodtran <- transfers_out %>%
  left_join(transfers_in, by = "deathrecency_cat_dss") %>%
  replace_na(list(n_in = 0)) %>%
  filter(deathrecency_cat_dss != "15+") %>%
  bind_rows( # add total
    summarise(.,
              deathrecency_cat_dss = "Total",
              n_total   = sum(n_total, na.rm = TRUE),
              n_correct = sum(n_correct, na.rm = TRUE),
              n_out     = sum(n_out, na.rm = TRUE),
              n_in      = sum(n_in, na.rm = TRUE)
    )
  ) %>%
  mutate( # expressing as % of DSS total for comparability
    per_correct = n_correct/n_total*100,
    per_out     = n_out/n_total*100,
    per_in      = n_in/n_total*100 
  ) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                    levels = c("0-4", "5-9", "10-14",  "Total"))) %>%
  arrange(deathrecency_cat_dss) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(deathrecency_cat_dss, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in) 

ft <- flextable(tabPeriodtran) %>%
  set_header_labels(values = c("Recall period of death", "N DSS", "N", "%","N", "%", "N", "%")) %>%
  add_header_row(values = c(" ", "Agreement", "FPH transferred out", "FPH transferred in"), colwidths = c(2, 2, 2, 2)) %>%
  set_caption(caption = "Recall period misclassification. Deaths in the DSS that were correctly matched to the same 5-year recall period in the FPH, transferred out or transferred in.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabPeriodtran), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-period-transfers.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# The key observation is that all transfers are to adjacent periods only — no deaths are jumping two periods, which confirms this is genuine boundary date uncertainty (deaths occurring near the cutoff between periods) rather than systematic misreporting in one direction. Boundary uncertainty like this will always be roughly symmetric by nature, so in and out largely cancel.
# Conclusion is the same as age transfers — worth documenting in a table but formal correction would add complexity for negligible gain. Your omission and addition corrections will dominate.


# Period of birth transfer ------------------------------------------------

# The Only Scenario Birth Transfer Matters
# The edge case where birth transfer does matter is if a child is born just inside your outer recall boundary (e.g. 13.5 years ago), and FPH pushes the birth to 15+ years ago — making the child disappear from the dataset entirely. But that's really a record inclusion issue, not a period misclassification issue, and it would be partially captured by your omission model anyway since the death effectively vanishes from the FPH count.
# Conclusion
# Ignore birth date transfers entirely. Your death recency transfer analysis is sufficient and correct as-is. The work you already did stands — death date transfer is the right mechanism, it's approximately symmetric, and no correction is needed.

# transferred out
dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  group_by(birthrecency_cat_dss) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(birthrecency_cat_dss == birthrecency_cat_sur),
    n_out     = sum(birthrecency_cat_dss != birthrecency_cat_sur),
    .groups = "drop"
  )
# transferred in
dat %>%
  filter(cstatus_agesp_dss != "Surviving") %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         birthrecency_cat_dss != birthrecency_cat_sur) %>%
  group_by(birthrecency_cat_sur) %>%
  summarise(n_in = n(), .groups = "drop") 
