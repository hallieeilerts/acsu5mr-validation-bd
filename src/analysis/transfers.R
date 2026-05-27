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
library(patchwork)
#' Inputs
overall <- readRDS("./gen/augment/overallName-recode.rds")
################################################################################

# For analysis
# Subsample: matched events (D) (deaths only)
dat <- subset(overall, type == "VS_Match" & (cstatus_dss == "Died" | cstatus_sur == "Died"))
nrow(dat) # 614
# drop cases where FPH says died and DSS does not. presumably dss hasn't captured death yet, but will
dat <- subset(overall, type == "VS_Match" & (cstatus_dss == "Died" & cstatus_sur == "Died"))
nrow(dat) # 612

# For sample table
# data for entire sample, not just deaths
# live births only (stipulated that cstatus is "Died", as done above, effectively does the same thing)
datSamp <- subset(overall, type == "VS_Match" & 
                    pregout_dss == "Live birth" & c223 == "Live birth")
datSamp <- subset(datSamp, !((cstatus_dss == "Died" & cstatus_sur != "Died") |
                    (cstatus_dss != "Died" & cstatus_sur == "Died")))
nrow(datSamp) # 1966


# Age agreement -----------------------------------------------------------

# Deaths reported as younger/older in FPH (DSS is reference)
agree_age <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur == "Neonatal" ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur != "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "10+" ~ "youngerfph",
    TRUE ~ NA
  )) %>%
  group_by(cstatus_agesp_dss, classified) %>%
  summarise(n = n())
# total for each age group
agetotal <- agree_age %>%
  ungroup() %>%
  group_by(classified) %>%
  summarise(n = sum(n)) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(total), names_from = classified, values_from = c(n, per)) %>%
  mutate(cstatus_agesp_dss = "Total")
tab_agreeage <- agree_age %>%
  group_by(cstatus_agesp_dss) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(cstatus_agesp_dss, total), names_from = classified, values_from = c(n, per)) %>%
  bind_rows(agetotal) %>%
  select(cstatus_agesp_dss, total, n_correct, per_correct, n_olderfph, per_olderfph, n_youngerfph, per_youngerfph) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+","Total"))) %>%
  arrange(cstatus_agesp_dss)

tab_agreeage$n_olderfph[is.na(tab_agreeage$n_olderfph)] <- 0
tab_agreeage$per_olderfph[is.na(tab_agreeage$per_olderfph)] <- "0.00"
tab_agreeage$n_youngerfph[is.na(tab_agreeage$n_youngerfph) & tab_agreeage$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeage$per_youngerfph[is.na(tab_agreeage$per_youngerfph) & tab_agreeage$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeage$n_youngerfph[is.na(tab_agreeage$n_youngerfph)] <- 0
tab_agreeage$per_youngerfph[is.na(tab_agreeage$per_youngerfph)] <- "0.00"

tab_agreeage

# Age transferred in/out ----------------------------------------------------------

# Deaths transferring out of each age group (DSS is reference)
transfers_out <- dat %>%
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
  filter(cstatus_agesp_dss != cstatus_agesp_sur) %>%
  group_by(cstatus_agesp_sur) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(cstatus_agesp_dss = cstatus_agesp_sur)

# combine
tab_transage <- transfers_out %>%
  left_join(transfers_in, by = "cstatus_agesp_dss") %>%
  replace_na(list(n_in = 0)) %>% 
  #filter(cstatus_agesp_dss != "10+") %>%
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
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+","Total"))) %>%
  arrange(cstatus_agesp_dss) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(cstatus_agesp_dss, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in)

# Age/cause FPH agreement -----------------------------------------------------

agree_agecause <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur == "Neonatal" ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur != "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "10+" ~ "youngerfph",
    TRUE ~ NA
  )) %>%
  group_by(cstatus_agesp_dss, cstrata_c, classified) %>%
  summarise(n = n())
tab_agreeagecause <- agree_agecause %>%
  group_by(cstatus_agesp_dss, cstrata_c) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(cstatus_agesp_dss, cstrata_c, total), names_from = classified, values_from = c(n, per)) %>%
  select(cstatus_agesp_dss, cstrata_c, total, n_correct, per_correct, n_olderfph, per_olderfph, n_youngerfph, per_youngerfph) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+"))) %>%
  arrange(cstatus_agesp_dss) %>%
  filter(!(cstatus_agesp_dss %in% c("5-9", "10+")))

tab_agreeagecause$n_olderfph[is.na(tab_agreeagecause$n_olderfph)] <- 0
tab_agreeagecause$per_olderfph[is.na(tab_agreeagecause$per_olderfph)] <- "0.00"
tab_agreeagecause$n_youngerfph[is.na(tab_agreeagecause$n_youngerfph) & tab_agreeagecause$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeagecause$per_youngerfph[is.na(tab_agreeagecause$per_youngerfph) & tab_agreeagecause$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeagecause$n_youngerfph[is.na(tab_agreeagecause$n_youngerfph)] <- 0
tab_agreeagecause$per_youngerfph[is.na(tab_agreeagecause$per_youngerfph)] <- "0.00"

# merge on causes and transfers in
TabAgetrans <- tab_agreeage %>%
  mutate(cstrata_c = "All causes") %>%
  bind_rows(tab_agreeagecause) %>%
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    cstrata_c == "All causes" ~ 3,
    TRUE ~ 1
  )) %>%
  arrange(cstatus_agesp_dss, cod_rank)  %>%
  left_join(
    tab_transage %>% select(cstatus_agesp_dss, n_in),
    by = "cstatus_agesp_dss"
  ) %>%
  select(cstatus_agesp_dss, cstrata_c, total, n_correct, per_correct, n_olderfph, per_olderfph, 
         n_youngerfph, per_youngerfph, n_in)

# Table: age transfers ----------------------------------------------------

ft <- flextable(TabAgetrans) %>%
  merge_v(j = ~ cstatus_agesp_dss + n_in) %>%
  set_header_labels(values = c("Age group of death", "COD", 
                               "N HDSS", "N", "%","N", "%", "N", "%", "N")) %>%
  add_header_row(values = c(" ", "Agreement", "Older in FPH", "Younger in FPH", "FPH transferred into age group"), 
                 colwidths = c(3, 2, 2, 2, 1)) %>%
  set_caption(caption = "Age-at-death misclassification. Deaths in the DSS that were correctly classified as the same age group in the FPH, reported as older or younger.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(TabAgetrans), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-age-transfers-cod.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Period of death agreement -----------------------------------------------------------

# Deaths reported as younger/older in FPH (DSS is reference)
agree_period <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss, classified) %>%
  summarise(n = n())
# total for each period
periodtotal <- agree_period %>%
  ungroup() %>%
  group_by(classified) %>%
  summarise(n = sum(n)) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(total), names_from = classified, values_from = c(n, per)) %>%
  mutate(deathrecency_cat_dss = "Total")
tab_agreeperiod <- agree_period %>%
  group_by(deathrecency_cat_dss) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(deathrecency_cat_dss, total), names_from = classified, values_from = c(n, per)) %>%
  bind_rows(periodtotal) %>%
  select(deathrecency_cat_dss, total, n_correct, per_correct, n_distantfph, per_distantfph,
         n_recentfph, per_recentfph) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                    levels = c("0-4", "5-9", "10-14", "15+","Total"))) %>%
  arrange(deathrecency_cat_dss)

tab_agreeperiod$n_distantfph[is.na(tab_agreeperiod$n_distantfph)] <- 0
tab_agreeperiod$per_distantfph[is.na(tab_agreeperiod$per_distantfph)] <- "0.00"
tab_agreeperiod$n_recentfph[is.na(tab_agreeperiod$n_recentfph) & tab_agreeperiod$deathrecency_cat_dss == "0-4"] <- "-"
tab_agreeperiod$per_recentfph[is.na(tab_agreeperiod$per_recentfph) & tab_agreeperiod$deathrecency_cat_dss == "0-4"] <- "-"
tab_agreeperiod$n_recentfph[is.na(tab_agreeperiod$n_recentfph)] <- 0
tab_agreeperiod$per_recentfph[is.na(tab_agreeperiod$per_recentfph)] <- "0.00"

# Deaths transferring out of each period (DSS is reference)
transfers_out <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  group_by(deathrecency_cat_dss) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(deathrecency_cat_dss == deathrecency_cat_sur),
    n_out     = sum(deathrecency_cat_dss != deathrecency_cat_sur),
    .groups = "drop"
  )

transfers_in <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(deathrecency_cat_dss = deathrecency_cat_sur)

# combine
tab_transperiod <- transfers_out %>%
  left_join(transfers_in, by = "deathrecency_cat_dss") %>%
  replace_na(list(n_in = 0)) %>% 
  #filter(cstatus_agesp_dss != "10+") %>%
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
                                       levels = c("0-4", "5-9", "10-14", "15+", "Total"))) %>%
  arrange(deathrecency_cat_dss) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(deathrecency_cat_dss, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in) 


TabPeriodtrans <- tab_agreeperiod %>%
  left_join(tab_transperiod %>% select(deathrecency_cat_dss, n_in))

TabPeriodtrans

# Period/age of death agreement -----------------------------------------------------------

# Deaths reported as younger/older in FPH (DSS is reference)
agree_periodage <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss, classified) %>%
  summarise(n = n())
# totals
tab_agreeperiodage <- agree_periodage %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(deathrecency_cat_dss, cstatus_agesp_dss, total), names_from = classified, values_from = c(n, per))
tab_agreeperiodage$n_distantfph[is.na(tab_agreeperiodage$n_distantfph)] <- 0
tab_agreeperiodage$per_distantfph[is.na(tab_agreeperiodage$per_distantfph)] <- "0.00"
tab_agreeperiodage$n_recentfph[is.na(tab_agreeperiodage$n_recentfph) & tab_agreeperiodage$deathrecency_cat_dss == "0-4"] <- "-"
tab_agreeperiodage$per_recentfph[is.na(tab_agreeperiodage$per_recentfph) & tab_agreeperiodage$deathrecency_cat_dss == "0-4"] <- "-"
tab_agreeperiodage$n_recentfph[is.na(tab_agreeperiodage$n_recentfph)] <- 0
tab_agreeperiodage$per_recentfph[is.na(tab_agreeperiodage$per_recentfph)] <- "0.00"

# transfers in
tab_transperiodage <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur, cstatus_agesp_dss) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(deathrecency_cat_dss = deathrecency_cat_sur)
tab_transperiodage <- tab_transperiodage %>%
  bind_rows(transfers_in %>% mutate(cstatus_agesp_dss = "All"))

# add totals
TabPeriodAgetrans <- tab_agreeperiodage %>%
  bind_rows(tab_agreeperiod) %>%
  mutate(cstatus_agesp_dss = ifelse(is.na(cstatus_agesp_dss), "All", cstatus_agesp_dss)) %>%
  left_join(tab_transperiodage) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                       levels = c("0-4", "5-9", "10-14", "15+","Total"))) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss,
                                    levels = c("Neonatal","Postneonatal", "1-4", "5-9", "10+", "All"))) %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss)

TabPeriodAgetrans[is.na(TabPeriodAgetrans)] <- 0
TabPeriodAgetrans

# Period/age/cause FPH agreement ----------------------------------------------

agree_periodagecause <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c, classified) %>%
  summarise(n = n())
# totals
tab_agreeperiodagecause <- agree_periodagecause %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c) %>%
  mutate(total = sum(n)) %>%
  mutate(per = round(n/total*100,2)) %>%
  pivot_wider(id_cols = c(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c, total), names_from = classified, values_from = c(n, per))
tab_agreeperiodagecause$n_distantfph[is.na(tab_agreeperiodagecause$n_distantfph)] <- 0
tab_agreeperiodagecause$per_distantfph[is.na(tab_agreeperiodagecause$per_distantfph)] <- "0.00"
tab_agreeperiodagecause$n_recentfph[is.na(tab_agreeperiodagecause$n_recentfph) & tab_agreeperiodagecause$deathrecency_cat_dss == "0-4"] <- "-"
tab_agreeperiodagecause$per_recentfph[is.na(tab_agreeperiodagecause$per_recentfph) & tab_agreeperiodagecause$deathrecency_cat_dss == "0-4"] <- "-"
tab_agreeperiodagecause$n_recentfph[is.na(tab_agreeperiodagecause$n_recentfph)] <- 0
tab_agreeperiodagecause$per_recentfph[is.na(tab_agreeperiodagecause$per_recentfph)] <- "0.00"

# add totals
TabPeriodAgeCausetrans <- tab_agreeperiodagecause %>%
  bind_rows(tab_agreeperiodage %>% mutate(n_recentfph = as.character(n_recentfph),
                                          per_distantfph = as.character(per_distantfph),
                                          per_recentfph = as.character(per_recentfph))) %>% 
  mutate(cstrata_c = ifelse(is.na(cstrata_c), "All causes", cstrata_c)) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                       levels = c("0-4", "5-9", "10-14", "15+","Total"))) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss,
                                    levels = c("Neonatal","Postneonatal", "1-4", "5-9", "10+", "All"))) %>%
    mutate(cod_rank = case_when(
      cstrata_c == "Birth asphyxia" ~ 1,
      cstrata_c == "RI and congenital" ~ 2,
      cstrata_c == "Drowning" ~ 3,
      cstrata_c == "Other" ~ 4,
      cstrata_c == "5-9 year" ~ 5,
      cstrata_c == "10+" ~ 6,
      cstrata_c == "All causes" ~ 7,
      TRUE ~ NA
    )) %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss, cod_rank)

# Table: period transfers ----------------------------------------------------

# simple table

ft <- flextable(TabPeriodtrans) %>%
  set_header_labels(values = c("Recall period of death",  
                               "N HDSS", "N", "%","N", "%", "N", "%", "N")) %>%
  add_header_row(values = c(" ", "Agreement", "Displaced backwards in FPH", "Displaced forwards in FPH", "Displaced into period in FPH"), 
                 colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Displacement in recall period of death.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(TabPeriodtrans), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-transfers.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Associations between recall period, age, cause --------------------------

TabPeriodtrans
TabPeriodAgetrans
TabPeriodAgeCausetrans

# ok maybe need to test association between recall period * age group
# and recall period * age/cause
# age/cause and recall period transfers should be figure

# recall period displacement by age
dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  count(cstatus_agesp_dss, classified) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("cstatus_agesp_dss") %>%
  chisq.test()

# recall period displacement by age, stratified by period
dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss) %>%
  group_modify(~ {
    .x %>%
      count(cstatus_agesp_dss, classified) %>%
      pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
      tibble::column_to_rownames("cstatus_agesp_dss") %>%
      chisq.test() %>%
      broom::tidy()
  })


# recall period displacement by cause of death
dat %>%
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  count(cstrata_c, classified) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(-cstrata_c) %>%
  as.matrix() %>%
  chisq.test()
  

# recall period displacement by cause of death, stratified by period
dat %>%
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss) %>%
  group_modify(~ {
    .x %>%
      count(cstrata_c, classified) %>%
      pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
      select(-cstrata_c) %>%
      as.matrix() %>%
      chisq.test(simulate.p.value = TRUE) %>%
      broom::tidy()
  }) %>%
  ungroup()


# recall period displacement by age-specific cause of death, stratified by period
dat %>%
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss) %>%
  filter(n_distinct(cstrata_c) >= 2) %>% # drop single cstrata groups
  group_modify(~ {
    .x %>%
      count(cstrata_c, classified) %>%
      pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
      select(-cstrata_c) %>%
      as.matrix() %>%
      chisq.test(simulate.p.value = TRUE) %>%
      broom::tidy()
  }) %>%
  ungroup()
# no statistically significant difference in the pattern of recall period displacement (correct vs distantfph vs recentfph) across cause-of-death strata, within any age × recency group.


# Figure: period/age/cause transfers --------------------------------------

# Figure for period/age/cause displacement

# count
p1 <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss) %>%
  count(cstrata_c, classified) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c) %>%
  mutate(pct = n / sum(n) * 100) %>% 
  mutate(labs = paste0(cstatus_agesp_dss , " - ", cstrata_c)) %>%
  filter(deathrecency_cat_dss != "15+") %>%
  mutate(labs = case_when(
    labs == "10+ - 10+" ~ "10+ years",
    labs == "5-9 - 5-9 year" ~ "5-9 years",
    labs == "1-4 - Other" ~ "1-4 years - other",
    labs == "1-4 - Drowning" ~ "1-4 years - drowning",
    labs == "Postneonatal - Other" ~ "Postneonatal - other",
    labs == "Postneonatal - RI and congenital" ~ "Postneonatal - RI and congenital",
    labs == "Neonatal - Other" ~ "Neonatal - other",
    labs == "Neonatal - Birth asphyxia" ~ "Neonatal - birth asphyxia",
    TRUE ~ labs
  )) %>%
  mutate(labs = factor(labs, levels = c("10+ years", "5-9 years", "1-4 years - other", "1-4 years - drowning",
                                        "Postneonatal - other", "Postneonatal - RI and congenital",
                                        "Neonatal - other", "Neonatal - birth asphyxia"))) %>%
  mutate(classified = factor(classified, levels = c("recentfph", "correct", "distantfph"),
                             labels = c("Displaced forward in FPH" , "Agreement", "Displaced backward in FPH"))) %>%
  ggplot() +
  geom_bar(aes(x = labs, y = n, fill = classified), stat = "identity") +
  facet_wrap(~deathrecency_cat_dss, ncol = 1) +
  coord_flip() +
  labs (y = "N", x = "") +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())  +
  guides(fill = guide_legend(reverse = TRUE))
# percent
p2 <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  mutate(classified = case_when(
    deathrecency_cat_dss == deathrecency_cat_sur ~ "correct",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur == "0-4" ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "5-9" &
      deathrecency_cat_sur != "0-4" ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      deathrecency_cat_sur %in% c("0-4", "5-9") ~ "recentfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "10-14" &
      !(deathrecency_cat_sur %in% c("0-4", "5-9")) ~ "distantfph",
    deathrecency_cat_dss != deathrecency_cat_sur & deathrecency_cat_dss == "15+" &
      deathrecency_cat_sur %in% c("0-4", "5-9", "10-14") ~ "recentfph",
    TRUE ~ NA
  )) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss) %>%
  count(cstrata_c, classified) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c) %>%
  mutate(pct = n / sum(n) * 100) %>% 
  mutate(labs = paste0(cstatus_agesp_dss , " - ", cstrata_c)) %>%
  filter(deathrecency_cat_dss != "15+") %>%
  mutate(labs = case_when(
    labs == "10+ - 10+" ~ "10+ years",
    labs == "5-9 - 5-9 year" ~ "5-9 years",
    labs == "1-4 - Other" ~ "1-4 years - other",
    labs == "1-4 - Drowning" ~ "1-4 years - drowning",
    labs == "Postneonatal - Other" ~ "Postneonatal - other",
    labs == "Postneonatal - RI and congenital" ~ "Postneonatal - RI and congenital",
    labs == "Neonatal - Other" ~ "Neonatal - other",
    labs == "Neonatal - Birth asphyxia" ~ "Neonatal - birth asphyxia",
    TRUE ~ labs
  )) %>%
  mutate(labs = factor(labs, levels = c("10+ years", "5-9 years", "1-4 years - other", "1-4 years - drowning",
                                        "Postneonatal - other", "Postneonatal - RI and congenital",
                                        "Neonatal - other", "Neonatal - birth asphyxia"))) %>%
  mutate(classified = factor(classified, levels = c("recentfph", "correct", "distantfph"),
                             labels = c("Displaced forward in FPH" , "Agreement", "Displaced backward in FPH"))) %>%
  ggplot() +
  geom_bar(aes(x = labs, y = pct, fill = classified), stat = "identity") +
  facet_wrap(~deathrecency_cat_dss, ncol = 1) +
  coord_flip() +
  labs (y = "%", x = "") +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())  +
  guides(fill = guide_legend(reverse = TRUE))


p1_nolegend <- p1 + theme(legend.position = "none")
p2_nolegend <- p2 + theme(legend.position = "none", 
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank())
combined <- p1_nolegend + p2_nolegend +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined
ggsave("./gen/figures/period-trans-agecause.png", combined, width = 8, height = 6, dpi = 300)



# OLD ---------------------------------------------------------------------




agree_agecause <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur == "Neonatal" ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur != "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "10+" ~ "youngerfph",
    TRUE ~ NA
  )) %>%
  group_by(cstatus_agesp_dss, cstrata_c, classified) %>%
  summarise(n = n()) %>%
  filter(!(cstatus_agesp_dss %in% c("5-9", "10+")))






# combine
tabAODtran <- transfers_out %>%
  left_join(transfers_in, by = "cstatus_agesp_dss") %>%
  replace_na(list(n_in = 0)) %>% 
  #filter(cstatus_agesp_dss != "10+") %>%
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
              levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+","Total"))) %>%
  arrange(cstatus_agesp_dss) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(cstatus_agesp_dss, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in)
tabAODtran_total <- tabAODtran

ft <- flextable(tabAODtran) %>%
  set_header_labels(values = c("Age group of death", "N HDSS", "N", "%","N", "%", "N", "%")) %>%
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


# Age of death transfers by cause -----------------------------------------

transfers_out <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    TRUE ~ "transfer_out"
  )) %>%
  group_by(cstatus_agesp_dss, cstrata_c) %>%
  summarise(
    n_total    = n(),
    n_correct  = sum(cstatus_agesp_dss == cstatus_agesp_sur),
    n_out      = sum(cstatus_agesp_dss != cstatus_agesp_sur),
    .groups = "drop"
  )
transfers_in <- dat %>%
  filter(cstatus_agesp_dss != cstatus_agesp_sur) %>%
  group_by(cstatus_agesp_sur, cstrata_c) %>%
  summarise(n_in = n(), .groups = "drop") %>% # recode those that are transfered in
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "1-4" & cstrata_c == "5-9 year", "Other", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "1-4" & cstrata_c == "RI and congenital", "Other", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "10+" & cstrata_c == "5-9 year", "10+", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "5-9", "5-9 year", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "Postneonatal" & cstrata_c == "Drowning", "Other", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "Postneonatal" & cstrata_c == "Birth asphyxia", "Other", cstrata_c )) %>%
  group_by(cstatus_agesp_sur, cstrata_c) %>%
  summarise(n_in = sum(n_in)) %>%
  rename(cstatus_agesp_dss = cstatus_agesp_sur)
# combine
tabAODtran <- transfers_out %>%
  full_join(transfers_in, by = c("cstatus_agesp_dss", "cstrata_c")) %>%
  replace_na(list(n_in = 0)) %>% 
  bind_rows( # add total
    summarise(.,
              cstatus_agesp_dss = "Total",
              cstrata_c = "",
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
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+","Total"))) %>%
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    TRUE ~ 1
  )) %>%
  arrange(cstatus_agesp_dss, cod_rank) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(cstatus_agesp_dss, cstrata_c, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in)

tabAODtran_total

# add totals row for each group
df_totals <- tabAODtran_total %>%
  filter(!(cstatus_agesp_dss %in% c("5-9","10+", "Total"))) %>%
  mutate(cstrata_c = "Total") 
tabAODtran <- tabAODtran %>%
  bind_rows(df_totals) %>% 
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    cstrata_c == "Total" ~ 3,
    cstrata_c == "" ~ 4,
    TRUE ~ 1
  )) %>%
  arrange(cstatus_agesp_dss, cod_rank) %>%
  select(-cod_rank)
tabAODtran$cstrata_c[tabAODtran$cstatus_agesp_dss == "5-9"] <- " "
tabAODtran$cstrata_c[tabAODtran$cstatus_agesp_dss == "10+"] <- " "
tabAODtran$cstatus_agesp_dss <- as.character(tabAODtran$cstatus_agesp_dss)
tabAODtran$cstatus_agesp_dss[tabAODtran$cstatus_agesp_dss == "1-4"] <- "1-4 years"
tabAODtran$cstatus_agesp_dss[tabAODtran$cstatus_agesp_dss == "5-9"] <- "5-9 years"
tabAODtran$cstatus_agesp_dss[tabAODtran$cstatus_agesp_dss == "10+"] <- "10+ years"

ft <- flextable(tabAODtran) %>%
  set_header_labels(values = c("Age group of death", "COD",
                               "N HDSS", "N", "%","N", "%", "N", "%")) %>%
  add_header_row(values = c(" ", "Agreement", "FPH transferred out", "FPH transferred in"), colwidths = c(3, 2, 2, 2)) %>%
  set_caption(caption = "Age-at-death misclassification. Deaths in the DSS that were correctly matched to the same age group in the FPH, transferred out or transferred in.") %>%
  merge_v(j = ~ cstatus_agesp_dss) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabAODtran), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-aod-transfers-cod.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Figure: age at death transfer by cause ----------------------------------

transfers_out <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    TRUE ~ "transfer_out"
  )) %>%
  group_by(cstatus_agesp_dss, cstrata_c) %>%
  summarise(
    n_total    = n(),
    n_correct  = sum(cstatus_agesp_dss == cstatus_agesp_sur),
    n_out      = sum(cstatus_agesp_dss != cstatus_agesp_sur),
    .groups = "drop"
  )
transfers_in <- dat %>%
  filter(cstatus_agesp_dss != cstatus_agesp_sur) %>%
  group_by(cstatus_agesp_sur, cstrata_c) %>%
  summarise(n_in = n(), .groups = "drop") %>% # recode those that are transfered in
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "1-4" & cstrata_c == "5-9 year", "Other", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "1-4" & cstrata_c == "RI and congenital", "Other", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "10+" & cstrata_c == "5-9 year", "10+", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "5-9", "5-9 year", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "Postneonatal" & cstrata_c == "Drowning", "Other", cstrata_c )) %>%
  mutate(cstrata_c = ifelse(cstatus_agesp_sur == "Postneonatal" & cstrata_c == "Birth asphyxia", "Other", cstrata_c )) %>%
  group_by(cstatus_agesp_sur, cstrata_c) %>%
  summarise(n_in = sum(n_in)) %>%
  rename(cstatus_agesp_dss = cstatus_agesp_sur)
tabAODtran <- transfers_out %>%
  full_join(transfers_in, by = c("cstatus_agesp_dss", "cstrata_c"))


tabAODtran %>%
  pivot_longer(cols = c(n_total, n_correct, n_out, n_in)) %>%
  ggplot() +
  geom_bar(aes(x = cstrata_c, y = value, fill = name), stat = "identity", position = "stack") +
  facet_wrap(~cstatus_agesp_dss) +
  coord_flip()

# Period of death transfer ------------------------------------------------

# Deaths transferring OUT of each recall period (DSS is reference)
# Note: limit the subsample to matched deaths where the age group at death is correctly classified across sources, thus excluding cases where age-at-death misclassification may have shifted deaths into a different recall period.

transfers_out <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  group_by(deathrecency_cat_dss) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(deathrecency_cat_dss == deathrecency_cat_sur),
    n_out     = sum(deathrecency_cat_dss != deathrecency_cat_sur),
    .groups = "drop"
  )

transfers_in <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(deathrecency_cat_dss = deathrecency_cat_sur)

# combine
tabPeriodtran <- transfers_out %>%
  left_join(transfers_in, by = "deathrecency_cat_dss") %>%
  replace_na(list(n_in = 0)) %>%
  #filter(deathrecency_cat_dss != "15+") %>%
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
                                    levels = c("0-4", "5-9", "10-14", "15+", "Total"))) %>%
  arrange(deathrecency_cat_dss) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(deathrecency_cat_dss, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in) 
tabPeriodtran_total <- tabPeriodtran

ft <- flextable(tabPeriodtran) %>%
  set_header_labels(values = c("Recall period of death", "N HDSS", "N", "%","N", "%", "N", "%")) %>%
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

# Period of death transfer by age-specific cause ---------------------------------------

transfers_out1 <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  group_by(deathrecency_cat_dss, cstrata_a, cstrata_c) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(deathrecency_cat_dss == deathrecency_cat_sur),
    n_out     = sum(deathrecency_cat_dss != deathrecency_cat_sur),
    .groups = "drop"
  )
transfers_out_tot <- transfers_out1 %>%
  group_by(deathrecency_cat_dss, cstrata_a) %>%
  summarise(n_total = sum(n_total), n_correct = sum(n_correct), n_out = sum(n_out)) %>%
  filter(!(cstrata_a %in% c("10+", "5-9 year"))) %>%
  mutate(cstrata_c = "All causes") %>%
  ungroup()
transfers_out <- transfers_out1 %>%
  bind_rows(transfers_out_tot)

transfers_in1 <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur, cstrata_a, cstrata_c) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(deathrecency_cat_dss = deathrecency_cat_sur)
transfers_in_tot <- transfers_in1 %>%
  group_by(deathrecency_cat_dss, cstrata_a) %>%
  summarise(n_in = sum(n_in)) %>%
  filter(!(cstrata_a %in% c("10+", "5-9 year"))) %>%
  mutate(cstrata_c = "All causes") %>%
  ungroup()
transfers_in <- transfers_in1 %>%
  bind_rows(transfers_in_tot)


tabPeriodtran <- transfers_out %>%
  full_join(transfers_in, by = c("deathrecency_cat_dss", "cstrata_a", "cstrata_c")) %>% 
  replace_na(list(n_in = 0)) %>%
  mutate(cstrata_a = factor(cstrata_a, 
                            levels = c("Neonatal", "Postneonatal", "1-4 year", "5-9 year", "10+"))) %>% 
  complete(deathrecency_cat_dss, cstrata_a, cstrata_c,
           fill = list(n_total = 0, n_correct = 0, n_out = 0, n_in = 0)) %>% 
  filter((cstrata_a == "Neonatal" & cstrata_c %in% c("Birth asphyxia", "Other", "All causes")) |
           (cstrata_a == "Postneonatal" & cstrata_c %in% c("RI and congenital", "Other", "All causes")) |
           (cstrata_a == "1-4 year" & cstrata_c %in% c("Drowning", "Other", "All causes")) |
           (cstrata_a == "5-9 year" & cstrata_c %in% c("5-9 year")) |
           (cstrata_a == "10+" & cstrata_c %in% c("10+"))) %>% 
  bind_rows( # add total
    summarise(.,
              deathrecency_cat_dss = "Total",
              cstrata_a = "",
              cstrata_c = "",
              n_total   = sum(transfers_out1$n_total, na.rm = TRUE),
              n_correct = sum(transfers_out1$n_correct, na.rm = TRUE),
              n_out     = sum(transfers_out1$n_out, na.rm = TRUE),
              n_in      = sum(transfers_in1$n_in, na.rm = TRUE)
    )
  ) %>%
  mutate( # expressing as % of DSS total for comparability
    per_correct = n_correct/n_total*100,
    per_out     = n_out/n_total*100,
    per_in      = n_in/n_total*100 
  ) %>% 
  mutate(per_correct = ifelse(is.na(per_correct), 0, per_correct),
         per_out = ifelse(is.na(per_out), 0, per_out),
         per_in = ifelse(is.na(per_in), 0, per_in)) %>% 
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                       levels = c("0-4", "5-9", "10-14", "15+", "Total"))) %>% 
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    TRUE ~ 1
  )) %>%
  mutate(cstrata_a = factor(cstrata_a, levels = c("Neonatal", "Postneonatal", "1-4 year",  
                                                  "5-9 year", "10+", ""))) %>% 
  arrange(deathrecency_cat_dss, cstrata_a, cod_rank) %>%
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(deathrecency_cat_dss, cstrata_a, cstrata_c, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in) 

# add totals row for each group
df_totals <- tabPeriodtran_total %>%
  mutate(cstrata_a = "Total", cstrata_c = "") %>%
  filter(!(deathrecency_cat_dss == "Total" & cstrata_a == "Total"))
tabPeriodtran <- tabPeriodtran %>%
  bind_rows(df_totals) %>% 
  mutate(cstrata_a = factor(cstrata_a, levels = c("Neonatal", "Postneonatal", "1-4 year","5-9 year", "10+", "Total",""))) %>% 
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    cstrata_c == "All causes" ~ 3,
    TRUE ~ 1
  )) %>%
  arrange(deathrecency_cat_dss, cstrata_a, cod_rank) %>%
  select(-cod_rank)
tabPeriodtran$cstrata_c[tabPeriodtran$cstrata_a == "5-9 year"] <- " "
tabPeriodtran$cstrata_c[tabPeriodtran$cstrata_a == "10+"] <- " "

ft <- flextable(tabPeriodtran) %>%
  set_header_labels(values = c("Recall period of death", "Age-at-death",
                               "COD", "N HDSS", "N", "%","N", "%", "N", "%")) %>%
  add_header_row(values = c(" ", "Agreement", "FPH transferred out of recall period", "FPH transferred into recall period"), colwidths = c(4, 2, 2, 2)) %>%
  set_caption(caption = "Recall period misclassification. Deaths in the DSS that were correctly matched to the same 5-year recall period in the FPH, transferred out or transferred in.") %>%
  merge_v(j = ~ deathrecency_cat_dss + cstrata_a) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabPeriodtran), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-period-transfers-cod.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Period of death transfer by binary cause ---------------------------------------

transfers_out1 <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur) %>%  # same age at death group
  filter(!(cstrata_a %in% c("5-9 year", "10+"))) %>% # remove because no cause strata
  group_by(deathrecency_cat_dss, cstrata_c_binary) %>%
  summarise(
    n_total   = n(),
    n_correct = sum(deathrecency_cat_dss == deathrecency_cat_sur),
    n_out     = sum(deathrecency_cat_dss != deathrecency_cat_sur),
    .groups = "drop"
  )
transfers_out_tot <- transfers_out1 %>%
  group_by(deathrecency_cat_dss) %>%
  summarise(n_total = sum(n_total), n_correct = sum(n_correct), n_out = sum(n_out)) %>%
  mutate(cstrata_c_binary = "All causes") %>%
  ungroup()
transfers_out <- transfers_out1 %>%
  bind_rows(transfers_out_tot)

transfers_in1 <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur, cstrata_c_binary) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  filter(!(cstrata_c_binary %in% "No COD strata")) %>% # not interesting in in transfers to 5-9 and 10+
  rename(deathrecency_cat_dss = deathrecency_cat_sur)
transfers_in_tot <- transfers_in1 %>%
  group_by(deathrecency_cat_dss) %>%
  summarise(n_in = sum(n_in)) %>%
  mutate(cstrata_c_binary = "All causes") %>%
  ungroup()
transfers_in <- transfers_in1 %>%
  bind_rows(transfers_in_tot)


tabPeriodtran <- transfers_out %>%
  full_join(transfers_in, by = c("deathrecency_cat_dss", "cstrata_c_binary")) %>% 
  replace_na(list(n_in = 0)) %>%
  mutate(cstrata_c_binary = factor(cstrata_c_binary, 
                            levels = c("Leading", "Non-leading", "All causes"))) %>% 
  complete(deathrecency_cat_dss, cstrata_c_binary,
           fill = list(n_total = 0, n_correct = 0, n_out = 0, n_in = 0)) %>% 
  bind_rows( # add total
    summarise(.,
              deathrecency_cat_dss = "Total",
              cstrata_c_binary = "",
              n_total   = sum(transfers_out1$n_total, na.rm = TRUE),
              n_correct = sum(transfers_out1$n_correct, na.rm = TRUE),
              n_out     = sum(transfers_out1$n_out, na.rm = TRUE),
              n_in      = sum(transfers_in1$n_in, na.rm = TRUE)
    )
  ) %>%
  mutate( # expressing as % of DSS total for comparability
    per_correct = n_correct/n_total*100,
    per_out     = n_out/n_total*100,
    per_in      = n_in/n_total*100 
  ) %>% 
  mutate(per_correct = ifelse(is.na(per_correct), 0, per_correct),
         per_out = ifelse(is.na(per_out), 0, per_out),
         per_in = ifelse(is.na(per_in), 0, per_in)) %>% 
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                       levels = c("0-4", "5-9", "10-14", "15+", "Total"))) %>% 
  mutate(per_correct = sprintf("%.2f", round(per_correct, 2)),
         per_out = sprintf("%.2f", round(per_out, 2)),
         per_in = sprintf("%.2f", round(per_in, 2))) %>%
  select(deathrecency_cat_dss, cstrata_c_binary, n_total, n_correct, per_correct, n_out, per_out, n_in, per_in) 

ft <- flextable(tabPeriodtran) %>%
  set_header_labels(values = c("Recall period of death", "COD",
                               "N HDSS", "N", "%","N", "%", "N", "%")) %>%
  add_header_row(values = c(" ", "Agreement", "FPH transferred out of recall period", "FPH transferred into recall period"), colwidths = c(3, 2, 2, 2)) %>%
  set_caption(caption = "Recall period misclassification. Deaths by COD in the DSS that were correctly matched to the same 5-year recall period in the FPH, transferred out or transferred in.") %>%
  merge_v(j = ~ deathrecency_cat_dss) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabPeriodtran), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-period-transfers-cod-binary.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")



# Period of birth transfer ------------------------------------------------

# The Only Scenario Birth Transfer Matters
# The edge case where birth transfer does matter is if a child is born just inside your outer recall boundary (e.g. 13.5 years ago), and FPH pushes the birth to 15+ years ago — making the child disappear from the dataset entirely. But that's really a record inclusion issue, not a period misclassification issue, and it would be partially captured by your omission model anyway since the death effectively vanishes from the FPH count.
# Conclusion
# Ignore birth date transfers entirely. Your death recency transfer analysis is sufficient and correct as-is. The work you already did stands — death date transfer is the right mechanism, it's approximately symmetric, and no correction is needed.

# transferred out
dat %>%
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
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         birthrecency_cat_dss != birthrecency_cat_sur) %>%
  group_by(birthrecency_cat_sur) %>%
  summarise(n_in = n(), .groups = "drop") 


# Numbers for sample -------------------------------------------------------

# Subsample: matched events (D) (deaths only)
datNum <- data.frame(subsample = c("matched-events"),
                     nWomen = length(unique(datSamp$rid_m)),
                     nLb_dss = nrow(datSamp),
                     nLb_sur = nrow(datSamp),
                     nDth_dss = nrow(subset(datSamp, cstatus_dss == "Died")),
                     nDth_sur = nrow(subset(datSamp, cstatus_sur == "Died")))
datNum
write.csv(datNum, "./gen/audit/num2.csv", row.names = FALSE)
