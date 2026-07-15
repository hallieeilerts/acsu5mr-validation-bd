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
# also ensuring that they agree on survival status
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

# Table 3: age transfers ----------------------------------------------------

ft <- flextable(TabAgetrans) %>%
  merge_v(j = ~ cstatus_agesp_dss + n_in) %>%
  set_header_labels(values = c("Age group of death", "COD", 
                               "N HDSS", "N", "%","N", "%", "N", "%", "N")) %>%
  add_header_row(values = c(" ", "Agreement", "Older in FPH", "Younger in FPH", "FPH transferred into age group"), 
                 colwidths = c(3, 2, 2, 2, 1)) %>%
  set_caption(caption = "Age-at-death errors by cause of death. Table displays counts and percentages for deaths in the HDSS that were correctly classified as the same age group in the FPH, or reported as occurring in an older or younger age group. The number of deaths that were transferred into the age group in the FPH is also provided.") %>%
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

# Test associations between age, cause --------------------------

# association between age and age displacement
dat %>%
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
  count(cstatus_agesp_dss, classified) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("cstatus_agesp_dss") %>%
  chisq.test()

# association between cause and displacement, by age
dat %>%
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
  group_by(cstatus_agesp_dss) %>%
  filter(n_distinct(cstrata_c) >= 2) %>%
  count(cstrata_c, classified) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0)
dat %>%
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
  group_by(cstatus_agesp_dss) %>%
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


# Table S1: associations age/cause ----------------------------------------

countsAgeCause <- dat %>%
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
  group_by(cstatus_agesp_dss) %>%
  filter(n_distinct(cstrata_c) >= 2) %>%
  count(cstrata_c, classified)
# testing Chi
countsAgeCause %>%
  ungroup() %>%
  filter(cstatus_agesp_dss == "1-4") %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(-c(cstrata_c, cstatus_agesp_dss)) %>%
  as.matrix() %>%
  chisq.test(simulate.p.value = TRUE) 
chiAgeCause <- countsAgeCause %>%
    group_modify(~ {
    .x %>%
      pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
      select(-cstrata_c) %>%
      as.matrix() %>%
      chisq.test(simulate.p.value = TRUE) %>%
      broom::tidy()
    }) %>%
    ungroup() %>%
  mutate(pvalcat = sprintf("%.2f", round(p.value, 2))) %>%
  mutate(pvalcat = ifelse(p.value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p.value <= 0.01, "<0.01", pvalcat)) %>%
  select(cstatus_agesp_dss, pvalcat)

# table
TabChiAgeCause <- countsAgeCause %>%
  mutate(classified = factor(classified, levels = c("correct", "olderfph", "youngerfph"),
                             labels = c("Agreement", "Older in FPH", "Younger in FPH"))) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  left_join(chiAgeCause, by = "cstatus_agesp_dss") %>%
  mutate(cstatus_agesp_dss = factor( cstatus_agesp_dss, levels = c("Neonatal", "Postneonatal", "1-4"))) %>%
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    TRUE ~ 1
  )) %>%
  arrange(cstatus_agesp_dss, cod_rank) %>%
  select(-c(cod_rank))


ft <- flextable(TabChiAgeCause) %>%
  merge_v(j = ~ cstatus_agesp_dss + pvalcat) %>%
  set_header_labels(values = c("Age group of death", "COD", 
                               "Agreement", "Older in FPH", "Younger in FPH", "p-value")) %>%
  set_caption(caption = "Age-at-death errors by cause of death. Table displays counts of deaths in the HDSS that were correctly classified as the same age group in the FPH, or reported as occurring in an older or younger age group, alongside p-values from Chi-squared tests for significance.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(TabChiAgeCause), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft


doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-age-transfers-cod-chi.docx")
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

# Table 4: period transfers ----------------------------------------------------

# simple table

ft <- flextable(TabPeriodtrans) %>%
  set_header_labels(values = c("Recall period of death (years prior to validation study)",  
                               "N HDSS", "N", "%","N", "%", "N", "%", "N")) %>%
  add_header_row(values = c(" ", "Agreement", "Displaced backwards in FPH", "Displaced forwards in FPH", "Displaced into period in FPH"), 
                 colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Displacement in recall period of death. Table displays counts and percentages for deaths in the HDSS that were correctly classified as occurring in the same five-year period in the FPH, or were displaced to a more distant or more recent period. The number of deaths that were displaced into each five-year period in the FPH is also provided.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(TabPeriodtrans), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-displacement.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Test associations between recall period, age, cause --------------------------

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


# Table S2: associations period/age ----------------------------------------

countsPeriodAge <- dat %>%
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
  count(cstatus_agesp_dss, classified)
# testing Chi
countsPeriodAge %>%
  ungroup() %>%
  filter(deathrecency_cat_dss == "0-4") %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(-c(deathrecency_cat_dss, cstatus_agesp_dss)) %>%
  as.matrix() %>%
  chisq.test(simulate.p.value = TRUE) 
chiPeriodAge <- countsPeriodAge  %>%
  group_by(deathrecency_cat_dss) %>%
  group_modify(~ {
    .x %>%
      pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
      select(-cstatus_agesp_dss) %>%
      as.matrix() %>%
      chisq.test(simulate.p.value = TRUE) %>%
      broom::tidy()
  }) %>%
  ungroup() %>%
  mutate(pvalcat = sprintf("%.2f", round(p.value, 2))) %>%
  mutate(pvalcat = ifelse(p.value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p.value <= 0.01, "<0.01", pvalcat)) %>%
  select(deathrecency_cat_dss, pvalcat)

# table
TabChiPeriodAge <- countsPeriodAge %>%
  mutate(classified = factor(classified, levels = c("correct", "distantfph", "recentfph"),
                             labels = c("Agreement", "Displaced backwards in FPH", "Displaced forwards in FPH"))) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+"),
                                    labels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"))) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, levels = c("0-4", "5-9", "10-14", "15+"))) %>%
  group_by(deathrecency_cat_dss) %>%
  complete(cstatus_agesp_dss, fill = list(Agreement = 0, `Displaced backwards in FPH` = 0,
                                          `Displaced forwards in FPH` = 0)) %>%
  left_join(chiPeriodAge, by = "deathrecency_cat_dss") %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss) 


ft <- flextable(TabChiPeriodAge) %>%
  merge_v(j = ~ deathrecency_cat_dss + pvalcat) %>%
  set_header_labels(values = c("Recall period of death (years prior to validation study)",
                               "Age group of death", 
                               "Agreement", "Displaced backwards in FPH", 
                               "Displaced forwards in FPH", "p-value")) %>%
  set_caption(caption = "Displacement in recall period of death by age. Table displays counts of deaths in the HDSS that were correctly classified as occurring in the same five-year period in the FPH, or were displaced to a more distant or more recent period, alongside p-values from Chi-squared tests for significance.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(TabChiPeriodAge), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft


doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-displacement-ageChi.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Table S3: associations period/age/cause ----------------------------------------

countsPeriodAgeC <- dat %>%
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
  filter(cstatus_agesp_dss %in% c("Neonatal", "Postneonatal", "1-4")) %>%
  filter(deathrecency_cat_dss != "15+") %>%
  group_by(deathrecency_cat_dss) %>%
  count(cstatus_agesp_dss, cstrata_c_binary, classified)
# testing Chi
countsPeriodAgeC %>%
  ungroup() %>%
  filter(deathrecency_cat_dss == "0-4" & cstatus_agesp_dss == "Neonatal") %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(-c(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c_binary)) %>%
  as.matrix() %>%
  chisq.test(simulate.p.value = TRUE) 
countsPeriodAgeC %>%
  ungroup() %>%
  filter(deathrecency_cat_dss == "0-4" & cstatus_agesp_dss == "Postneonatal") %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(-c(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c_binary)) %>%
  as.matrix() %>%
  chisq.test(simulate.p.value = TRUE) 
countsPeriodAgeC %>%
  ungroup() %>%
  filter(deathrecency_cat_dss == "5-9" & cstatus_agesp_dss == "Neonatal") %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(-c(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c_binary)) %>%
  as.matrix() %>%
  chisq.test(simulate.p.value = TRUE) 
chiPeriodAgeC <- countsPeriodAgeC %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss) %>%
  group_modify(~ {
    .x %>%
      pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
      select(-cstrata_c_binary) %>%
      as.matrix() %>%
      chisq.test(simulate.p.value = TRUE) %>%
      broom::tidy()
  }) %>%
  ungroup() %>%
  mutate(pvalcat = sprintf("%.2f", round(p.value, 2))) %>%
  mutate(pvalcat = ifelse(p.value <= 0.001, "<0.001", pvalcat)) %>%
  mutate(pvalcat = ifelse(p.value <= 0.01, "<0.01", pvalcat)) %>%
  select(deathrecency_cat_dss, cstatus_agesp_dss, pvalcat)

# table
TabChiPeriodAgeC <- countsPeriodAgeC %>%
  mutate(cstrata_c_binary = factor(cstrata_c_binary, levels = c("Leading", "Non-leading"))) %>%
  group_by(deathrecency_cat_dss, cstatus_agesp_dss) %>%
  complete(cstrata_c_binary, fill = list(n = 0)) %>% 
  group_by(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c_binary) %>%
  mutate(classified = factor(classified, levels = c("correct", "distantfph", "recentfph"),
                             labels = c("Agreement", "Displaced backwards in FPH", "Displaced forwards in FPH"))) %>%
  complete(classified, fill = list(n = 0)) %>% 
  filter(!is.na(classified)) %>%
  left_join(chiPeriodAgeC, by = c("deathrecency_cat_dss","cstatus_agesp_dss")) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+"),
                                    labels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"))) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, levels = c("0-4", "5-9", "10-14", "15+"))) %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss, cstrata_c_binary) %>%
  mutate(COD = case_when(
    cstatus_agesp_dss == "Neonatal" & cstrata_c_binary == "Leading" ~ "Birth asphyxia",
    cstatus_agesp_dss == "Postneonatal" & cstrata_c_binary == "Leading" ~ "RI and congenital",
    cstatus_agesp_dss == "1-4 years" & cstrata_c_binary == "Leading" ~ "Drowning",
    TRUE ~ "Other"
  )) %>%
  ungroup() %>%
  select(deathrecency_cat_dss, cstatus_agesp_dss, COD, classified, n, pvalcat) %>%
  pivot_wider(names_from = classified, values_from = n, values_fill = 0) %>%
  select(deathrecency_cat_dss, cstatus_agesp_dss, COD, Agreement, `Displaced backwards in FPH`, `Displaced forwards in FPH`, pvalcat)



ft <- flextable(TabChiPeriodAgeC) %>%
  merge_v(j = ~ deathrecency_cat_dss + cstatus_agesp_dss + pvalcat) %>%
  set_header_labels(values = c("Recall period of death (years prior to validation study)",
                               "Age group of death", "COD",
                               "Agreement", "Displaced backwards in FPH", 
                               "Displaced forwards in FPH", "p-value")) %>%
  set_caption(caption = "Displacement in recall period of death by age. Table displays counts of deaths in the HDSS that were correctly classified as occurring in the same five-year period in the FPH, or were displaced to a more distant or more recent period, alongside p-values from Chi-squared tests for significance.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(TabChiPeriodAgeC), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft


doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-displacement-ageCauseChi.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

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


