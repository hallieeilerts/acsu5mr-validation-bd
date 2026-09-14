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
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4 years" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4 years" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9 years" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4 years") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9 years" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4 years"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "10+ years" ~ "youngerfph",
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
                                    levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years","Total"))) %>%
  arrange(cstatus_agesp_dss)

tab_agreeage$n_olderfph[is.na(tab_agreeage$n_olderfph)] <- 0
tab_agreeage$per_olderfph[is.na(tab_agreeage$per_olderfph)] <- "0.00"
tab_agreeage$n_youngerfph[is.na(tab_agreeage$n_youngerfph) & tab_agreeage$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeage$per_youngerfph[is.na(tab_agreeage$per_youngerfph) & tab_agreeage$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeage$n_youngerfph[is.na(tab_agreeage$n_youngerfph)] <- 0
tab_agreeage$per_youngerfph[is.na(tab_agreeage$per_youngerfph)] <- "0.00"

# Age/cause agreement -----------------------------------------------------

agree_agecause <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur == "Neonatal" ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur != "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4 years" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4 years" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9 years" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4 years") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9 years" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4 years"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "10+ years" ~ "youngerfph",
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
                                    levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"))) %>%
  arrange(cstatus_agesp_dss) %>%
  filter(!(cstatus_agesp_dss %in% c("5-9", "10+")))

tab_agreeagecause$n_olderfph[is.na(tab_agreeagecause$n_olderfph)] <- 0
tab_agreeagecause$per_olderfph[is.na(tab_agreeagecause$per_olderfph)] <- "0.00"
tab_agreeagecause$n_youngerfph[is.na(tab_agreeagecause$n_youngerfph) & tab_agreeagecause$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeagecause$per_youngerfph[is.na(tab_agreeagecause$per_youngerfph) & tab_agreeagecause$cstatus_agesp_dss == "Neonatal"] <- "-"
tab_agreeagecause$n_youngerfph[is.na(tab_agreeagecause$n_youngerfph)] <- 0
tab_agreeagecause$per_youngerfph[is.na(tab_agreeagecause$per_youngerfph)] <- "0.00"

tab_agreeage
tab_agreeagecause

# Combine age and age/cause agreement -----------------------------------------------

tab_agree_ac <- tab_agreeage %>%
  mutate(cstrata_c = "All causes") %>%
  bind_rows(tab_agreeagecause) %>%
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    cstrata_c == "All causes" ~ 3,
    TRUE ~ 1
  )) %>%
  arrange(cstatus_agesp_dss, cod_rank) %>%
  select(cstatus_agesp_dss, cstrata_c, total, n_correct, per_correct, 
         n_olderfph, per_olderfph, 
         n_youngerfph, per_youngerfph)


# Total transferred into age group ----------------------------------------

# Deaths transferring into each age group from FPH (DSS is reference)
transfers_in <- dat %>%
  filter(cstatus_agesp_dss != cstatus_agesp_sur) %>%
  group_by(cstatus_agesp_sur) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(cstatus_agesp_dss = cstatus_agesp_sur) %>%
  bind_rows(
    summarise(.,
              cstatus_agesp_dss = "Total",
              n_in      = sum(n_in, na.rm = TRUE)
    )
  ) 

tab_agreeTrans_ac  <- tab_agree_ac %>%
  left_join(transfers_in, by = "cstatus_agesp_dss")

# Table 4: Age/cause transfers ---------------------------------------

# Format for flextable
table_with_headers <- tab_agreeTrans_ac %>%
  ungroup() %>%
  mutate(across(everything(), as.character)) %>%
  group_split(cstatus_agesp_dss, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      cstatus_agesp_dss = unique(df$cstatus_agesp_dss),
      cstrata_c = "",
      total = "",
      n_correct = "", 
      per_correct = "", 
      n_olderfph = "",
      per_olderfph = "", 
      n_youngerfph = "", 
      per_youngerfph = "", 
      n_in = ""
    )
    bind_rows(header_row, df)
  }) %>%
  mutate(recnr = 1:n()) %>%
  mutate( cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                     levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years", "Total"))) %>%
  arrange(cstatus_agesp_dss, recnr) %>%
  select(-recnr)

table_with_headers$cstatus_agesp_dss <- as.character(table_with_headers$cstatus_agesp_dss)

table_with_headers <- table_with_headers %>%
  filter(!(cstatus_agesp_dss == "5-9 years" & cstrata_c == "")) %>%
  filter(!(cstatus_agesp_dss == "10+ years" & cstrata_c == "")) %>%
  filter(!(cstatus_agesp_dss == "Total" & cstrata_c == ""))
table_with_headers$cstatus_agesp_dss[!(table_with_headers$cstatus_agesp_dss %in% c("5-9 years", "10+ years", "Total")) &
                                       table_with_headers$cstrata_c != ""] <- ""


ft <- flextable(table_with_headers) %>%
  merge_v(j = ~ cstatus_agesp_dss + n_in) %>%
  set_header_labels(values = c("Age group of death", "COD", 
                               "N HDSS", "N", "%","N", "%", "N", "%", "N")) %>%
  add_header_row(values = c(" ", "Agreement", "Older in FPH", "Younger in FPH", "FPH transferred into age group"), 
                 colwidths = c(3, 2, 2, 2, 1)) %>%
  set_caption(caption = "Age-at-death errors by cause of death. Table displays counts and percentages for deaths in the HDSS that were correctly classified as the same age group in the FPH, or reported as occurring in an older or younger age group. The number of deaths that were transferred into the age group in the FPH is also provided.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(table_with_headers), part = "all") %>%
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


# Table A1: Age/cause transfer associations ----------------------------------------

countsAgeCause <- dat %>%
  mutate(classified = case_when(
    cstatus_agesp_dss == cstatus_agesp_sur ~ "correct",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur == "Neonatal" ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "Postneonatal" &
      cstatus_agesp_sur != "Neonatal" ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4 years" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "1-4 years" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9 years" &
      cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4 years") ~ "youngerfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "5-9 years" &
      !(cstatus_agesp_sur %in% c("Neonatal","Postneonatal", "1-4 years"))  ~ "olderfph",
    cstatus_agesp_dss != cstatus_agesp_sur & cstatus_agesp_dss == "10+ years" ~ "youngerfph",
    TRUE ~ NA
  )) %>%
  group_by(cstatus_agesp_dss) %>%
  filter(n_distinct(cstrata_c) >= 2) %>%
  count(cstrata_c, classified)
# testing Chi
countsAgeCause %>%
  ungroup() %>%
  filter(cstatus_agesp_dss == "1-4 years") %>%
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
  mutate(cstatus_agesp_dss = factor( cstatus_agesp_dss, levels = c("Neonatal", "Postneonatal", "1-4 years"))) %>%
  mutate(cod_rank = case_when(
    cstrata_c == "Other" ~ 2,
    TRUE ~ 1
  )) %>%
  arrange(cstatus_agesp_dss, cod_rank) %>%
  select(-c(cod_rank))


table_with_headers <- TabChiAgeCause %>%
  ungroup() %>%
  mutate(across(everything(), as.character)) %>%
  group_split(cstatus_agesp_dss, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      cstatus_agesp_dss = unique(df$cstatus_agesp_dss),
      cstrata_c = "",
      Agreement = "",
      `Older in FPH` = "", 
      `Younger in FPH` = "", 
      pvalcat  = ""
    )
    bind_rows(header_row, df)
  }) %>%
  mutate(recnr = 1:n()) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                     levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years"))) %>%
  arrange(cstatus_agesp_dss, recnr) %>%
  select(-recnr)


table_with_headers$cstatus_agesp_dss <- as.character(table_with_headers$cstatus_agesp_dss)

table_with_headers$cstatus_agesp_dss[table_with_headers$cstrata_c != ""] <- ""

ft <- flextable(table_with_headers) %>%
  merge_v(j = ~ pvalcat) %>%
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

# Period agreement --------------------------------------------------------

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

# Period/age agreement ----------------------------------------------------

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

tab_agreeperiod
tab_agreeperiodage

# Combine period and period/age -------------------------------------------

tab_agree_pa <- tab_agreeperiod %>%
  mutate(cstatus_agesp_dss = "All") %>%
  bind_rows(tab_agreeperiodage) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                       levels = c("0-4", "5-9", "10-14", "15+","Total"))) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss,
                                    levels = c("Neonatal","Postneonatal", "1-4 years", "5-9 years", "10+ years", "All"))) %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss)


# Total transferred into period -------------------------------------------

transfers_in <- dat %>%
  filter(cstatus_agesp_dss == cstatus_agesp_sur,
         deathrecency_cat_dss != deathrecency_cat_sur) %>%
  group_by(deathrecency_cat_sur) %>%
  summarise(n_in = n(), .groups = "drop") %>%
  rename(deathrecency_cat_dss = deathrecency_cat_sur) %>%
  bind_rows(
    summarise(.,
              deathrecency_cat_dss = "Total",
              n_in      = sum(n_in, na.rm = TRUE)
    )
  ) 

tab_agreeTrans_pa <- tab_agree_pa %>%
  left_join(transfers_in, by = "deathrecency_cat_dss")

# Table 5: period transfers ----------------------------------------------------

ft <- tab_agreeTrans_pa %>%
  filter(cstatus_agesp_dss == "All") %>%
  select(deathrecency_cat_dss, total, n_correct, 
         per_correct, n_distantfph, per_distantfph, 
         n_recentfph, per_recentfph, n_in) %>%
  flextable() %>%
  set_header_labels(values = c("Recall period of death (years prior to validation study)",  
                               "N HDSS", "N", "%","N", "%", "N", "%", "N")) %>%
  add_header_row(values = c(" ", "Agreement", "Displaced backwards in FPH", "Displaced forwards in FPH", "Displaced into period in FPH"), 
                 colwidths = c(2, 2, 2, 2, 1)) %>%
  set_caption(caption = "Displacement in recall period of death. Table displays counts and percentages for deaths in the HDSS that were correctly classified as occurring in the same five-year period in the FPH, or were displaced to a more distant or more recent period. The number of deaths that were displaced into each five-year period in the FPH is also provided.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:9, part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-displacement.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# Test associations between recall period, age, cause --------------------------

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

# Table A2: associations period/age ----------------------------------------

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
                                    levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"),
                                    labels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"))) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, levels = c("0-4", "5-9", "10-14", "15+"))) %>%
  group_by(deathrecency_cat_dss) %>%
  complete(cstatus_agesp_dss, fill = list(Agreement = 0, `Displaced backwards in FPH` = 0,
                                          `Displaced forwards in FPH` = 0)) %>%
  left_join(chiPeriodAge, by = "deathrecency_cat_dss") %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss) 


table_with_headers <- TabChiPeriodAge  %>%
  ungroup() %>%
  mutate(across(everything(), as.character)) %>%
  group_split(deathrecency_cat_dss, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      deathrecency_cat_dss = unique(df$deathrecency_cat_dss),
      cstatus_agesp_dss = "",
      Agreement = "",
      `Displaced backwards in FPH` = "", 
      `Displaced forwards in FPH` = "", 
      pvalcat  = ""
    )
    bind_rows(header_row, df)
  }) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                    levels = c("0-4",  "5-9", "10-14", "15+"))) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                    levels = c("","Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"))) %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss) 

# table_with_headers <- table_with_headers %>%
#   filter(deathrecency_cat_dss != "15+") %>%
#   filter(!(cstatus_agesp_dss %in% c( "5-9 years", "10+ years")))

table_with_headers$deathrecency_cat_dss <- as.character(table_with_headers$deathrecency_cat_dss)

table_with_headers$deathrecency_cat_dss[table_with_headers$cstatus_agesp_dss != ""] <- ""



ft <- flextable(table_with_headers) %>%
  merge_v(j = ~ deathrecency_cat_dss + pvalcat) %>%
  set_header_labels(values = c("Recall period of death (years prior to validation study)",
                               "Age group of death", 
                               "Agreement", "Displaced backwards in FPH", 
                               "Displaced forwards in FPH", "p-value")) %>%
  set_caption(caption = "Displacement in recall period of death by age. Table displays counts of deaths in the HDSS that were correctly classified as occurring in the same five-year period in the FPH, or were displaced to a more distant or more recent period, alongside p-values from Chi-squared tests for significance.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(table_with_headers), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft


doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-displacement-ageChi.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# Table A3: associations period/age/cause ----------------------------------------

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
  filter(cstatus_agesp_dss %in% c("Neonatal", "Postneonatal", "1-4 years")) %>%
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
                                    levels = c("Neonatal", "Postneonatal", "1-4 years", "5-9 years", "10+ years"),
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



table_with_headers <- TabChiPeriodAgeC  %>%
  ungroup() %>%
  mutate(across(everything(), as.character)) %>%
  group_split(deathrecency_cat_dss, .keep = TRUE) %>%
  map_dfr(function(df) {
    header_row <- tibble(
      deathrecency_cat_dss = unique(df$deathrecency_cat_dss),
      cstatus_agesp_dss = "",
      COD = "",
      Agreement = "",
      `Displaced backwards in FPH` = "", 
      `Displaced forwards in FPH` = "", 
      pvalcat  = ""
    )
    bind_rows(header_row, df)
  }) %>%
  mutate(deathrecency_cat_dss = factor(deathrecency_cat_dss, 
                                       levels = c("0-4",  "5-9", "10-14"))) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, 
                                    levels = c("","Neonatal", "Postneonatal", "1-4 years"))) %>%
  arrange(deathrecency_cat_dss, cstatus_agesp_dss) 


table_with_headers$deathrecency_cat_dss <- as.character(table_with_headers$deathrecency_cat_dss)

table_with_headers$deathrecency_cat_dss[table_with_headers$cstatus_agesp_dss != ""] <- ""



ft <- flextable(table_with_headers) %>%
  merge_v(j = ~  cstatus_agesp_dss + pvalcat) %>%
  set_header_labels(values = c("Recall period of death (years prior to validation study)",
                               "Age group of death", "COD",
                               "Agreement", "Displaced backwards in FPH", 
                               "Displaced forwards in FPH", "p-value")) %>%
  set_caption(caption = "Displacement in recall period of death by age. Table displays counts of deaths in the HDSS that were correctly classified as occurring in the same five-year period in the FPH, or were displaced to a more distant or more recent period, alongside p-values from Chi-squared tests for significance.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(table_with_headers), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft


doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-period-displacement-ageCauseChi.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


