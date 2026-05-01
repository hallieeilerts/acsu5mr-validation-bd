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

# Age of death transfers --------------------------------------------------

# age transfer

dat %>%
  group_by(cstatus_agesp_dss, cstatus_agesp_sur) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) 

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
