################################################################################
#' @description analyse additions of live births and deaths
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
library(officer)
library(ggplot2)
library(flextable)
library(ggh4x)
#' Inputs
overall <- readRDS("./gen/augment/overallName-recode.rds")
################################################################################

dat <- overall %>%
  mutate(subsampA = 1,
         subsampB = ifelse(dob_m_dss == doi_m_dss, 1, 0),
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
         eventDth_dss = ifelse(!is.na(cstatus_dss) & cstatus_dss == "Died", 1, 0),
         # deaths in survey
         eventDth_sur = ifelse(!is.na(cstatus_sur) & cstatus_sur == "Died", 1, 0),
         # live births in dss
         eventLB_dss = ifelse(!is.na(pregout_dss) & pregout_dss == "Live birth", 1, 0),
         # live births in survey
         eventLB_sur = ifelse(!is.na(c223) & c223 == "Live birth", 1, 0),
         # deaths in either source
         eventDth = ifelse(eventDth_dss == 1 | eventDth_sur == 1, 1, 0),
         # births in either source
         eventLB = ifelse(eventLB_dss == 1 | eventLB_sur == 1, 1, 0),
         # set denominator for deaths assesssment
         denomA = ifelse(subsampA == 1 & eventDth_dss == 1, 1, 0),
         denomB = ifelse(subsampB == 1 & eventDth == 1, 1, 0),
         denomC = ifelse((subsampC_dss == 1 | subsampC_sur == 1) & eventDth == 1, 1, 0),
         # set denominator for births assessment
         denomAlb = ifelse(subsampA == 1 & eventLB_dss == 1, 1, 0),
         denomBlb = ifelse(subsampB == 1 & eventLB == 1, 1, 0),
         denomClb = ifelse((subsampC_dss == 1 | subsampC_sur == 1) & eventLB == 1, 1, 0),
         )

dat %>%
  select(rid_m, denomC) %>%
  distinct() %>%
  group_by(denomC) %>%
  summarise(n = n()) # 265 women with residency in prev 15 years with live births in dss or fph during that time

# Omissions - deaths ---------------------------------------------------------------

# omissions of deaths
tabD <- dat %>%
  filter(denomA == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omissions of age-specific deaths
tabDage <- dat %>%
  filter(denomA == 1) %>%
  group_by(type, cstatus_agesp_dss) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_dss, n, per, total) 
tabDtotal <- dat %>%
  filter(denomA == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA_character_
  )) %>%
  group_by(cstatus_agesp_dss) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    type = "Total",
    total = sum(n),
    per = n / total * 100
  ) %>%
  select(type, cstatus_agesp_dss, n, per, total)
tabDage <- tabDage %>%
  bind_rows(tabDtotal) %>%
  group_by(type, total) %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss,
                             levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+"))) %>%
  complete(cstatus_agesp_dss, fill = list(n = 0, per = 0))

# combine
tabComb <- tabD %>% 
  rename(n_dth = n, per_dth = per) %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_dss = factor(cstatus_agesp_dss,
                               levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+", "Total"))) %>%
  arrange(rank, cstatus_agesp_dss)  %>%
  select(type,n_dth, per_dth, cstatus_agesp_dss, n, per) 

tabComb1 <- tabComb %>%
  mutate(subsample = "All-women") %>%
  select(subsample, everything())

# Additions and omissions - deaths: lifelong resident -------------------------------------------------

# additions or omission of deaths
tabD <- dat %>%
  filter(denomB == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omission or additions of age-specific deaths
tabDage <- dat %>%
  filter(denomB == 1) %>%
  group_by(type, cstatus_agesp_comb) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_comb, n, per, total) 
tabDtotal <- dat %>%
  filter(denomB == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA_character_
  )) %>%
  group_by(cstatus_agesp_comb) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    type = "Total",
    total = sum(n),
    per = n / total * 100
  ) %>%
  select(type, cstatus_agesp_comb, n, per, total)
tabDage <- tabDage %>%
  bind_rows(tabDtotal) %>%
  group_by(type, total) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb,
                                    levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+"))) %>%
  complete(cstatus_agesp_comb, fill = list(n = 0, per = 0))


# combine
tabComb <- tabD %>% 
  rename(n_dth = n, per_dth = per) %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_comb = factor(cstatus_agesp_comb,
                               levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+", "Total"))) %>%
  arrange(rank, cstatus_agesp_comb)  %>%
  select(type, n_dth, per_dth, cstatus_agesp_comb, n, per) 

tabComb2 <- tabComb %>%
  mutate(subsample = "Lifelong-resident") %>%
  select(subsample, everything())


# Additions and omissions - deaths: recent pregnancies -------------------------------------------------

# additions or omission of deaths
tabD <- dat %>%
  filter(denomC == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )

# omission or additions of age-specific deaths
tabDage <- dat %>%
  filter(denomC == 1) %>%
  group_by(type, cstatus_agesp_comb) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_agesp_comb, n, per, total) 
tabDtotal <- dat %>%
  filter(denomC == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA_character_
  )) %>%
  group_by(cstatus_agesp_comb) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    type = "Total",
    total = sum(n),
    per = n / total * 100
  ) %>%
  select(type, cstatus_agesp_comb, n, per, total)
tabDage <- tabDage %>%
  bind_rows(tabDtotal) %>%
  group_by(type, total) %>%
  mutate(cstatus_agesp_comb = factor(cstatus_agesp_comb,
                                     levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+"))) %>%
  complete(cstatus_agesp_comb, fill = list(n = 0, per = 0))

# combine
tabComb <- tabD %>% 
  rename(n_dth = n, per_dth = per) %>%
  left_join(tabDage, by = c("type" = "type", "n_dth" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_agesp_comb = factor(cstatus_agesp_comb,
                                levels = c("Neonatal", "Postneonatal", "1-4", "5-9", "10+", "Total"))) %>%
  arrange(rank, cstatus_agesp_comb)  %>%
  select(type, n_dth, per_dth, cstatus_agesp_comb, n, per) 

tabComb3 <- tabComb %>%
  mutate(subsample = "Recent-pregnancies") %>%
  select(subsample, everything())


# Create figure dat -------------------------------------------------------

figdatA1 <- dat %>%
  filter(denomAlb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "All-women",
         event = "Live births")
figdatA2 <- dat %>%
  filter(denomAlb == 1) %>%
  filter(cstatus_dss == "Surviving") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "All-women",
         event = "Child surviving")
figdatA3 <- dat %>%
  filter(denomA == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "All-women",
         event = "Child died")
figdatA <- rbind(figdatA1, figdatA2, figdatA3)
figdatA <- figdatA %>%
  pivot_longer(cols = c(n, per))

figdatB1 <- dat %>%
  filter(denomBlb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Lifelong-residents",
         event = "Live births")
figdatB2 <- dat %>%
  filter(denomBlb == 1) %>%
  filter(cstatus_dss == "Surviving" | cstatus_sur == "Surviving") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Lifelong-residents",
         event = "Child surviving")
figdatB3 <- dat %>%
  filter(denomB == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Lifelong-residents",
         event = "Child died")
figdatB <- rbind(figdatB1, figdatB2, figdatB3)
figdatB <- figdatB %>%
  pivot_longer(cols = c(n, per))


figdatC1 <- dat %>%
  filter(denomClb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Recent-births",
         event = "Live births")
figdatC2 <- dat %>%
  filter(denomClb == 1) %>%
  filter(cstatus_dss == "Surviving" | cstatus_sur == "Surviving") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Recent-births",
         event = "Child surviving")
figdatC3 <- dat %>%
  filter(denomC == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Recent-births",
         event = "Child died")
figdatC <- rbind(figdatC1, figdatC2, figdatC3)
figdatC <- figdatC %>%
  pivot_longer(cols = c(n, per)) 


allfigdat <- rbind(figdatA, figdatB, figdatC)
allfigdat$name <- ifelse(allfigdat$name == "n", "N", "%")
allfigdat$name <- factor(allfigdat$name, levels = c("N", "%"))
totals <- allfigdat %>%
  filter(name == "N") %>%
  group_by(subsample, event) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  mutate(name = "N") %>%
  mutate(name = factor(name, levels = c("N", "%")))

# numbers for paper
allfigdat %>% filter(type == "Omission" & event == "Live births" & subsample == "All-women")
allfigdat %>% filter(type == "Omission" & event == "Child died" & subsample == "All-women")
allfigdat %>% filter(type == "Omission" & event == "Child surviving" & subsample == "All-women")

allfigdat %>% filter(type == "Match" & event == "Live births" & subsample == "Lifelong-residents")
allfigdat %>% filter(type == "Omission" & event == "Live births" & subsample == "Lifelong-residents")
allfigdat %>% filter(type == "Addition" & event == "Live births" & subsample == "Lifelong-residents")

allfigdat %>% filter(type == "Match" & event == "Live births" & subsample == "Recent-births")
allfigdat %>% filter(type == "Omission" & event == "Live births" & subsample == "Recent-births")
allfigdat %>% filter(type == "Addition" & event == "Live births" & subsample == "Recent-births")
allfigdat %>% filter(type == "Omission" & event == "Child died" & subsample == "Recent-births")

# Figure: matches, omissions, and additions -------------------------------


myplot <- allfigdat %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 0))) %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals,
    aes(x = event, y = total, label = total),
    hjust = -0.05,
    size = 3
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_flip() +
  facet_grid(subsample ~ name, scales = "free", switch = "x") +
  labs(x = "", y = "", title = "Matching of live births in HDSS with FPH") +
  facetted_pos_scales(
    y = list(
      name == "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0.1))),
      name != "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0)))
    )
  ) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(face = "bold"),
    legend.position = "bottom"
  )
myplot
ggsave("./gen/figures/fig-matching.png", myplot, width = 8, height = 5, dpi = 500)



# Figure with all horizontal text -----------------------------------------

p1 <- allfigdat %>%
  filter(subsample == "All-women") %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 0))) %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals %>% filter(subsample == "All-women"),
    aes(x = event, y = total, label = total),
    hjust = -0.05,
    size = 3
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, scales = "free_x", switch = "x") +
  labs(x = "", y = "", subtitle = "All-women") +
  facetted_pos_scales(
    y = list(
      name == "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0.1))),
      name != "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0)))
    )
  ) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_blank(),
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text = element_text(color = "black", face = "bold", hjust = 0.5),
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black", face = "bold", hjust = 0.5),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )
p2 <- allfigdat %>%
  filter(subsample == "Lifelong-residents") %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 0))) %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals %>% filter(subsample == "Lifelong-residents"),
    aes(x = event, y = total, label = total),
    hjust = -0.05,
    size = 3
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, scales = "free_x", switch = "x") +
  labs(x = "", y = "", subtitle = "Lifelong-residents") +
  facetted_pos_scales(
    y = list(
      name == "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0.1))),
      name != "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0)))
    )
  ) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.x = element_blank(),
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text = element_text(color = "black", face = "bold", hjust = 0.5),
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black", face = "bold", hjust = 0.5),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

p3 <- allfigdat %>%
  filter(subsample == "Recent-births") %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 0))) %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals %>% filter(subsample == "Recent-births"),
    aes(x = event, y = total, label = total),
    hjust = -0.05,
    size = 3
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~name, scales = "free_x", switch = "x") +
  labs(x = "", y = "", subtitle = "Recent-births") +
  facetted_pos_scales(
    y = list(
      name == "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0.1))),
      name != "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0)))
    )
  ) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    legend.position = "bottom",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text = element_text(color = "black", face = "bold", hjust = 0.5),
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black", face = "bold", hjust = 0.5),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

combined_plot <- p1 / p2 / p3
ggsave(
  filename = "./gen/figures/fig-matching-v2.png",
  plot = combined_plot,
  width = 8,
  height = 5,
  dpi = 500
)

# PAA figure: matching, omissions, additions ------------------------------

allfigdat <- figdatA
allfigdat$name <- ifelse(allfigdat$name == "n", "N", "%")
allfigdat$name <- factor(allfigdat$name, levels = c("N", "%"))
totals <- allfigdat %>%
  filter(name == "N") %>%
  group_by(subsample, event) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  mutate(name = "N") %>%
  mutate(name = factor(name, levels = c("N", "%")))
myplot <- allfigdat %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 1))) %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals,
    aes(x = event, y = total, label = total),
    hjust = -0.05,
    size = 4
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~ name, nrow = 2, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.45, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
myplot

ggsave("./gen/figures/paa/matching-aw.png", myplot, width = 6, height = 6, dpi = 500)


allfigdat <- figdatC
allfigdat$name <- ifelse(allfigdat$name == "n", "N", "%")
allfigdat$name <- factor(allfigdat$name, levels = c("N", "%"))
totals <- allfigdat %>%
  filter(name == "N") %>%
  group_by(subsample, event) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  mutate(name = "N") %>%
  mutate(name = factor(name, levels = c("N", "%")))
myplot <- allfigdat %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 1))) %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = totals,
    aes(x = event, y = total, label = total),
    hjust = -0.05,
    size = 4
  ) +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~ name, nrow = 2, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "") +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
myplot

ggsave("./gen/figures/paa/matching-rp.png", myplot, width = 6, height = 6, dpi = 500)
scales::viridis_pal(option = "plasma", direction = -1, begin = 0.1, end = 0.8)(3)

#scales::viridis_pal(option = "plasma", direction = -1, begin = 0.1, end = 0.8)(3)
allfigdat <- rbind(figdatA, figdatC)
allfigdat$name <- ifelse(allfigdat$name == "n", "N", "%")
allfigdat$name <- factor(allfigdat$name, levels = c("N", "%"))
myplot <- allfigdat %>%
  filter(event != "Live births") %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving"))) %>% #, "Live births"))) %>%
  mutate(type = factor(type, levels = c("Addition", "Omission", "Match"))) %>%
  mutate(label = ifelse(name == "N", value, round(value, 0))) %>%
  filter(name == "%") %>%
  ggplot() +
  geom_bar(aes(x = event, y = value, fill = type), stat = "identity", position = "stack") +
  geom_text(
    data = ~subset(.x, name == "%"),
    aes(x = event, y = value, label = label, group = type),
    position = position_stack(vjust = 0.5),
    size = 6,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~ subsample, nrow = 1, scales = "free_x") +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    values = c(
      "Match"    = "#42049EFF",
      "Omission" = "#BF3984FF",
      "Addition" = "#FCA636FF"
    ),
    name = ""
  ) +
  theme(legend.position = "bottom", text = element_text(size = 18)) +
  guides(fill = guide_legend(reverse = TRUE))
myplot

ggsave("./gen/figures/paa/matching-both-justper.png", myplot, width = 12, height = 6, dpi = 500)



# Flextable ---------------------------------------------------------------

tabComb3 <- tabComb1 %>%
  rename(cstatus_agesp_comb = cstatus_agesp_dss) %>%
  bind_rows(tabComb2) %>%
  bind_rows(tabComb3) %>%
  mutate(per_dth = sprintf("%.2f", round(per_dth, 2)),
         per = sprintf("%.2f", round(per, 2))) %>%
  mutate(per = ifelse(per == "NA", "", per))

ft <- flextable(tabComb3) %>%
  set_header_labels(values = c("Sample", "Event agreement", "N", "%","Age group", "N", "%")) %>%
  add_header_row(values = c(" ", "Death"), colwidths = c(2, 5)) %>%
  set_caption(caption = "Omissions and additions deaths in FPH among all-women, lifelong-resident, and recent-pregnancies subsamples.") %>%
  merge_v(j = ~ subsample + type + n_dth + per_dth) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabComb3), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-additionsOmissions.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")


# All the same, but for live births ---------------------------------------

## Sample A
# omissions of LB
tabLB <- dat %>%
  filter(denomAlb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )
# omissions of lb by survival
tabLBsurv <- dat %>%
  filter(denomAlb == 1) %>%
  group_by(type, cstatus_dss) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_dss, n, per, total) 
tabLBsurvtotal <- dat %>%
  filter(denomAlb == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA_character_
  )) %>%
  group_by(cstatus_dss) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    type = "Total",
    total = sum(n),
    per = n / total * 100
  ) %>%
  select(type, cstatus_dss, n, per, total)
tabLBsurv <- tabLBsurv %>%
  bind_rows(tabLBsurvtotal) 

# combine
tabComb <- tabLB %>% 
  rename(n_lb = n, per_lb = per) %>%
  left_join(tabLBsurv, by = c("type" = "type", "n_lb" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatus_dss = factor(cstatus_dss, levels = c("Died", "Surviving", "Total"))) %>%
  arrange(rank, cstatus_dss)  %>%
  select(type, n_lb, per_lb, cstatus_dss, n, per) 
tabComb1 <- tabComb %>%
  mutate(subsample = "All-women") %>%
  select(subsample, everything())


# Sample B
# additions or omission of deaths
tabLB <- dat %>%
  filter(denomBlb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )
# omission or additions of age-specific deaths
tabLBsurv <- dat %>%
  filter(denomBlb == 1) %>%
  group_by(type, cstatus_comb) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_comb, n, per, total) 
tabLBsurvtotal <- dat %>%
  filter(denomBlb == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  group_by(cstatus_comb) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    type = "Total",
    total = sum(n),
    per = n / total * 100
  ) %>%
  select(type, cstatus_comb, n, per, total)
tabLBsurv <- tabLBsurv %>%
  bind_rows(tabLBsurvtotal) 
# combine
tabComb <- tabLB %>% 
  rename(n_lb = n, per_lb = per) %>%
  left_join(tabLBsurv, by = c("type" = "type", "n_lb" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatusp_comb = factor(cstatus_comb, levels = c("Died", "Surviving", "Total"))) %>%
  arrange(rank, cstatus_comb)  %>%
  select(type, n_lb, per_lb, cstatus_comb, n, per) 
tabComb2 <- tabComb %>%
  mutate(subsample = "Lifelong-resident") %>%
  select(subsample, everything())

# Sample C
# additions or omission of deaths
tabLB <- dat %>%
  filter(denomClb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition", # not reported in hdss, added in validation study
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  select(type, n, per, total) %>%
  bind_rows(
    summarise(., 
              type = "Total",
              n = sum(n),
              per = 100)
  )
# omission or additions of age-specific deaths
tabLBsurv <- dat %>%
  filter(denomClb == 1) %>%
  group_by(type, cstatus_comb) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  select(type, cstatus_comb, n, per, total) 
tabLBsurvtotal <- dat %>%
  filter(denomClb == 1) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    type == "VS_NoMatch" ~ "Addition",
    TRUE ~ NA
  )) %>%
  group_by(cstatus_comb) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    type = "Total",
    total = sum(n),
    per = n / total * 100
  ) %>%
  select(type, cstatus_comb, n, per, total)
tabLBsurv <- tabLBsurv %>%
  bind_rows(tabLBsurvtotal) 
# combine
tabComb <- tabLB %>% 
  rename(n_lb = n, per_lb = per) %>%
  left_join(tabLBsurv, by = c("type" = "type", "n_lb" = "total")) %>%
  mutate(rank = case_when(
    type == "Match" ~ 1,
    type == "Omission" ~ 2,
    type == "Addition" ~ 3,
    type == "Total" ~ 4,
    TRUE ~ NA),
    cstatusp_comb = factor(cstatus_comb, levels = c("Died", "Surviving", "Total"))) %>%
  arrange(rank, cstatus_comb)  %>%
  select(type, n_lb, per_lb, cstatus_comb, n, per) 
tabComb3 <- tabComb %>%
  mutate(subsample = "Recent-pregnancies") %>%
  select(subsample, everything())

tabComb3 <- tabComb1 %>%
  rename(cstatus_comb = cstatus_dss) %>%
  bind_rows(tabComb2) %>%
  bind_rows(tabComb3) %>%
  mutate(per_lb = sprintf("%.2f", round(per_lb, 2)),
         per = sprintf("%.2f", round(per, 2))) %>%
  mutate(per = ifelse(per == "NA", "", per))

ft <- flextable(tabComb3) %>%
  set_header_labels(values = c("Sample", "Event agreement", "N", "%","Survival status", "N", "%")) %>%
  add_header_row(values = c(" ", "Live births"), colwidths = c(2, 5)) %>%
  set_caption(caption = "Omissions and additions live births in FPH among all-women, lifelong-resident, and recent-pregnancies subsamples.") %>%
  merge_v(j = ~ subsample + type + n_lb + per_lb) %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:ncol(tabComb3), part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-additionsOmissions-lb.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

