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
library(patchwork)
library(viridisLite)
#' Inputs
overall <- readRDS("./gen/augment/overallName-recode.rds")
overallDate <- readRDS("./gen/augment/overallDate-recode.rds")
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
    type == "HDSS_NoMatch" ~ "Omission",
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
    type == "HDSS_NoMatch" ~ "Omission",
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
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition", 
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
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition",
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
    type == "VS_NoMatch" ~ "Addition",
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
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition",
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
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition", 
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
ggsave("./gen/figures/fig-matching-noaug.png", myplot, width = 8, height = 5, dpi = 500)



# Figure: Demography -----------------------------------------

plasma(n = 3, begin = 0.1, end = 0.8, direction = -1)
p1 <- allfigdat %>%
  filter(subsample == "All-women") %>%
  mutate(event = factor(event, levels = c("Child died", "Child surviving", "Live births"))) %>%
  mutate(type = factor(type, levels = c("Omission", "Match"))) %>%
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
     name == "N" ~ scale_y_continuous(limits = c(0, 2200), breaks = c(0, 500, 1000,1500, 2000),
                                      expand = expansion(mult = c(0, 0.1))),
     name != "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0)))
   )
  ) +
  scale_fill_manual(values = c("#BF3984FF", "#42049EFF")) +
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
      name == "N" ~ scale_y_continuous(limits = c(0, 2200), breaks = c(0, 500, 1000,1500, 2000),
                                       expand = expansion(mult = c(0, 0.1))),
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
      name == "N" ~ scale_y_continuous(limits = c(0, 2200), breaks = c(0, 500, 1000,1500, 2000),
                                       expand = expansion(mult = c(0, 0.1))),
      name != "N" ~ scale_y_continuous(expand = expansion(mult = c(0, 0)))
    )
  ) +
  scale_fill_viridis_d(option = "plasma", direction = -1, begin = 0.1, end = 0.8, name = "",
                       guide = guide_legend(reverse = TRUE)) +
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
combined_plot
ggsave(
  filename = "./gen/figures/fig-matching-v2.png",
  plot = combined_plot,
  width = 8,
  height = 5,
  dpi = 500
)


# Date figure dat ---------------------------------------------------------

datDate <- overallDate %>%
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


figdatA1 <- datDate %>%
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
figdatA2 <- datDate %>%
  filter(denomAlb == 1) %>%
  filter(cstatus_dss == "Surviving") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "All-women",
         event = "Child surviving")
figdatA3 <- datDate %>%
  filter(denomA == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "All-women",
         event = "Child died")
figdatA <- rbind(figdatA1, figdatA2, figdatA3)
figdatA <- figdatA %>%
  pivot_longer(cols = c(n, per))

figdatB1 <- datDate %>%
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
figdatB2 <- datDate %>%
  filter(denomBlb == 1) %>%
  filter(cstatus_dss == "Surviving" | cstatus_sur == "Surviving") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition", 
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Lifelong-residents",
         event = "Child surviving")
figdatB3 <- datDate %>%
  filter(denomB == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Lifelong-residents",
         event = "Child died")
figdatB <- rbind(figdatB1, figdatB2, figdatB3)
figdatB <- figdatB %>%
  pivot_longer(cols = c(n, per))


figdatC1 <- datDate %>%
  filter(denomClb == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission", # reported in hdss, omission from validation study
    type == "VS_NoMatch" ~ "Addition",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Recent-births",
         event = "Live births")
figdatC2 <- datDate %>%
  filter(denomClb == 1) %>%
  filter(cstatus_dss == "Surviving" | cstatus_sur == "Surviving") %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition",
    type == "VS_Match" ~ "Match",
    TRUE ~ NA
  )) %>%
  mutate(subsample = "Recent-births",
         event = "Child surviving")
figdatC3 <- datDate %>%
  filter(denomC == 1) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(type = case_when(
    type == "HDSS_NoMatch" ~ "Omission",
    type == "VS_NoMatch" ~ "Addition", 
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

ggsave("./gen/figures/fig-date-matching.png", myplot, width = 8, height = 5, dpi = 500)
