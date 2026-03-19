################################################################################
#' @description Prepare figures for submission to PAA 2026
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(haven)
library(lubridate)
library(viridis)
#' Inputs
hdss <- read_dta("C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/ACSU5MR/acsu5mr-validation-bd/data/ACSU5MR_FILES/hdss_final_all2.dta")
survey <- read_dta("C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/ACSU5MR/acsu5mr-validation-bd/data/ACSU5MR_FILES/survey_final_all2.dta")
overall <- read_dta("C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/ACSU5MR/acsu5mr-validation-bd/data/ACSU5MR_FILES/overall.dta")
overall_aug <- readRDS("C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/ACSU5MR/acsu5mr-validation-bd/reports/preliminary-assessment/gen-files-used/overall-aug.rds")
################################################################################

mypal2 <- rev(plasma(n = 3)[1:2])

# total lb and sb
overall_aug %>% 
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  group_by(c223) %>%
  summarise(n = n())

# matching by lb and sb
overall_aug %>% 
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  mutate(type = factor(type, levels = c("VS_NoMatch", "VS_Match"), labels = c("Nomatch", "Match"))) %>%
  group_by(type, c223) %>%
  summarise(n = n()) %>%
  group_by(c223) %>%
  mutate(total = sum(n),
         per = sprintf("%0.1f", round(n/total*100, 1)))

# matches of live births and stillbirths by DOB year
p <- overall_aug %>%
  filter(c223 %in% c("Live birth", "Stillbirth")) %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  mutate(doby_c_sur = year(dob_c_sur),
         type = factor(type, levels = c("VS_NoMatch", "VS_Match"),
                       labels = c("No match", "Match"))) %>%
  group_by(c223, doby_c_sur, type) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(c223, doby_c_sur) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  pivot_longer(cols = c(N, Proportion)) %>%
  ggplot() +
  geom_bar(aes(x = doby_c_sur, y = value, fill = type),
           stat = "identity", width = 0.9) +
  scale_fill_manual(values = mypal2) +
  labs(x = "DOB year",title = "Matching of events in FPH with HDSS by DOB year") +
  facet_grid(name ~ c223, scales = "free") +
  theme_bw(base_size = 7) + 
  theme(
    #plot.title = element_blank(),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 7),
    strip.text = element_text(size = 7, margin = margin(0.1,0.1,0.1,0.1, "cm")),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),   # very tight facet spacing
    plot.margin = margin(1, 1, 1, 1, "mm") # almost no outer margin
  )
ggsave("./gen/paa2026-figures/matches-by-dob.png",p, height = 2.5, width = 5, dpi = 600)
# numbers
overall_aug %>%
  filter(c223 %in% c("Live birth", "Stillbirth")) %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  mutate(doby_c_sur = year(dob_c_sur),
         type = factor(type, levels = c("VS_NoMatch", "VS_Match"),
                       labels = c("No match", "Match")),
         yearcat = cut(doby_c_sur, breaks = c(1992, 2000, 2010, 2020, 2025, Inf), include.lowest = TRUE, right = FALSE,
                       labels = c("1992-1999", "2000-2009", "2010-2019", "2020-2024", "2025+"))) %>%
  group_by(c223, yearcat, type) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(c223, yearcat) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  filter(type == "Match") %>%
  as.data.frame()


# omissions in FPH by strata
p <- overall_aug %>%
  filter(sample_c != "0_10+") %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>%
  mutate(sample_c = case_when(
    sample_c == "6" ~ "0_Surviving", # group those in strata6 (mothers with all surviving children) with all other survivors
    sample_c == "1_NoVAyet" ~ "1_Unk",
    sample_c == "2_NoVAyet" ~ "2_Unk",
    sample_c == "3_NoVAyet" ~ "3_Unk",
    TRUE ~ sample_c),
    sample_c1 = case_when(
      sample_c1 == "6" ~ "0", # group those in strata6 (mothers with all surviving children) with all other survivors
      TRUE ~ sample_c1
    )) %>%
  mutate(sample_c = factor(sample_c, levels = c("0_Surviving",
                                                "4_Stillbirth",
                                                "1_BirthAsphyxia", "1_Other", "1_Unk",
                                                "2_RI+Congenital", "2_Other", "2_Unk",
                                                "3_Drowning", "3_Other", "3_Unk", "5"),
                           labels = c("Surviving", 
                                      "Stillbirth", 
                                      "Neonatal: birth asphyxia", "Neonatal: other", "Neonatal: unk.",
                                      "Postneonatal: RI, congen.", "Postneonatal: other", "Postneonatal: unk.",
                                      "Child: drowning", "Child: other", "Child: unk.",
                                      "5-9y death"))) %>%
  mutate(type = factor(type, levels = c("HDSS_NoMatch", "VS_Match"), labels = c("Omitted from FPH", "Reported in FPH and HDSS"))) %>%
  group_by(sample_c, sample_c1, type) %>%
  summarise(N = n()) %>% 
  group_by(sample_c) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  pivot_longer(cols = c(N, Proportion)) %>%
  ggplot() +
  geom_bar(aes(x = sample_c, y = value, fill = type), stat = "identity") +
  scale_fill_manual(values = mypal2) +
  labs(y = "",  x = "", fill = "Reported", 
       title = "Omissions from FPH by survival status and age/cause of death") +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  theme_bw(base_size = 7) + 
  theme(
    #plot.title = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 7),
    strip.text = element_text(size = 7, margin = margin(0.1,0.1,0.1,0.1, "cm")),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),   # very tight facet spacing
    plot.margin = margin(1, 1, 1, 1, "mm") # almost no outer margin
  )
ggsave("./gen/paa2026-figures/omissions-by-cod.png",p, height = 2.5, width = 6.25, dpi = 600)
# numbers
overall_aug %>%
  filter(sample_c != "0_10+") %>%
  filter(type %in% c("VS_Match", "HDSS_NoMatch")) %>%
  mutate(sample_c = case_when(
    sample_c == "6" ~ "0_Surviving", # group those in strata6 (mothers with all surviving children) with all other survivors
    sample_c == "1_NoVAyet" ~ "1_Unk",
    sample_c == "2_NoVAyet" ~ "2_Unk",
    sample_c == "3_NoVAyet" ~ "3_Unk",
    TRUE ~ sample_c),
    sample_c1 = case_when(
      sample_c1 == "6" ~ "0", # group those in strata6 (mothers with all surviving children) with all other survivors
      TRUE ~ sample_c1
    )) %>%
  mutate(sample_c = factor(sample_c, levels = c("0_Surviving",
                                                "4_Stillbirth",
                                                "1_BirthAsphyxia", "1_Other", "1_Unk",
                                                "2_RI+Congenital", "2_Other", "2_Unk",
                                                "3_Drowning", "3_Other", "3_Unk", "5"),
                           labels = c("Surviving", 
                                      "Stillbirth", 
                                      "Neonatal: birth asphyxia", "Neonatal: other", "Neonatal: unk.",
                                      "Postneonatal: RI, congen.", "Postneonatal: other", "Postneonatal: unk.",
                                      "Child: drowning", "Child: other", "Child: unk.",
                                      "5-9y death"))) %>%
  mutate(type = factor(type, levels = c("HDSS_NoMatch", "VS_Match"), labels = c("Omitted from FPH", "Reported in FPH and HDSS"))) %>%
  group_by(sample_c, sample_c1, type) %>%
  summarise(N = n()) %>% 
  group_by(sample_c) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  filter(type == "Omitted from FPH") %>%
  as.data.frame()

# additions in FPH by strata
p <- overall_aug %>%
  filter(sample_c != "0_10+") %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>% 
  filter(dob_c_sur < doo_m_dss) %>% # exclude outcomes taking place after out-migration
  mutate(doby_c_sur = year(dob_c_sur)) %>% 
  mutate(type = factor(type, levels = c("VS_NoMatch", "VS_Match"), labels = c("Addition in FPH", "Reported in FPH and HDSS"))) %>%
  mutate(sample_c = case_when(
    sample_c == "6" ~ "0_Surviving", # group those in strata6 (mothers with all surviving children) with all other survivors
    TRUE ~ sample_c),
    sample_c1 = case_when(
      sample_c1 == "6" ~ "0", # group those in strata6 (mothers with all surviving children) with all other survivors
      TRUE ~ sample_c1
    )) %>%
  mutate(sample_c1 = factor(sample_c1, levels = c("0", "4",  "1", "2", "3","5"),
                            labels = c("Surviving", "Stillbirth", "Neonatal death", "Postneonatal death", "Child death",
                                        "5-9y death"))) %>%
  group_by(sample_c1, type) %>%
  summarise(N = n()) %>% 
  group_by(sample_c1) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  pivot_longer(cols = c(N, Proportion)) %>%
  ggplot() +
  geom_bar(aes(x = sample_c1, y = value, fill = type), stat = "identity") +
  scale_fill_manual(values = mypal2) +
  labs(x = "", y ="", fill = "Reported", title = "Additions in FPH by survival status and age of death") +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  theme_bw(base_size = 7) + 
  theme(
    #plot.title = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 7),
    strip.text = element_text(size = 7, margin = margin(0.1,0.1,0.1,0.1, "cm")),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),   # very tight facet spacing
    plot.margin = margin(1, 1, 1, 1, "mm") # almost no outer margin
  )
ggsave("./gen/paa2026-figures/additions-by-age.png",p, height = 2.5, width = 6.25, dpi = 600)
# numbers
overall_aug %>%
  filter(sample_c != "0_10+") %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>% 
  filter(dob_c_sur < doo_m_dss) %>% # exclude outcomes taking place after out-migration
  mutate(doby_c_sur = year(dob_c_sur)) %>% 
  mutate(type = factor(type, levels = c("VS_NoMatch", "VS_Match"), labels = c("Addition in FPH", "Reported in FPH and HDSS"))) %>%
  mutate(sample_c = case_when(
    sample_c == "6" ~ "0_Surviving", # group those in strata6 (mothers with all surviving children) with all other survivors
    TRUE ~ sample_c),
    sample_c1 = case_when(
      sample_c1 == "6" ~ "0", # group those in strata6 (mothers with all surviving children) with all other survivors
      TRUE ~ sample_c1
    )) %>%
  mutate(sample_c1 = factor(sample_c1, levels = c("0", "4",  "1", "2", "3","5"),
                            labels = c("Surviving", "Stillbirth", "Neonatal death", "Postneonatal death", "Child death",
                                       "5-9y death"))) %>%
  group_by(sample_c1, type) %>%
  summarise(N = n()) %>% 
  group_by(sample_c1) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  filter(type == "Addition in FPH")
