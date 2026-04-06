################################################################################
#' @description analyse the overallDate file, event level, denominator E (matched pregnancies)
#' Assess:
#' Misclassification of stb and lb
#' I'm using the overallDate file instead of overallName, because overallName does not include stillbirths. 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
#' Inputs
overallDate <- readRDS("./gen/augment/overallDate-recode.rds")
################################################################################

# Denominator
# E - matched pregnancies only
dat <- subset(overallDate, type == "VS_Match")

nrow(dat) # 1902
nrow(subset(dat, cstatus_agesp_dss== "Stillbirth")) # 90
nrow(subset(dat, cstatus_agesp_sur== "Stillbirth")) # 92

# Misclassification -------------------------------------------------------

stb_in_fph <- dat %>%
  filter(cstatus_agesp_sur== "Stillbirth") %>%
  group_by(cstatus_agesp_dss, cstatus_agesp_sur) %>%
  summarise(n = n())
stb_in_dss <- dat %>%
  filter(cstatus_agesp_dss== "Stillbirth") %>%
  group_by(cstatus_agesp_dss, cstatus_agesp_sur) %>%
  summarise(n = n()) 

res <- stb_in_dss %>%
  bind_rows(stb_in_fph) %>%
  ungroup() %>%
  mutate(cstatus_agesp_dss = factor(cstatus_agesp_dss, levels = c("Stillbirth", "Neonatal", "Postneonatal", "Surviving"))) %>%
  mutate(cstatus_agesp_sur = factor(cstatus_agesp_sur, levels = c("Stillbirth", "Neonatal", "Postneonatal", "Surviving"))) %>%
  complete(cstatus_agesp_dss, cstatus_agesp_sur, fill = list(n = 0)) %>%
  mutate(n = ifelse(!(cstatus_agesp_sur == "Stillbirth" | cstatus_agesp_dss == "Stillbirth") & n == 0, NA, n))

myplot <- res %>%
  ggplot() +
  geom_tile(aes(x= cstatus_agesp_sur, y = cstatus_agesp_dss, fill = n), color = "black") +
  geom_text(aes(x = cstatus_agesp_sur, y = cstatus_agesp_dss, label = n)) +
  scale_fill_gradientn(
    colors = c("white", rev(viridisLite::viridis(100, option = "plasma"))),
    limits = c(0, max(res$n, na.rm = TRUE)),
    oob = scales::squish,
    na.value = "gray"
  ) +
  labs(title = "Agreement in stillbirths", x = "FPH", y = "DSS") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 12)
  )
ggsave("./gen/figures/event-misclass-stb.png", myplot, width = 7, height = 4.5, dpi = 500)







  

# # include misclassification of stb and neonatal/postneonatal deaths
# datsamp <- dat %>%
#   filter(pregout_dss == "Stillbirth" | c223 == "Stillbirth" | # subsetting potential misclassifciations of stb/nnd
#            cstatus_agesp_dss == "Neonatal" | cstatus_agesp_sur == "Neonatal") 
# 
# # exclude other types of misclassification, because...
# # (1) we're not interested in them for this specific part of the analysis
# # (2) we look into them with the overall-dob-matched datsampaset
# datsamp <- datsamp %>% 
#   filter(!(cstatus_agesp_dss == "Neonatal" & cstatus_agesp_sur == "Postneonatal")) %>% # exclude disagreement in neo and pneo
#   filter(!(cstatus_agesp_dss == "Postneonatal" & cstatus_agesp_sur == "Neonatal")) %>% # exclude disagreement in neo and pneo
#   filter(!(cstatus_agesp_dss == "Neonatal" & cstatus_agesp_sur == "1-4")) %>% # exclude disagreement in neo and 1-4y
#   filter(!(cstatus_agesp_dss == "1-4" & cstatus_agesp_sur == "Neonatal")) %>% # exclude disagreement in neo and 1-4y
#   filter(!(cstatus_dss == "Surviving" & cstatus_sur == "Died")) %>% # exclude disagreement in surviving and died
#   filter(!(cstatus_dss == "Died" & cstatus_sur == "Surviving")) %>% # exclude disagreement in surviving and died
#   filter(!(cstatus_dss == "Surviving" & cstatus_sur == "Stillbirth")) %>% # exclude disagreement in surviving and stb
#   filter(!(cstatus_dss == "Stillbirth" & cstatus_sur == "Surviving"))
# nrow(datsamp) # 288
# 
# # classify types of misclassification of interest
# datsamp <- datsamp %>%
#   mutate(agreement = case_when(
#     pregout_dss == "Stillbirth" & c223 == "Stillbirth" ~ "Both stb",
#     pregout_dss == "Live birth" & cstatus_agesp_dss == "Neonatal" & c223 == "Stillbirth" ~ "DSS nnd, FPH stb",
#     pregout_dss == "Stillbirth" & c223 == "Live birth" & cstatus_agesp_sur == "Neonatal"~ "DSS stb, FPH nnd",
#     pregout_dss == "Live birth" & cstatus_agesp_dss == "Postneonatal" & c223 == "Stillbirth" ~ "DSS pnnd, FPH stb",
#     pregout_dss == "Stillbirth" & c223 == "Live birth" & cstatus_agesp_sur == "Postneonatal"~ "DSS stb, FPH pnnd",
#     cstatus_agesp_dss == "Neonatal" & cstatus_agesp_sur == "Neonatal" ~ "Both nnd",
#     TRUE ~ NA
#   )) 
# 
# # and make sure there are no uncategorized cases
# datsamp %>%
#   filter(is.na(agreement)) %>%
#   select(match_n,
#            rid_m,
#            rid_c, pregout_dss, cstatus_dss, cstatus_agesp_dss,  dob_c_dss, dod_c_dss, # child-level information from dss
#            uid_c_sur, c215, c220, c223, aady_sur, cstatus_sur, cstatus_agesp_sur# child-level information from survey
#     ) %>% 
#   nrow() # 0
#   
# nrow(dat) # 1902 matched
# nrow(subset(dat, pregout_dss == "Stillbirth")) # 90
# nrow(subset(dat, cstatus_agesp_dss == "Neonatal")) # 203
# nrow(subset(dat, c223 == "Stillbirth")) # 92
# nrow(subset(dat, cstatus_agesp_sur == "Neonatal")) # 203
# 
# datsamp %>%
#   group_by(agreement) %>%
#   summarise(n = n())





# TO BE MOVED TO overallDOB file

# Disagreement in AOD -----------------------------------------------------

# Level: event
# Denominator: matches
# Exclude: cases have disagreement on stb/nnd or died/surviving. 
# Exclude: 10+
# Notes:
## sur has reported aod
## hdss has aod derived from reported dod

# by age
dat %>%
  filter(type == "VS_Match") %>%
  filter(!(cstatus_dss %in% c("Miscarriage", "Abortion", "Stillbirth", "Surviving") |
             cstatus_sur %in% c("Miscarriage", "Abortion", "Stillbirth", "Surviving"))) %>%
  filter(!(cstrata_ac == "10+")) %>%
  mutate(aadd_dif = aadd_dss - aadd_sur) %>%
  mutate(cstrata_a = factor(cstrata_a, levels = c("Neonatal", "Postneonatal", "1-4 year", "5-9 year"))) %>%
  mutate(cstrata_c = factor(cstrata_c, levels = c( "Other", "Birth asphyxia", "RI and congenital", "Drowning", "5-9 year"))) %>%
  filter(!(aadd_dif < -500 & cstrata_a == "Neonatal")) %>% # remove aberration for now
  ggplot() +
  geom_boxplot(aes(x = cstrata_a, y = aadd_dif)) +
  geom_hline(yintercept = 0, color = "red") +
  coord_flip() +
  facet_wrap(~cstrata_a, scales = "free", ncol = 1)

# by age/cause
dat %>%
  filter(type == "VS_Match") %>%
  filter(!(cstatus_dss %in% c("Miscarriage", "Abortion", "Stillbirth", "Surviving") |
           cstatus_sur %in% c("Miscarriage", "Abortion", "Stillbirth", "Surviving"))) %>%
  filter(!(cstrata_ac == "10+")) %>%
  mutate(aadd_dif = aadd_dss - aadd_sur) %>%
  mutate(cstrata_a = factor(cstrata_a, levels = c("Neonatal", "Postneonatal", "1-4 year", "5-9 year"))) %>%
  mutate(cstrata_c = factor(cstrata_c, levels = c( "Other", "Birth asphyxia", "RI and congenital", "Drowning", "5-9 year"))) %>%
  filter(!(aadd_dif < -500 & cstrata_a == "Neonatal")) %>% # remove aberration for now
  ggplot() +
  geom_boxplot(aes(x = cstrata_c, y = aadd_dif)) +
  geom_hline(yintercept = 0, color = "red") +
  coord_flip() +
  facet_wrap(~cstrata_a, scales = "free", ncol = 1)





# to be deleted -----------------------------------------------------------


# can probably delete this
mypal2 <- rev(plasma(n = 3)[1:2])

# matches of live births and stillbirths by DOB year
dat %>%
  filter(c223 %in% c("Live birth", "Stillbirth")) %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  mutate(doby_c_comb = year(dob_c_comb),
         type = factor(type, levels = c("VS_NoMatch", "VS_Match"),
                       labels = c("No match", "Match"))) %>%
  group_by(c223, doby_c_comb, type) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(c223, doby_c_comb) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  pivot_longer(cols = c(N, Proportion)) %>%
  ggplot() +
  geom_bar(aes(x = doby_c_comb, y = value, fill = type),
           stat = "identity", width = 0.9) +
  scale_fill_manual(values = mypal2) +
  labs(x = "DOB year",title = "Matching of events in FPH with HDSS by DOB year") +
  facet_grid(name ~ c223, scales = "free") +
  theme_bw(base_size = 7) + 
  theme(
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
