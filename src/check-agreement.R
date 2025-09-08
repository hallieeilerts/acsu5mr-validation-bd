################################################################################
#' @description check agreement between surveys and hdss
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
#' Inputs
dat <- readRDS("./gen/overall-aug.rds")
################################################################################

# N/per of matched/no matched validation study records by mother strata
unique(dat$type)
nrow(subset(dat, type %in% c("VS_Match", "VS_NoMatch"))) # 2353
dat %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  mutate(sample2 = factor(sample2, levels = c("2024all", "Live birth", "Stillbirth", 
    "Neonatal (birth asphyxia)", "Neonatal (other)","Postneonatal (RI+con)", 
    "Postneonatal (other)", "1-4 year (drowning)", "1-4 year (other)", "5-9 year"))) %>%
  mutate(type = factor(type, levels = c("VS_NoMatch", "VS_Match"), labels = c("No match", "Match"))) %>%
  group_by(type, sample2) %>%
  summarise(N = n()) %>%
  group_by(sample2) %>%
  mutate(total = sum(N),
         Proportion = N/total) %>%
  pivot_longer(cols = c(N, Proportion)) %>%
  ggplot() +
  geom_bar(aes(x = sample2, y = value, fill = type), stat = "identity") +
  coord_flip() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Matching of survey live births and stillbirths with HDSS by strata",x = "Strata", y = "") +
  theme_bw() +
  theme(legend.title = element_blank())
# Shouldn't look at it this way
# Because mother strata only applies to one of the records of her children

# Matching by date of birth in survey
# Matching by pregnancy outcome in survey
dat %>%
  filter(type %in% c("VS_Match", "VS_NoMatch")) %>%
  mutate(doby_c_sur = year(dob_c_sur)) %>%
  mutate(type = factor(type, levels = c("VS_NoMatch", "VS_Match"), labels = c("No match", "Match"))) %>%
  group_by(doby_c_sur, type) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_bar(aes(x = doby_c_sur, y = N, fill = type), stat = "identity") +
  labs(title = "Matching of survey LB/SB with HDSS by DOB year (from survey)") +
  theme_bw() +
  theme(legend.title = element_blank())




df_match <- subset(dat, match_score == 1)
df_non <- subset(dat, is.na(match_score))

df_match %>%
  group_by(va) %>%
  summarise(n = n())
df_match %>%
  group_by(sample) %>%
  summarise(n = n())
df_match %>%
  group_by(sample2) %>%
  summarise(n = n())

# For those who matched to HDSS, compare DOB
# compare age at death
df_match %>%
  mutate(dob_dif = dob_c_sur - dob_c_dss) %>%
  group_by(sample2, dob_dif) %>%
  summarise(n = n()) %>%
  group_by(sample2) %>%
  mutate(total = sum(n),
         per = n/total) %>%
  ggplot() +
  geom_bar(aes(x=dob_dif, y = per, fill = sample2), stat = "identity") +
  facet_wrap(~sample2)
df_match %>%
  mutate(dob_dif = abs(as.numeric(dob_c_sur - dob_c_dss))) %>%
  mutate(sample2 = factor(sample2, levels = c("2024all", "Live birth", "Stillbirth", "Neonatal (birth asphyxia)", "Neonatal (other)",
                             "Postneonatal (RI+con)", "Postneonatal (other)", "1-4 year (drowning)", "1-4 year (other)", 
                              "5-9 year"))) %>%
  group_by(sample2) %>%
  summarise(avg_dif = mean(dob_dif)) %>% 
  ggplot() +
  geom_bar(aes(x = sample2, y = avg_dif, fill = sample2), stat = "identity") +
  labs(x = "", y = "Average difference in DOB (days)") +
  coord_flip() +
  theme(legend.position = "none")

# For those who matched to HDSS, compare age at death
unique(dat$dod_c_dss)
names(dat)


# For those who didn't match to HDSS
