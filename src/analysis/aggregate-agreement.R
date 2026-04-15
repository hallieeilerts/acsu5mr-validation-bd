################################################################################
#' @description analyse all records regardless of match status, aggegrate level
#' Assess:
#' Aggregate agreement in total number of lb, abo, msc, stb, surv children, died children, agegrp of deaths
#' I think it shouldn't matter to use the overallDate instead of overallDOB 
#' since we are not taking match status into account and each file has been augmented to contain all records from survey and HDSS.
#' Though I'm not totally sure, so let's just use overallDOB
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(viridis)
library(officer)
library(flextable)
#' Inputs
overall <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################

# Assign denominator: 
# A - lifelong residents
# B - reproductive age residents (15+)
# C - Mother+pregnancies: 10 yr residents
# All - pregnancies or all women regardless of migration status. FPH should capture more.
# Since this is aggregate agreement, DSS and FPH events are included conditional on their source-specific DOB.
# Hence the need for denomC_dss and denomC_sur columns.
dat <- overall %>%
  mutate(denomA = ifelse(dob_m_dss == doi_m_dss, 1, 0),
         denomB = ifelse(as.numeric(doi_m_dss - dob_m_dss)/365.25 <= 15, 1, 0),
         denomC_dss = ifelse(
           # mother's in-migration is more than 10 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 10 & 
             # dss dob is within past 10 years
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 10), 
           1, 0),
         denomC_sur = ifelse(
           # mother's in-migration is more than 10 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 10 & 
                # validation study dob is within past 10 years
                (!is.na(c220) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 10), 
           1, 0),
         denomAll = 1) %>%
  mutate(denomC = ifelse(denomC_dss == 1 | denomC_sur == 1, 1, 0))
dat %>%
  select(rid_m, denomA) %>%
  distinct() %>%
  group_by(denomA) %>%
  summarise(n = n()) # 210 lifelong residents
dat %>%
  select(rid_m, denomB) %>%
  distinct() %>%
  group_by(denomB) %>%
  summarise(n = n()) # 280 reproductive age residents
dat %>%
  select(rid_m, denomC) %>%
  distinct() %>%
  group_by(denomC) %>%
  summarise(n = n()) # 497 women with uniterrupted residency in past 10 years

n_womAll <- length(unique(dat$rid_m))
n_womA <- length(unique(subset(dat, denomA == 1)$rid_m))
n_womB <- length(unique(subset(dat, denomB == 1)$rid_m))
n_womC <- length(unique(subset(dat, denomC == 1)$rid_m))

# Define function for agreement in total number of events -----------------

dat %>%
  filter(c223 %in% c("Live birth")) %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  group_by(n) %>%
  summarise(n = n())

fn_aggAgreement <- function(dat, outcome, denom, plot = TRUE){
  
  denom_col <- paste0("denom", denom) 
  
  # women-level denominator
  if(denom %in% c("A", "B", "All")){
    if(outcome %in% c("Live birth", "Stillbirth", "Abortion", "Miscarriage")){
      n_sur <- dat %>%
        filter(c223 == outcome &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(pregout_dss == outcome &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
    if(outcome %in% c("Neonatal", "Postneonatal", "1-4", "5-9")){
      n_sur <- dat %>%
        filter(cstatus_agesp_sur == outcome &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(cstatus_agesp_dss == outcome &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
    if(outcome %in% c("Surviving", "Died")){
      n_sur <- dat %>%
        filter(cstatus_sur == outcome &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(cstatus_dss == outcome &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
    if(outcome %in% c("Pregnancy")){
      n_sur <- dat %>%
        filter(!is.na(cstatus_sur) &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(!is.na(cstatus_dss) &
                 .data[[denom_col]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
  }
  
  # women+pregnancy level denominator
  if(!(denom %in% c("A", "B", "All"))){
    
    denom_col_dss <- paste0(denom_col, "_dss") 
    denom_col_sur <- paste0(denom_col, "_sur") 
    
    if(outcome %in% c("Live birth", "Stillbirth", "Abortion", "Miscarriage")){
      n_sur <- dat %>%
        filter(c223 == outcome &
                 .data[[denom_col_sur]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(pregout_dss == outcome &
                 .data[[denom_col_dss]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
    if(outcome %in% c("Neonatal", "Postneonatal", "1-4", "5-9")){
      n_sur <- dat %>%
        filter(cstatus_agesp_sur == outcome &
                 .data[[denom_col_sur]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(cstatus_agesp_dss == outcome &
                 .data[[denom_col_dss]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
    if(outcome %in% c("Surviving", "Died")){
      n_sur <- dat %>%
        filter(cstatus_sur == outcome &
                 .data[[denom_col_sur]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(cstatus_dss == outcome &
                 .data[[denom_col_dss]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
    if(outcome %in% c("Pregnancy")){
      n_sur <- dat %>%
        filter(!is.na(cstatus_sur) &
                 .data[[denom_col_sur]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
      n_dss <- dat %>%
        filter(!is.na(cstatus_dss) &
                 .data[[denom_col_dss]] == 1) %>%
        group_by(rid_m) %>%
        summarise(n = n())
    }
  }
  

  
  # individuals who do not have event in either source during the observation period
  n_zero <- dat %>% 
    filter(!(rid_m %in% c(n_sur$rid_m, n_dss$rid_m)) &
             .data[[denom_col]] == 1) %>%
    select(rid_m) %>%
    distinct() %>%
    mutate(n_sur = 0,
           n_dss = 0)
  
  
  nAll <- n_sur %>%
    dplyr::full_join(n_dss,by = c("rid_m"),suffix = c("_sur", "_dss")) %>%
    bind_rows(n_zero)
  
  # # are there any that had no reported live births in one source and not other
  # nrow(subset(nLB, is.na(nLB_sur) | is.na(nLB_dss))) # 7
  # v_check <- subset(nLB, is.na(nLB_sur) | is.na(nLB_dss))$rid_m
  # dat %>%
  #   filter(rid_m %in% v_check) %>%
  #   select(match_n, type,
  #          rid_m, int_date_sur, # mother id for both survey and dss, date of interview
  #          mstrata_a,  # mother-level strata drawn from dss for sampling, but only available in survey file?
  #          rid_c, pregout_dss, cstatus_dss, dob_c_dss, dod_c_dss, # child-level information from dss
  #          uid_c_sur, c215, c220, c223, aady_sur, cstatus_sur# child-level information from survey
  #   ) %>%
  #   head()
  # fill in NA's with zero
  nAll[is.na(nAll)] <- 0
  # reshape long
  nAlllong <- nAll %>% 
    ungroup() %>%
    mutate(denom = denom) %>%
    select(rid_m, n_sur, n_dss, denom)
  
  # summarise for plot
  plotDat <- nAlllong %>%
    group_by(denom, n_sur, n_dss) %>%
    summarise(n = n()) %>%
    mutate(outcome = outcome)
  
  if(plot){
    return(plotDat)
  }else{
    return(nAll)
  }

}



# Women with disagreement -------------------------------------------------

# investigate individual women with disagreement in n events between dss and sur

df_n_sur <- dat %>%
  filter(c223 == "Live birth" &
           denomA == 1) %>%
  group_by(rid_m) %>%
  summarise(n_sur = n())
df_n_dss <- dat %>%
  filter(pregout_dss == "Live birth" &
           denomA == 1) %>%
  group_by(rid_m) %>%
  summarise(n_dss = n())
df_ind_agree <- df_n_sur %>% 
  left_join(df_n_dss)

# woman with 4 live births in survey and 2 in dss
subset(df_ind_agree, n_sur == 4 & n_dss == 2)
overall %>%
  filter(rid_m == "3D93016009") %>% 
  select(match_n, rid_m, cid_m, 
         dob_m_dss, doi_m_dss, pregout_dss, name_c_dss, sex_c_dss, dob_c_dss, dod_c_dss, 
         c215, c216, c218, c219, c220, c223)

# Figure: mother-level agreement (tiles) ----------------------------------------------------------

plotDat1 <- fn_aggAgreement(dat, outcome = "Live birth", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Live birth", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot1 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = unique(plotDat$outcome), x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.1, 7.1), xlim = c(-0.1, 7.1)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
myplot1
#ggsave("./gen/figures/agg-agree-LB.png", myplot1, width = 4, height = 2.5, dpi = 500) # formerly 8 and 4

plotDat1 <- fn_aggAgreement(dat, outcome = "Stillbirth", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Stillbirth", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot2 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = unique(plotDat$outcome), x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 2.3), xlim = c(-0.3, 2.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-SB.png", myplot2, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "Miscarriage", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Miscarriage", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot3 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = unique(plotDat$outcome), x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 2.3), xlim = c(-0.3, 2.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-MSC.png", myplot3, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "Abortion", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Abortion", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot4 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = unique(plotDat$outcome), x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 2.3), xlim = c(-0.3, 2.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-AB.png", myplot4, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "Neonatal", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Neonatal", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot5 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = "Neonatal death", x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 3.3), xlim = c(-0.3, 3.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-Neonatal.png", myplot5, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "Postneonatal", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Postneonatal", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot6 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = "Postneonatal death", x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 2.3), xlim = c(-0.3, 2.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-Postneonatal.png", myplot6, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "1-4", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "1-4", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot7 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = "1-4y death", x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 2.3), xlim = c(-0.3, 2.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-Child.png", myplot7, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "5-9", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "5-9", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot8 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = "5-9y death", x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 1.3), xlim = c(-0.3, 1.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-OlderChild.png", myplot8, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "Surviving", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Surviving", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot9 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = "Surviving children", x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.1, 6.1), xlim = c(-0.1, 6.1)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-surviving.png", myplot9, width = 4, height = 2.5, dpi = 500)

plotDat1 <- fn_aggAgreement(dat, outcome = "Died", denom = "A")
plotDat2 <- fn_aggAgreement(dat, outcome = "Died", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot10 <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years")) %>%
  mutate(denom = factor(denom, levels = c("Mothers: lifelong residents", "Mother+pregnancies: prev. 10 years"))) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma", limits = c(0, 484), name = "N mothers") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = "Non-surviving children", x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.2, 4.2), xlim = c(-0.2, 4.2)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
#ggsave("./gen/figures/agg-agree-died.png", myplot10, width = 4, height = 2.5, dpi = 500)


plots <- list(
  myplot1, myplot2, myplot3, myplot4, myplot5,
  myplot6, myplot7, myplot8#, 
  #myplot9, myplot10
)
combined_plot <- ggarrange(
  plotlist = plots,
  ncol = 2, nrow = 4,
  common.legend = TRUE,
  legend = "bottom"
)
combined_plot
ggsave(
  "./gen/figures/agg-agree-tiles.png",
  plot = combined_plot,
  width = 10,
  height = 12,
  dpi = 300
)

# Figure: total number of events --------------------------------------------------


# define function to count events in each source
fn_totaleventsTable <- function(dat, denom, outcome){
  
  dat <- dat %>%
    summarise(n_sur = sum(n_sur),
              n_dss = sum(n_dss)) %>%
    mutate(outcome = outcome,
           denom = denom)
  
  return(dat)
  
} 

# Lifelong residents
# preg outcomes
datTab <- fn_aggAgreement(dat, outcome = "Pregnancy", denom = "A", plot = FALSE)
datPr <- fn_totaleventsTable(datTab, denom = "A", outcome = "Pregnancy")
datTab <- fn_aggAgreement(dat, outcome = "Live birth", denom = "A", plot = FALSE)
datLB <- fn_totaleventsTable(datTab, denom = "A", outcome = "Live birth")
datTab <- fn_aggAgreement(dat, outcome = "Stillbirth", denom = "A", plot = FALSE)
datSB <- fn_totaleventsTable(datTab, denom = "A", outcome = "Stillbirth")
datTab <- fn_aggAgreement(dat, outcome = "Miscarriage", denom = "A", plot = FALSE)
datMSC <- fn_totaleventsTable(datTab, denom = "A", outcome = "Miscarriage")
datTab <- fn_aggAgreement(dat, outcome = "Abortion", denom = "A", plot = FALSE)
datAB <- fn_totaleventsTable(datTab, denom = "A", outcome = "Abortion")
# deaths
datTab <- fn_aggAgreement(dat, outcome = "Neonatal", denom = "A", plot = FALSE)
datNeo <- fn_totaleventsTable(datTab, denom = "A", outcome = "Neonatal death")
datTab <- fn_aggAgreement(dat, outcome = "Postneonatal", denom = "A", plot = FALSE)
datPneo <- fn_totaleventsTable(datTab, denom = "A", outcome = "Postneonatal death")
datTab <- fn_aggAgreement(dat, outcome = "1-4", denom = "A", plot = FALSE)
datChild <- fn_totaleventsTable(datTab, denom = "A", outcome = "1-4y death")
datTab <- fn_aggAgreement(dat, outcome = "5-9", denom = "A", plot = FALSE)
datOlderchild <- fn_totaleventsTable(datTab, denom = "A", outcome = "5-9y death")
# died/surviving
datTab <- fn_aggAgreement(dat, outcome = "Surviving", denom = "A", plot = FALSE)
datSurv <- fn_totaleventsTable(datTab, denom = "A", outcome = "Surviving children")
datTab <- fn_aggAgreement(dat, outcome = "Died", denom = "A", plot = FALSE)
datDied <- fn_totaleventsTable(datTab, denom = "A", outcome = "Non-surviving children")
# combine
datTabAllA <- rbind(datPr, datLB, datSB, datMSC, datAB,
                   datNeo, datPneo, datChild, datOlderchild,
                   datSurv, datDied)

# Pregnancies in past 10 years (only mothers with uninterrupted residency in that time)
# preg outcomes
datTab <- fn_aggAgreement(dat, outcome = "Pregnancy", denom = "C", plot = FALSE)
datPr <- fn_totaleventsTable(datTab, denom = "C", outcome = "Pregnancy")
datTab <- fn_aggAgreement(dat, outcome = "Live birth", denom = "C", plot = FALSE)
datLB <- fn_totaleventsTable(datTab, denom = "C", outcome = "Live birth")
datTab <- fn_aggAgreement(dat, outcome = "Stillbirth", denom = "C", plot = FALSE)
datSB <- fn_totaleventsTable(datTab, denom = "C", outcome = "Stillbirth")
datTab <- fn_aggAgreement(dat, outcome = "Miscarriage", denom = "C", plot = FALSE)
datMSC <- fn_totaleventsTable(datTab, denom = "C", outcome = "Miscarriage")
datTab <- fn_aggAgreement(dat, outcome = "Abortion", denom = "C", plot = FALSE)
datAB <- fn_totaleventsTable(datTab, denom = "C", outcome = "Abortion")
# deaths
datTab <- fn_aggAgreement(dat, outcome = "Neonatal", denom = "C", plot = FALSE)
datNeo <- fn_totaleventsTable(datTab, denom = "C", outcome = "Neonatal death")
datTab <- fn_aggAgreement(dat, outcome = "Postneonatal", denom = "C", plot = FALSE)
datPneo <- fn_totaleventsTable(datTab, denom = "C", outcome = "Postneonatal death")
datTab <- fn_aggAgreement(dat, outcome = "1-4", denom = "C", plot = FALSE)
datChild <- fn_totaleventsTable(datTab, denom = "C", outcome = "1-4y death")
datTab <- fn_aggAgreement(dat, outcome = "5-9", denom = "C", plot = FALSE)
datOlderchild <- fn_totaleventsTable(datTab, denom = "C", outcome = "5-9y death")
# died/surviving
datTab <- fn_aggAgreement(dat, outcome = "Surviving", denom = "C", plot = FALSE)
datSurv <- fn_totaleventsTable(datTab, denom = "C", outcome = "Surviving children")
datTab <- fn_aggAgreement(dat, outcome = "Died", denom = "C", plot = FALSE)
datDied <- fn_totaleventsTable(datTab, denom = "C", outcome = "Non-surviving children")
# combine
datTabAllC <- rbind(datPr, datLB, datSB, datMSC, datAB,
                    datNeo, datPneo, datChild, datOlderchild,
                    datSurv, datDied)

# All mothers regardless of residency
# preg outcomes
datTab <- fn_aggAgreement(dat, outcome = "Pregnancy", denom = "All", plot = FALSE)
datPr <- fn_totaleventsTable(datTab, denom = "All", outcome = "Pregnancy")
datTab <- fn_aggAgreement(dat, outcome = "Live birth", denom = "All", plot = FALSE)
datLB <- fn_totaleventsTable(datTab, denom = "All", outcome = "Live birth")
datTab <- fn_aggAgreement(dat, outcome = "Stillbirth", denom = "All", plot = FALSE)
datSB <- fn_totaleventsTable(datTab, denom = "All", outcome = "Stillbirth")
datTab <- fn_aggAgreement(dat, outcome = "Miscarriage", denom = "All", plot = FALSE)
datMSC <- fn_totaleventsTable(datTab, denom = "All", outcome = "Miscarriage")
datTab <- fn_aggAgreement(dat, outcome = "Abortion", denom = "All", plot = FALSE)
datAB <- fn_totaleventsTable(datTab, denom = "All", outcome = "Abortion")
# deaths
datTab <- fn_aggAgreement(dat, outcome = "Neonatal", denom = "All", plot = FALSE)
datNeo <- fn_totaleventsTable(datTab, denom = "All", outcome = "Neonatal death")
datTab <- fn_aggAgreement(dat, outcome = "Postneonatal", denom = "All", plot = FALSE)
datPneo <- fn_totaleventsTable(datTab, denom = "All", outcome = "Postneonatal death")
datTab <- fn_aggAgreement(dat, outcome = "1-4", denom = "All", plot = FALSE)
datChild <- fn_totaleventsTable(datTab, denom = "All", outcome = "1-4y death")
datTab <- fn_aggAgreement(dat, outcome = "5-9", denom = "All", plot = FALSE)
datOlderchild <- fn_totaleventsTable(datTab, denom = "All", outcome = "5-9y death")
# died/surviving
datTab <- fn_aggAgreement(dat, outcome = "Surviving", denom = "All", plot = FALSE)
datSurv <- fn_totaleventsTable(datTab, denom = "All", outcome = "Surviving children")
datTab <- fn_aggAgreement(dat, outcome = "Died", denom = "All", plot = FALSE)
datDied <- fn_totaleventsTable(datTab, denom = "All", outcome = "Non-surviving children")
# combine
datTabAllAll <- rbind(datPr, datLB, datSB, datMSC, datAB,
                    datNeo, datPneo, datChild, datOlderchild,
                    datSurv, datDied)

datTabAll <- rbind(datTabAllA, datTabAllC, datTabAllAll)


myplot <- datTabAll %>%
  filter(denom %in% c("A", "C")) %>%
  mutate(denom = case_when(
    denom == "A" ~ paste0("Pregnancies of lifelong residents (n women = ",n_womA, ")"), #"Mothers: lifelong residents",
    denom == "C" ~ paste0("Pregnancies in prev. 10 years of resident women (n women = ", n_womC, ")"), #"Mother+pregnancies: prev. 10 years",
    TRUE ~ denom
  )) %>%
  mutate(denom = factor(denom, levels = c( paste0("Pregnancies of lifelong residents (n women = ",n_womA, ")"), # "Mothers: lifelong residents"
                                           paste0("Pregnancies in prev. 10 years of resident women (n women = ", n_womC, ")")))) %>% # "Mother+pregnancies: prev. 10 years
  pivot_longer(cols = c(n_sur, n_dss), names_to = "n") %>%
  mutate(n = ifelse(n == "n_dss", "DSS", "FPH")) %>%
  mutate(n = factor(n, levels = c("FPH", "DSS"))) %>%
  mutate(outcome = factor(outcome, levels = rev(c("Pregnancy",
                                                  "Live birth",
                                                  "Stillbirth","Miscarriage","Abortion","Neonatal death", "Postneonatal death",
                                                  "1-4y death","5-9y death","Surviving children", "Non-surviving children")))) %>%
  ggplot() +
  geom_bar(aes(x = outcome, y = value, fill = n), 
           stat = "identity", position = "dodge") +
  geom_text(aes(x = outcome, y = value, label = value, group = n),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  scale_fill_manual(values = scales::viridis_pal(option = "plasma")(4)[2:3], name = "",
                    guide = guide_legend(reverse = TRUE)) +
  labs(y = "N events", x = "") +
  facet_wrap(~denom, labeller = label_wrap_gen(40)) +
  coord_flip(
    ylim = c(0, 1200)
  )
myplot
ggsave("./gen/figures/total-events-bysource.png", myplot, width = 8, height = 4, dpi = 500)

myplot <- datTabAll %>%
  filter(denom %in% c("A", "C", "All")) %>%
  mutate(denom = case_when(
    denom == "A" ~ paste0("Pregnancies of lifelong residents (n women = ",n_womA, ")"), #"Mothers: lifelong residents",
    denom == "C" ~ paste0("Pregnancies in prev. 10 years of resident women (n women = ", n_womC, ")"), #"Mother+pregnancies: prev. 10 years",
    denom == "All" ~ paste0("All pregnancies (n women = ", n_womAll, ")"), #"All",
    TRUE ~ denom
  )) %>%
  mutate(denom = factor(denom, levels = c(paste0("All pregnancies (n women = ", n_womAll, ")"),
                                          paste0("Pregnancies of lifelong residents (n women = ",n_womA, ")"), # "Mothers: lifelong residents"
                                          paste0("Pregnancies in prev. 10 years of resident women (n women = ", n_womC, ")")))) %>% # "Mother+pregnancies: prev. 10 years
  pivot_longer(cols = c(n_sur, n_dss), names_to = "n") %>%
  mutate(n = ifelse(n == "n_dss", "DSS", "FPH")) %>%
  mutate(n = factor(n, levels = c("FPH", "DSS"))) %>%
  mutate(outcome = factor(outcome, levels = rev(c("Pregnancy","Live birth",
       "Stillbirth","Miscarriage","Abortion","Neonatal death", "Postneonatal death",
       "1-4y death","5-9y death","Surviving children", "Non-surviving children")))) %>%
  ggplot() +
  geom_bar(aes(x = outcome, y = value, fill = n), 
           stat = "identity", position = "dodge") +
  geom_text(aes(x = outcome, y = value, label = value, group = n),
    position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  scale_fill_manual(values = scales::viridis_pal(option = "plasma")(4)[2:3], name = "",
                    guide = guide_legend(reverse = TRUE)) +
  labs(y = "N events", x = "") +
  facet_wrap(~denom, labeller = label_wrap_gen(40)) +
  coord_flip(
    ylim = c(0, 2900)
    )
myplot
ggsave("./gen/figures/total-events-bysource-all.png", myplot, width = 10, height = 4, dpi = 500)

# Table: mother-level agreement by event -----------------------------------------------

# define function to create agreement in total number of events
fn_agreementTable <- function(dat, denom, outcome){
  
  dat <- dat %>%
    mutate(error = case_when(
      n_sur == n_dss ~ "Agree",
      n_sur > n_dss ~ "Addition",
      n_sur < n_dss ~ "Omission"
    )) %>%
    group_by(error) %>%
    summarise(n = n()) %>%
    mutate(total = sum(n)) %>%
    mutate(per = n/total*100) %>%
    mutate(outcome = outcome,
           denom = denom)
  
  return(dat)
  
}

datTab <- fn_aggAgreement(dat, outcome = "Live birth", denom = "A", plot = FALSE)
datLB <- fn_agreementTable(datTab, denom = "A", outcome = "Live birth")
datTab <- fn_aggAgreement(dat, outcome = "Stillbirth", denom = "A", plot = FALSE)
datSB <- fn_agreementTable(datTab, denom = "A", outcome = "Stillbirth")
datTab <- fn_aggAgreement(dat, outcome = "Miscarriage",  denom = "A", plot = FALSE)
datMSC <- fn_agreementTable(datTab, denom = "A", outcome = "Miscarriage")
datTab <- fn_aggAgreement(dat, outcome = "Abortion",  denom = "A", plot = FALSE)
datAB <- fn_agreementTable(datTab, denom = "A", outcome = "Abortion")

datTab <- fn_aggAgreement(dat, outcome = "Neonatal",  denom = "A", plot = FALSE)
datNeo <- fn_agreementTable(datTab, denom = "A", outcome = "Neonatal death")
datTab <- fn_aggAgreement(dat, outcome = "Postneonatal",  denom = "A", plot = FALSE)
datPneo <- fn_agreementTable(datTab, denom = "A", outcome = "Postneonatal death")
datTab <- fn_aggAgreement(dat, outcome = "1-4", denom = "A", plot = FALSE)
datChild <- fn_agreementTable(datTab, denom = "A", outcome = "1-4y death")
datTab <- fn_aggAgreement(dat, outcome = "5-9", denom = "A", plot = FALSE)
datOlderchild <- fn_agreementTable(datTab, denom = "A", outcome = "5-9y death")

datTab <- fn_aggAgreement(dat, outcome = "Surviving", denom = "A", plot = FALSE)
datSurv <- fn_agreementTable(datTab, denom = "A", outcome = "Surviving children")
datTab <- fn_aggAgreement(dat, outcome = "Died", denom = "A", plot = FALSE)
datDied <- fn_agreementTable(datTab, denom = "A", outcome = "Non-surviving children")

datTabAll <- rbind(datLB, datSB, datMSC, datAB,
                   datNeo, datPneo, datChild, datOlderchild,
                   datSurv, datDied)
unique(datTabAll$total) # 210 for denominator A
datTabAll <- datTabAll %>%
  mutate(per = sprintf("%.2f", round(per, 2))) %>%
  pivot_wider(id_cols = outcome, values_from = c(n, per), names_from = error) %>%
  select(outcome, n_Agree, per_Agree, n_Omission, per_Omission, n_Addition, per_Addition) %>%
  mutate(rank = case_when(
    outcome == "Live birth" ~ 1,
    outcome == "Stillbirth" ~ 2,
    outcome == "Miscarriage" ~ 3,
    outcome == "Abortion" ~ 4,
    outcome == "Neonatal death" ~ 7,
    outcome == "Postneonatal death" ~ 8,
    outcome == "1-4y death" ~ 9,
    outcome == "5-9y death" ~ 10,
    outcome == "Surviving children" ~ 11,
    outcome == "Non-surviving children" ~ 12,
  )) %>%
  arrange(rank)
datTabAll$n_Addition[is.na(datTabAll$n_Addition)] <- 0
datTabAll$per_Addition[is.na(datTabAll$per_Addition)] <- "0.00"
datTabAll$rank <- NULL

# create Word table
ft <- flextable(datTabAll) %>%
  set_header_labels(values = c("Outcome", "N", "%", "N", "%", "N", "%")) %>%
  add_header_row(values = c(" ","Agree", "Omission", "Addition"), colwidths = c(1, 2, 2, 2)) %>%
  set_caption(caption = "Mother-level agreement in total number of events reported in each source for lifelong residents (n = 210)") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:7, part = "all") %>%
  align(align = "left", j = 1, part = "all")
ft

# doc <- read_docx() %>%
#   body_add_par("Table 1", style = "heading 1") %>%
#   body_add_flextable(ft)
# 
# output_path <- here::here("gen/figures", "table-agg-agree.docx")
# print(doc, target = output_path)
# cat("Saved to:", output_path, "\n")

# Note
# I think not worth it to investigate the mother-level characteristics here...
# the sample size is so small for omission and addition
# We could combine them I suppose. but seems a bit silly
# and probably more interesting to look at the individual event

# Investigate mother-level characteristics --------------------------------

df_mothers <- fn_aggAgreement(dat, outcome = "Live birth", plot = FALSE)
df_mothers <- df_mothers %>%
  mutate(error = case_when(
    n_sur == n_dss ~ "Agree",
    n_sur > n_dss ~ "Addition",
    n_sur < n_dss ~ "Omission"
  )) %>%
  left_join(dat %>% select(rid_m, 
                           mage_int, magecat_int, 
                           paritymax_dss, paritymaxcat_dss, 
                           meducat_sur,
                           observer_sur,
                           hhsize_sur, hhsizecat_sur,
                           hhassets_sur) %>% distinct()
            )
df_char_cont <- df_mothers %>%
  filter(denomA == 1) %>%
  pivot_longer(
    cols = c(mage_int, paritymax_dss, hhsize_sur),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(error, variable) %>%
  summarise(avg = mean(value))
df_char_cat <- df_mothers %>%
  filter(denomA == 1) %>%
  pivot_longer(
    cols = c(magecat_int, paritymaxcat_dss, meducat_sur, observer_sur, hhsizecat_sur, hhassets_sur),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(error, variable, value) %>%
  summarise(n = n()) %>%
  group_by(error, variable) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  select(-total)
df_chi <- df_mothers %>%
  filter(denomA == 1) %>%
  pivot_longer(
    cols = c(magecat_int, paritymaxcat_dss, meducat_sur, observer_sur, hhsizecat_sur, hhassets_sur),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    test = list(chisq.test(table(error, value))),
    p_value = test[[1]]$p.value,
    .groups = "drop"
  )


datTabChi <- df_char_cat %>% 
  select(-n) %>%
  pivot_wider(names_from = error, values_from = per) %>%
  left_join(df_chi) %>%
  select(-test) %>%
  mutate(Agree= sprintf("%.2f", round(Agree, 2)),
         Omission = sprintf("%.2f", round(Omission, 2)),
    p_value = sprintf("%.2f", round(p_value, 2))) %>%
  mutate(Omission = ifelse(Omission == "NA", "", Omission))


# create Word table
ft <- flextable(datTabChi) %>%
  merge_v(j = ~ variable + p_value) %>%
  set_caption(caption = "Mother-level characteristics and chi-squared for agreement between HDSS and FPH in number of live births for lifelong residents (n = 210)") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:5, part = "all") %>%
  align(align = "left", j = 1, part = "all")

doc <- read_docx() %>%
  body_add_par("Table 2", style = "heading 1") %>%
  body_add_flextable(ft)

# output_path <- here::here("gen/figures", "table-agg-agree-lb-chi.docx")
# print(doc, target = output_path)
# cat("Saved to:", output_path, "\n")
