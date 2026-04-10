################################################################################
#' @description analyse all records regardless of match status, aggegrate level, denominators A and B
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
dat <- overall %>%
  mutate(denomA = ifelse(dob_m_dss == doi_m_dss, 1, 0),
         denomB = ifelse(as.numeric(doi_m_dss - dob_m_dss)/365.25 <= 15, 1, 0),
         denomC = ifelse(
           # mother's in-migration is more than 10 years ago, and
           as.numeric(as.Date(max(unique(overall$int_date_sur))) - doi_m_dss)/365.25 >= 10 & 
             # dss dob is within past 10 years or
             (!is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - dob_c_dss)/365.25 <= 10 | 
                # unmatched validation study dob is within past 10 years
                (is.na(dob_c_dss) & as.numeric(as.Date(max(unique(overall$int_date_sur))) - c220)/365.25 <= 10)), 
           1, 0))
table(dat$denomA, useNA = "always") # 746 pregnancies of lifelong residents
table(dat$denomB, useNA = "always") # 1012 pregnancies of 15+ residents
table(dat$denomC, useNA = "always") # 1300 mother-pregnancies in past 10 years
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

# Define function for agreement in total number of events -----------------

dat %>%
  filter(c223 %in% c("Live birth")) %>%
  group_by(rid_m) %>%
  summarise(n = n()) %>%
  group_by(n) %>%
  summarise(n = n())

fn_aggAgreement <- function(dat, outcome, denom, plot = TRUE){
  
  denom_col <- paste0("denom", denom) 
  
  if(outcome %in% c("Live birth", "Stillbirth")){
    n_sur <- dat %>%
      filter(type %in% c("VS_Match", "VS_NoMatch", "VS_STB") &
               c223 == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
    n_dss <- dat %>%
      filter(type %in% c("VS_Match", "HDSS_NoMatch", "HDSS_STB") &
               pregout_dss == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
  }

  if(outcome == "Abortion"){
    n_sur <- dat %>%
      filter(type %in% c("VS_AB") &
               c223 == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
    n_dss <- dat %>%
      filter(type %in% c("HDSS_AB") &
               pregout_dss == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
  }
  if(outcome == "Miscarriage"){
    n_sur <- dat %>%
      filter(type %in% c("VS_MSC") &
               c223 == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n()) 
    n_dss <- dat %>%
      filter(type %in% c("HDSS_MSC") &
               pregout_dss == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
  }
  if(outcome %in% c("Neonatal", "Postneonatal", "1-4", "5-9")){
    n_sur <- dat %>%
      filter(type %in% c("VS_Match", "VS_NoMatch") &
               cstatus_agesp_sur == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
    n_dss <- dat %>%
      filter(type %in% c("VS_Match", "HDSS_NoMatch") &
               cstatus_agesp_dss == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
  }
  if(outcome %in% c("Surviving", "Died")){
    n_sur <- dat %>%
      filter(type %in% c("VS_Match", "VS_NoMatch") &
               cstatus_sur == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
    n_dss <- dat %>%
      filter(type %in% c("VS_Match", "HDSS_NoMatch") &
               cstatus_dss == outcome &
               .data[[denom_col]] == 1) %>%
      group_by(rid_m) %>%
      summarise(n = n())
  }
  
  
  # individuals who do not have even in either source
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

# Plot agreement ----------------------------------------------------------

plotDat1 <- fn_aggAgreement(dat, outcome = "Live birth", denom = "A")
#plotDat2 <- fn_aggAgreement(dat, outcome = "Live birth", denom = "B")
plotDat2 <- fn_aggAgreement(dat, outcome = "Live birth", denom = "C")
plotDat <- rbind(plotDat1, plotDat2)
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
myplot
ggsave("./gen/figures/agg-agree-LB.png", myplot, width = 4, height = 2.5, dpi = 500) # formerly 8 and 4

plotDat <- fn_aggAgreement(dat, outcome = "Stillbirth")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
  scale_x_continuous(breaks = seq(min(plotDat$n_sur), max(plotDat$n_sur), by = 1)) +
  scale_y_continuous(breaks = seq(min(plotDat$n_dss), max(plotDat$n_dss), by = 1)) +
  labs(title = unique(plotDat$outcome), x = "FPH", y = "DSS") +
  coord_cartesian(ylim = c(-0.3, 3.3), xlim = c(-0.3, 3.3)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "black", linewidth = 0.3),
    text = element_text(size = 10)
  )
ggsave("./gen/figures/agg-agree-SB.png", myplot, width = 4, height = 2.5, dpi = 500)


plotDat <- fn_aggAgreement(dat, outcome = "Miscarriage")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-MSC.png", myplot, width = 4, height = 2.5, dpi = 500)


plotDat <- fn_aggAgreement(dat, outcome = "Abortion")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-AB.png", myplot, width = 4, height = 2.5, dpi = 500)


plotDat <- fn_aggAgreement(dat, outcome = "Neonatal")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-Neonatal.png", myplot, width = 4, height = 2.5, dpi = 500)

plotDat <- fn_aggAgreement(dat, outcome = "Postneonatal")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-Postneonatal.png", myplot, width = 4, height = 2.5, dpi = 500)

plotDat <- fn_aggAgreement(dat, outcome = "1-4")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-Child.png", myplot, width = 4, height = 2.5, dpi = 500)

plotDat <- fn_aggAgreement(dat, outcome = "5-9")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-OlderChild.png", myplot, width = 4, height = 2.5, dpi = 500)


plotDat <- fn_aggAgreement(dat, outcome = "Surviving")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-surviving.png", myplot, width = 4, height = 2.5, dpi = 500)

plotDat <- fn_aggAgreement(dat, outcome = "Died")
myplot <- plotDat %>%
  mutate(denom = ifelse(denom == "A", "A: Lifelong residents", "B: Residents since age 15y")) %>%
  ggplot() +
  geom_tile(aes(x= n_sur, y = n_dss, fill = n), color = "black") +
  geom_text(aes(x= n_sur, y = n_dss, label = n)) +
  facet_wrap(~denom) +
  scale_fill_viridis_c(direction = -1, option = "plasma") +
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
ggsave("./gen/figures/agg-agree-died.png", myplot, width = 4, height = 2.5, dpi = 500)


# Table -------------------------------------------------------------------

fn_agreementTable <- function(dat, denom, outcome){
  
  denom_col <- paste0("denom", denom) 

  dat <- dat %>% filter(.data[[denom_col]] == 1)

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

datTab <- fn_aggAgreement(dat, outcome = "Live birth", plot = FALSE)
datLB <- fn_agreementTable(datTab, denom = "A", outcome = "Live birth")
datTab <- fn_aggAgreement(dat, outcome = "Stillbirth", plot = FALSE)
datSB <- fn_agreementTable(datTab, denom = "A", outcome = "Stillbirth")
datTab <- fn_aggAgreement(dat, outcome = "Miscarriage", plot = FALSE)
datMSC <- fn_agreementTable(datTab, denom = "A", outcome = "Miscarriage")
datTab <- fn_aggAgreement(dat, outcome = "Abortion", plot = FALSE)
datAB <- fn_agreementTable(datTab, denom = "A", outcome = "Abortion")

datTab <- fn_aggAgreement(dat, outcome = "Neonatal", plot = FALSE)
datNeo <- fn_agreementTable(datTab, denom = "A", outcome = "Neonatal death")
datTab <- fn_aggAgreement(dat, outcome = "Postneonatal", plot = FALSE)
datPneo <- fn_agreementTable(datTab, denom = "A", outcome = "Postneonatal death")
datTab <- fn_aggAgreement(dat, outcome = "1-4", plot = FALSE)
datChild <- fn_agreementTable(datTab, denom = "A", outcome = "1-4y death")
datTab <- fn_aggAgreement(dat, outcome = "5-9", plot = FALSE)
datOlderchild <- fn_agreementTable(datTab, denom = "A", outcome = "5-9y death")

datTab <- fn_aggAgreement(dat, outcome = "Surviving", plot = FALSE)
datSurv <- fn_agreementTable(datTab, denom = "A", outcome = "Surviving children")
datTab <- fn_aggAgreement(dat, outcome = "Died", plot = FALSE)
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
    outcome == "Surviving children" ~ 5,
    outcome == "Non-surviving children" ~ 6,
    outcome == "Neonatal death" ~ 7,
    outcome == "Postneonatal death" ~ 8,
    outcome == "1-4y death" ~ 9,
    outcome == "5-9y death" ~ 10
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
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:7, part = "all") %>%
  align(align = "left", j = 1, part = "all")

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-agg-agree.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

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
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:5, part = "all") %>%
  align(align = "left", j = 1, part = "all")

doc <- read_docx() %>%
  body_add_par("Table 2", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-agg-agree-lb-chi.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")
