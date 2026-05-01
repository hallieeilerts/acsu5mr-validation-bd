################################################################################
#' @description check misclassification of lb and stb
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(flextable)
library(officer)
#' Inputs
overall <- readRDS("./gen/augment/overallDate-recode.rds") 
################################################################################


# live birth or stillbirth in fph or hdss
# For sample table
datSamp <- subset(overall, type == "VS_Match")
length(unique(datSamp$rid_m)) # 820

# n live births in hdss
nrow(subset(datSamp, pregout_dss == "Live birth")) # 1812

# misclassification
datTab <- datSamp %>%
  mutate(combos = paste0(pregout_dss, " ", c223)) %>%
  group_by(combos, pregout_dss, c223) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(per = round(n/sum(n)*100,2)) %>%
  as.data.frame() %>%
  select(-combos) %>%
  bind_rows(
    summarise(., 
              pregout_dss = "Total",
              c223 = "",
              n = sum(n),
              per = 100)
  )


ft <- flextable(datTab) %>%
  set_header_labels(values = c("HDSS", "FPH", "N", "%")) %>%
  set_caption(caption = "Misclassification of live births and stillbirths in HDSS and FPH in match-LB/SB subsample.") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 3:ncol(datTab), part = "all") %>%
  align(align = "left", j = 1:2, part = "all")
ft

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/figures", "table-misclassification.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

# ages at death of misclassified
datSamp %>%
  filter(pregout_dss == "Live birth" & c223 == "Stillbirth") %>%
  select(cstatus_dss, aadd_dss)
datSamp %>%
  filter(pregout_dss == "Stillbirth" & c223 == "Live birth") %>%
  select(cstatus_sur, aadd_sur, aadm_sur, aady_sur)
