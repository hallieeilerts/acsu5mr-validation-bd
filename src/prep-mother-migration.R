################################################################################
#' @description Basic exploration of new files
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
# In the "IN_OUT_2018_2025.dta" file, you will get the migration in and migration out data of the 848 interviewed mothers.
inout <- read_dta("./data/20250903/IN_OUT_2018_2025.dta")
################################################################################

length(unique(inout$rid_m)) # 838
nrow(inout) # 951

as.Date(inout$mot_in_date, format = "%d-%b-%Y")
head(as.Date(inout$mot_out_date, format = "%d-%b-%Y"))

