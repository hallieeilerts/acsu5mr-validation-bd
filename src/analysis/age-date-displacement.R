################################################################################
#' @description analyse the overallDob file, event level, denominator E (matched pregnancies)
#' Assess:
#' dob/dod/aod displacement
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(haven)
#' Inputs
dat <- readRDS("./gen/augment/overallDob-recode.rds")
################################################################################
