################################################################################
#' @description
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(ggplot2)
library(tidyverse)
#' Inputs
dat_filename <- list.files("./gen/process-kobo/")
dat_filename <- dat_filename[grepl("parent", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
parent <- read_dta(paste0("./gen/process-kobo/", dat_filename))
dat_filename <- list.files("./gen/process-kobo/")
dat_filename <- dat_filename[grepl("rh", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
rh <- read_dta(paste0("./gen/process-kobo/", dat_filename))
dat_filename <- list.files("./gen/process-kobo/")
dat_filename <- dat_filename[grepl("all", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
all <- read_dta(paste0("./gen/process-kobo/", dat_filename))
################################################################################

names(parent)
# a00-a713 are questions we didn't include in FPH instrument. seem to cover what house is made of, water source, etc.
# w1-w30
# a000, b00
# b110-b130 are the background questions we did include
# c201-c212, c230 are reproductive section outside of FPH
# c232-c244 are reproductive section questions that we did not include about pregnancy, LMP, menstruation care (?). Not in 2022 BDHS.
# d1-d9 are interviewer observations

x <- '"c1000"              
[113] "preg_num"            "c222_A"              "c222_B"              "c2000"               "c244"                "c244_a"              "c244_a1"            
[120] "c244_a2"             "c244_a3"             "c244_a4"             "c244_a5"             "c244_a_1"            "c244_a_2"            "c244_a_3"           
[127] "c244_a_4"            "c244_a_5"            "c3000" '
recreateVector <- function(X, numeric = TRUE, quiet = FALSE){
  X <- scan(what = character(), text = X, quiet = quiet)
  X <- sub("^\\s*\\[\\d+\\]", "", X)
  X <- X[X != ""]
  if(numeric) X <- as.numeric(X)
  X
}
cols <- recreateVector(x, numeric = FALSE)
View(parent[,cols])


# Identifiers
unique(parent$`_index`)
unique(rh$`_parent_index`)



# parent ------------------------------------------------------------------

# Data.frame with column names, column labels and an indicator for whether the column is labelled:
parent_vars <- data.frame(
  "name" = names(parent),
  "label" = sapply(parent, function(x) attr(x, "label"))  %>% as.character(),
  "labelled" = sapply(parent, is.labelled) )

# Extract the labels as a factor for each column in the dataframe.
# Then give these columns unique new names.
df_parent <- haven::as_factor(parent, levels="labels")

# Interviews
df_parent %>%
  ggplot() +
  geom_bar(aes(x=x1), stat = "count") +
  labs(x = "interview result after 1st attempt")

# First visit date
df_parent %>%
  mutate(x0_1 = as.Date(x0_1, format = "YYYY-mm-dd")) %>% 
  mutate(x0_1y = year(x0_1), x0_1m = month(x0_1), x0_1d = day(x0_1)) %>%
  mutate(x0_1y = ifelse(x0_1y %in% c("2023","2024") & x0_1m != "12", "2025", x0_1y)) %>%
  mutate(int_date = as.Date(paste0(x0_1y, "-",x0_1m,  "-",x0_1d))) %>% select(x0_1y, x0_1m, x0_1d, int_date) %>% 
  filter(!is.na(int_date)) %>%
  ggplot() +
  geom_histogram(aes(x=int_date))

# Birthday
#df_parent$b110_a, df_parent$b110_b

# Respondents age
df_parent %>%
  filter(b111 < 500) %>%
  ggplot() +
  geom_histogram(aes(x=b111)) +
  labs(x = "age")

# Marital status
df_parent %>%
  ggplot() +
  geom_bar(aes(x=b111_a), stat = "count") +
  labs(x = "marital status")

# Health condition
df_parent %>%
  ggplot() +
  geom_bar(aes(x=b112), stat = "count") +
  labs(x = "respondent self-rated health")
df_parent %>%
  ggplot() +
  geom_histogram(aes(x=b112_a)) +
  labs(x = "respondent health score")

# Education 
df_parent %>%
  ggplot() +
  geom_bar(aes(x=b113_a), stat = "count") +
  labs(x = "respondent education")


# Interviewer observations 
df_parent %>%
  ggplot() +
  geom_bar(aes(x=d1), stat = "count") +
  labs(x = "interview location")
df_parent %>%
  ggplot(aes(x=d2)) +
  geom_bar(stat = "count") +
  geom_text(aes(label=after_stat(count)), vjust=-.5, stat='count') +
  labs(x = "interview interruption")
df_parent %>%
  ggplot() +
  geom_bar(aes(x=d3), stat = "count") +
  labs(x = "interview interruption time")
df_parent %>%
  ggplot() +
  geom_bar(aes(x=d4), stat = "count") +
  labs(x = "interviewer familiarity with respondent")
df_parent %>%
  ggplot() +
  geom_bar(aes(x=d5), stat = "count") +
  labs(x = "interviewer familiarity with other hhd member")
df_parent %>%
  ggplot() +
  geom_bar(aes(x=d7), stat = "count") +
  labs(x = "respondent cooperation")
df_parent %>%
  ggplot() +
  geom_bar(aes(x=d8), stat = "count") +
  labs(x = "anyone else around")

df_parent %>%
  ggplot() +
  geom_bar(aes(x=x1), stat = "count")

df_parent %>%
  ggplot() +
  geom_bar(aes(x=b112), stat = "count")


# rh ----------------------------------------------------------------------

# Data.frame with column names, column labels and an indicator for whether the column is labelled:
rh_vars <- data.frame(
  "name" = names(rh),
  "label" = sapply(rh, function(x) attr(x, "label"))  %>% as.character(),
  "labelled" = sapply(rh, is.labelled) )

# c221_aaa - Pregnancy ending time (text)
unique(rh$c221_aaa)

# c228_cc  - Age at death (text)
unique(rh$c228_cc)



