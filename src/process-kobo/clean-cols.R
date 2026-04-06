################################################################################
#' @description Clean and rename columns in rh and parent files
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

parent_vars <- data.frame(
  "name" = names(parent),
  "label" = sapply(parent, function(x) attr(x, "label"))  %>% as.character(),
  "labelled" = sapply(parent, is.labelled) )
row.names(parent_vars) <- NULL

rh_vars <- data.frame(
  "name" = names(rh),
  "label" = sapply(rh, function(x) attr(x, "label"))  %>% as.character(),
  "labelled" = sapply(rh, is.labelled) )
row.names(rh_vars) <- NULL

all_vars <- data.frame(
  "name" = names(all),
  "label" = sapply(all, function(x) attr(x, "label"))  %>% as.character(),
  "labelled" = sapply(all, is.labelled) )
row.names(all_vars) <- NULL

# all_vars contains all variables in otehrs plus _index1 and _merge
parent_vars$name[!(parent_vars$name %in% all_vars$name)]
rh_vars$name[!(rh_vars$name %in% all_vars$name)]
all_vars$name[!(all_vars$name %in% c(parent_vars$name, rh_vars$name))]

# order
all_vars <- all_vars[order(all_vars$name),]

# Create new name that will order things correctly
all_vars$newname <- all_vars$name
all_vars$newname[all_vars$name == "asset_score"] <- "a7_a"
all_vars$newname[all_vars$name == "asset_quintile"] <- "a7_b"
all_vars$newname[all_vars$name == "start"] <- "_start"
all_vars$newname[all_vars$name == "end"] <- "_start_end"
all_vars$newname[all_vars$name == "Location"] <- "_Location"
all_vars$newname[all_vars$name == "HH_size"] <- "a1_a"
all_vars$newname[all_vars$name == "self_hscore"] <- "b112_a2"
all_vars$newname[all_vars$name == "serial1"] <- "_serial1"
all_vars$newname[all_vars$name == "serial2"] <- "_serial2"
all_vars$newname[all_vars$name == "serial3"] <- "_serial3"
all_vars$newname[all_vars$name == "s3"] <- "_serial3_recode"
all_vars$newname[all_vars$name == "metainstanceID"] <- "_metainstanceID"
all_vars$newname[all_vars$name == "aa"] <- "_start_a"  
all_vars$newname[all_vars$name == "age_alive"] <- "c225_a"  
all_vars$newname[all_vars$name == "cal_216"] <- "c216_b"  
all_vars$newname[all_vars$name == "cal_223"] <- "c223_a" 
all_vars$newname[all_vars$name == "preg_num"] <- "c212_a" 
all_vars$newname[all_vars$name == "age"] <- "b111_a" 
all_vars$newname[all_vars$name == "resp_age"] <- "b111_b" 
all_vars$newname[all_vars$name == "b111_a"] <- "b111_c" 
all_vars$newname[all_vars$name == "x0"] <- "_x0" 
all_vars$newname[all_vars$name == "x0_1"] <- "_x0_1" 
all_vars$newname[all_vars$name == "x0_2"] <- "_x0_2" 
all_vars$newname[all_vars$name == "x0_3"] <- "_x0_3" 
all_vars$newname[all_vars$name == "x1"] <- "_x1" 
all_vars$newname[all_vars$name == "x1_a"] <- "_x1_a" 
all_vars$newname[all_vars$name == "_uuid"] <- "_id_uuid" 
all_vars$newname[all_vars$name == "_parent_table_name"] <- "_index_parent_table_name"
all_vars$newname[all_vars$name == "_parent_index"] <- "_index_parent_index"

# ordering d and w
all_vars$firstletter <- substr(all_vars$newname, 1, 1)
all_vars$firstnum <- str_extract(all_vars$newname, "(?<=d|w)\\d+")
all_vars$firstnum <- as.numeric(all_vars$firstnum)
all_vars <- all_vars[order(all_vars$firstletter, all_vars$firstnum, all_vars$newname),]

# adjust newname
all_vars$newname <- tolower(all_vars$newname)
all_vars$newname <- gsub("^_+", "", all_vars$newname)
all_vars$newname <- gsub("_+$", "", all_vars$newname)
all_vars$newname <- trimws(all_vars$newname)

# set order
all_vars$col_order <- 1:nrow(all_vars)

# View all in correct order
#View(all[,all_vars$name])

# number of rows/ids
length(unique(all$`_id`))
length(unique(all$`_index`))
length(unique(all$`_index1`))
nrow(all)

# These are the matching variables.
#parent$`_parent_index`
#rh$`_index`

# order parent and rh in the same way as all
order_parent <- all_vars$name[all_vars$name %in% names(parent)]
order_rh <- all_vars$name[all_vars$name %in% names(rh)]
df_parent <- parent[,order_parent]
df_rh <- rh[,order_rh]

# Create a named vector for renaming
rename_map <- setNames(all_vars$newname, all_vars$name)

# Keep only existing columns to rename
rename_map_parent <- rename_map[names(rename_map) %in% names(parent)]
df_parent <- df_parent %>%
  rename_with(~ rename_map_parent[.x], .cols = names(rename_map_parent))

# rename rh
rename_map_rh <- rename_map[names(rename_map) %in% names(rh)]
df_rh <- df_rh %>%
  rename_with(~ rename_map_rh[.x], .cols = names(rename_map_rh))

# delete unnecessary
df_rh <- df_rh %>%
  select(-c(id, id_uuid, notes, tags, submission_time)) %>%
  rename(index_parent = index_parent_index)

# delete unnecessary and
# rename index for parents
df_parent <- df_parent %>%
  select(-c(index_parent_index, index_parent_table_name)) %>%
  rename(index_parent = index)

# Missing values
df_parent <- df_parent %>%
  mutate(across(where(is.character), ~ ifelse(. == "Na", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "NA", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "N/A", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "N/ A", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "Na.", NA, .)))
df_rh <- df_rh %>%
  mutate(across(where(is.character), ~ ifelse(. == "Na", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "NA", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "N/A", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "N/ A", NA, .))) %>%
  mutate(across(where(is.character), ~ ifelse(. == "Na.", NA, .)))

# Replace empty strings and blank spaces
df_parent <- df_parent %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .)))
df_rh <- df_rh %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .)))


# Plot missings

df_parent %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = fct_reorder(name, value), y = value)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size = 8))
# lots of missing
df_parent %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything()) %>%
  filter(value > .25) %>%
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size = 8))

df_rh %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = fct_reorder(name, value), y = value)) +
  geom_col() +
  coord_flip()

# Save --------------------------------------------------------------------

write.csv(df_rh, "./gen/data-prep/output/rh-clean.csv", row.names = FALSE)
write.csv(df_parent, "./gen/data-prep/output/parent-clean.csv", row.names = FALSE)
