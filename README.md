# acsu5mr-validation-bd

Quantitative analysis of validation study in BD

## Steps to process raw data

1. Download data from kobo toolbox website following instructions in `/documentation` "Data download way"
2. Run STATA script in `src/process-kobo` to assign value labels in English for *Parent_file* and *Sheet*
3. The output files final_rh, final_parent, final_all will be generated in the data folder. Move to gen/process-kobo and add date.

## Processed data

1. First delivery provided by Moin et al. in data/ACSU5MR_FILES 
2. There was a second delivery on 20250903 for the dob-matching
3. There was third delivery on 20250930 for the name and dob-matching

