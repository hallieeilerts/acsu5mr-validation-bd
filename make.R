################################################
# ACSU5MR
# Validation Study
# Bangladesh
################################################


# Exploration of data from Kobo toolbox -----------------------------------

#source("./src/process-kobo/explore-kobo.R", local = new.env())
#source("./src/process-kobo/clean-cols.R", local = new.env())

# Cleaning ----------------------------------------------------------------

# now working with processed spreadsheets shared by icddr,b team

source("./src/clean/basic-exploration.R", local = new.env())
source("./src/clean/prep-survey.R", local = new.env())
source("./src/clean/prep-hdss.R", local = new.env())
source("./src/clean/prep-overallDate.R", local = new.env())
source("./src/clean/prep-overallDob.R", local = new.env())

# Create augmented files that include all records -------------------------

source("./src/augment/augment-overallDate.R", local = new.env())
source("./src/augment/augment-overallDob.R", local = new.env())
source("./src/augment/recode-overallDate.R", local = new.env())
source("./src/augment/recode-overallDob.R", local = new.env())

# Analysis ----------------------------------------------------------------

# aggregate
source("./src/analysis/aggregate-agreement.R", local = new.env())

# descriptive
source("./src/analysis/additions-omissions.R", local = new.env())
source("./src/analysis/transfers.R", local = new.env())
source("./src/analysis/char-additions-omissions.R", local = new.env())

# regression
source("./src/analysis/regression-additions-omissions.R", local = new.env())


# Other -------------------------------------------------------------------

source("./src/sample-qualitative.R", local = new.env())

source("./src/check-agreement.R", local = new.env())

source("./src/paa-figures.R", local = new.env())

# sample size for non-inferiority trial for natural language processing fph
source("./src/sample-size-NLP-nFPH.R", local = new.env())
