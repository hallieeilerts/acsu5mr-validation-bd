################################################
# ACSU5MR
# Validation Study
# Bangladesh
################################################


# Exploration of data from Kobo toolbox -----------------------------------

#source("./src/process-kobo/explore-kobo.R", local = new.env())
#source("./src/process-kobo/clean-cols.R", local = new.env())

# Basic exploration -------------------------------------------------------

# now working with processed spreadsheets shared by icddr,b team

source("./src/basic-exploration.R", local = new.env())
#source("./src/prep-mother-migration.R", local = new.env())
source("./src/prep-survey.R", local = new.env())
source("./src/prep-hdss.R", local = new.env())
source("./src/prep-overall-date.R", local = new.env())
source("./src/augment-overall-date.R", local = new.env())
source("./src/sample-qualitative.R", local = new.env())

source("./src/check-agreement.R", local = new.env())

source("./src/paa-figures.R", local = new.env())

# start with aggregate analysis of number of events reported
## same total number of live births?
## same total number of stillbirths
# then look at event-level matches
# file matched on dob
## misclassification of stillbirth/live birth
## age transferrence
# file matched on name
## date displacement


