################################################################################
#' @description Descriptive table for denominators
#' and make sure numbers in tables all add up
#' @return 
################################################################################


num1 <- read.csv("./gen/audit/num1.csv")
num2 <- read.csv("./gen/audit/num2.csv")

## omissions and additions table
# all-women total
subset(num1, subsample == "all-women")$nDth_dss
# lifelong resident match plus omission
subset(num1, subsample == "lifelong-residents")$nDth_dss
155 + 11
# recent pregnancies match plus omission
subset(num1, subsample == "recent-pregnancies")$nDth_dss
269 + 27

## age at death misclassification
num2$nDth_dss

## period misclassifcation
# total should match the agreement column in the age at death misclass table
  
## characteristics table
# all-women n match
num2$nDth_dss
# all-women n omission
subset(num1, subsample == "all-women")$nDth_dss - num2$nDth_dss
# recent-pregnancies n match + n addition should equal
subset(num1, subsample == "recent-pregnancies")$nDth_sur

## regression
# omission
subset(num1, subsample == "all-women")$nDth_dss # minus deaths >15 years ago and to children 10+ years
# addition
subset(num1, subsample == "recent-pregnancies")$nDth_sur

mytab <- data.frame(
  row_names = c("Level", "Description", "N women", "N lb in DSS", "N dth in DSS", "N dth in FPH"),
  all_women = c("Woman", "desc", subset(num1, subsample == "all-women")$nWomen,
                subset(num1, subsample == "all-women")$nLb_dss,
                subset(num1, subsample == "all-women")$nDth_dss,
                subset(num1, subsample == "all-women")$nDth_sur),
  lifelong_res = c("Woman", "desc", subset(num1, subsample == "lifelong-residents")$nWomen,
                   subset(num1, subsample == "lifelong-residents")$nLb_dss,
                   subset(num1, subsample == "lifelong-residents")$nDth_dss,
                   subset(num1, subsample == "lifelong-residents")$nDth_sur),
  rec_preg = c("Woman+pregnancy", "desc", subset(num1, subsample == "recent-pregnancies")$nWomen,
               subset(num1, subsample == "recent-pregnancies")$nLb_dss,
               subset(num1, subsample == "recent-pregnancies")$nDth_dss,
               subset(num1, subsample == "recent-pregnancies")$nDth_sur),
  matched_lb = c("Live birth", "desc", num2$nWomen, num2$nLb_dss, num2$nDth_dss, num2$nDth_sur))
mytab


