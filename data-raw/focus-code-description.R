# Michael D Garrett.  20170814
#
# Dissertation / Creates code tables
#
# Save coding definitions as R tbls to be used in .Rmd files, etc.
#
# Modified: 20180105

## Environment assumes these packages have been installed --------------

library(dplyr)

pars <- wkf_config()

## Create focus code table --------------------------

# Codes
fc <- pars$focus_codes
# Names
fn <- c(
  "Reading",
  "Doing",
  "Representing",
  "Discussing",
  "Other on-task",
  "Off-task")
# Meanings
fm <- c(
  "A focus on reading instructions, getting clarification from the facilitator, or discussing the instructions.",
  "A focus on manipulating the materials related to the phenomenon under study, or manipulating other materials to facilitate working with the phenomenon, or measuring some aspect of the phenomenon.",
  "A focus on group members' capturing on paper their thinking about the phenomenon.",
  "A focus on talk among subjects about what happened, why it happened, or explanations of what happened without directly manipulating the materials and without a focus on representing group members' thinking.",
  "A focus on working on the task at hand (describing and explaining the phenomenon) not covered by the other on-task codes. For example, organizing or getting new flipbook pages.",
  "A focus on something not to do with the task at hand (describing and explaining the phenomenon)")

# This df will be used to create nice tables in vignettes
focus_code_description <- cbind(code = fc, name = fn, meaning = fm)
# give it a version
attributes(focus_code_description)$version <- Sys.Date()

devtools::use_data(focus_code_description, overwrite = TRUE)
