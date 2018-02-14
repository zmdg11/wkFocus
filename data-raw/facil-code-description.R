# Michael D Garrett.
#
# Dissertation / Creates code tables
#
# Save coding definitions as R tbls to be used in .Rmd files, etc.
#
# Modified: 20180211

## Environment assumes these packages have been installed --------------

library(dplyr)

pars <- wkf_config()

## Create focus code table --------------------------

# Codes
fc <- pars$facil_codes
# Names
fn <- c(
  "Facilitated",
  "Independent")
# Meanings
fm <- c(
  "Group members are working with the science adept. During facilitated work, the adept is acting as a group member. The adept's behavior is also used to determine the group focus.",
  "Group members are working independently of the adept. During independent work, the adept is not acting as a group member. The adept's behavior is not used to determine the group focus.")

# This df will be used to create nice tables in vignettes
facil_code_description <- cbind(code = fc, name = fn, meaning = fm)
# give it a version
attributes(facil_code_description)$version <- Sys.Date()

devtools::use_data(facil_code_description, overwrite = TRUE)
