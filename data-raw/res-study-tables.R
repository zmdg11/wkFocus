# Michael D Garrett. Dissertation / Res phase work focus codings
#
# Create the study session, group, and participant data tables from the raw
# Excel files
#
# Modified: 20180105

## Load packages -------------------------

library(dplyr)     # dplyr
library(readxl)

## Parameters -------------------------

pars <- wkf_config()

## Get and clean session-level data tables ---------------

raw_dat <- file.path(".", "data-raw", "res_study_tables.xlsx")

## Session table
session_tbl <-
  readxl::read_excel(raw_dat, sheet = "session_table",
                     col_types = c("numeric", "text", "text", "numeric"))

#  Make dataset identifiers into factors, especially for plotting
session_tbl <- session_tbl %>%
  mutate(
    GID = factor(GID, levels = pars$GID_levels),
    Round = factor(Round, levels = pars$round_levels, ordered = TRUE))

devtools::use_data(session_tbl, overwrite = TRUE)
glimpse(session_tbl)

## Group table
group_tbl <-
  readxl::read_excel(raw_dat, sheet = "group_table",
                     col_types = c("text", "text"))

#  Make dataset identifiers into factors, especially for plotting
group_tbl <- group_tbl %>%
  mutate(GID = factor(GID, levels = pars$GID_levels))

devtools::use_data(group_tbl, overwrite = TRUE)
glimpse(group_tbl)

## Participant table
participant_tbl <-
  readxl::read_excel(raw_dat, sheet = "participant_table",
                     col_types = c("numeric", "numeric", "text"))

#  Make dataset identifiers into factors, especially for plotting
participant_tbl <- participant_tbl %>%
  mutate(PID = factor(PID),
         GID = factor(GID, levels = pars$GID_levels))

devtools::use_data(participant_tbl, overwrite = TRUE)
glimpse(participant_tbl)
