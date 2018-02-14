# Michael D Garrett. Dissertation / Res phase work focus codings
#
# Create the study session, group, and participant data tables from the raw
# Excel files
#
# Modified: 20180105

## Load packages -------------------------

library(dplyr)     # dplyr
library(readxl)
library(wkFocus)

## Parameters

pars <- wkFocus::wkf_config()

## Get and clean session-level data tables

raw_dat <- file.path(".", "data-raw", "res_tables.xlsx")

## Session table -------------------------
session_tbl <-
  readxl::read_excel(raw_dat, sheet = "session_table", skip = 1,
                     col_names = c("round", "gid", "phenomenon", "duration"),
                     col_types = c("numeric", "text", "text", "numeric"))

#  Make dataset identifiers into factors, especially for plotting
session_tbl <- session_tbl %>%
  mutate(
    gid = factor(gid, levels = pars$gid_levels),
    round = factor(round, levels = pars$round_levels, ordered = TRUE))

devtools::use_data(session_tbl, overwrite = TRUE)
glimpse(session_tbl)

## Group table
group_tbl <-
  readxl::read_excel(raw_dat, sheet = "group_table", skip = 1,
                     col_names = c("gid", "group_name"),
                     col_types = c("text", "text"))

#  Make dataset identifiers into factors, especially for plotting
group_tbl <- group_tbl %>%
  mutate(gid = factor(gid, levels = pars$gid_levels))

devtools::use_data(group_tbl, overwrite = TRUE)
glimpse(group_tbl)

## Participant table
participant_tbl <-
  readxl::read_excel(raw_dat, sheet = "participant_table", skip = 1,
                     col_names = c("study_id", "pid", "gid"),
                     col_types = c("numeric", "numeric", "text"))

#  Make dataset identifiers into factors, especially for plotting
participant_tbl <- participant_tbl %>%
  mutate(pid = factor(pid),
         gid = factor(gid, levels = pars$gid_levels))

devtools::use_data(participant_tbl, overwrite = TRUE)
glimpse(participant_tbl)
