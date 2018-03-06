# Michael D Garrett. Dissertation / Res phase work focus codings
#
# Create the study session, group, and participant data tables from the raw
# Excel files
#

## Load packages -------------------------

library(dplyr)     # dplyr
library(readxl)    # read_excel
library(purrr)     # map

study_tables <- file.path(".", "data-raw", "study_tables.xlsx")

## Session table -------------------------
session_tbl <-
  read_excel(study_tables, sheet = "session_table") %>%
  mutate(
    round = factor(round, ordered = TRUE),
    gid = factor(gid, ordered = TRUE)
  )

glimpse(session_tbl)

## Group table -------------------------
group_tbl <-
  read_excel(study_tables, sheet = "group_table") %>%
  mutate(
    gid = factor(gid, ordered = TRUE)
  ) %>%
  rename(group_name = name)

glimpse(group_tbl)

## Participant table -------------------------
participant_tbl <-
  read_excel(study_tables, sheet = "participant_table") %>%
  mutate(
    pid = factor(pid, ordered = TRUE),
    gid = factor(gid, ordered = TRUE)
  )

glimpse(participant_tbl)

#  all member-specific data into one table
member_tbl <- full_join(group_tbl, participant_tbl, by = "gid")

## Code tables -------------------------
code_tbl_list <-
  read_excel(study_tables, sheet = "code_table") %>%
  mutate(
    type = factor(type, ordered = TRUE),
    order = as.integer(order)
  ) %>%
  # separate code descriptions by type
  split(.$type) %>%
  # will need as factors for plotting. Put in semantic order, not alphabetical
  map(~ mutate(.x, code = factor(code, code[order], ordered = TRUE)))

glimpse(code_tbl_list)

## Coder table -------------------------
coder_tbl <-
  read_excel(study_tables, sheet = "coder_table") %>%
  mutate(
    cid = factor(cid, ordered = TRUE)
  )

## Save as list of all tables as package data -------------------------
study <- c(list(sessions = session_tbl,
                members = member_tbl,
                coders = coder_tbl),
           code_tbl_list)
devtools::use_data(study, overwrite = TRUE)
