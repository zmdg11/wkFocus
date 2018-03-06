# Michael D Garrett.  2018-01-30 Dissertation / Res phase work focus codings
#
# Create the focus coding dataset files from the raw Excel files for the res
# phase of the study
#
# Modified: 20180109

## Load packages -------------------------

library(devtools)     # use_data
library(dplyr)        # manipulate dataframes
library(readxl)       # read excel files
library(stringr)
library(purrr)        # map
library(wkFocus)

## Parameters

pars <- wkFocus::wkf_config()

## Functions  ----------------------

read_raw_datasets <- function(sid) {

  ## Returns a list of raw wkFocus datasets from raw data in the sheets of
  #  an excel data file.

  ds_src <- file.path(".", "data-raw", paste0(sid, "_focus.xlsx"))

  ## Get specific dataset specs from sheet names
  ds_sheets <- readxl::excel_sheets(ds_src)  # chr[1:n] xxx_vvvvvvvv

  ## Build list of dataset IDs (sid, coder, versrion) from each sheet name
  ds_id <- ds_sheets %>%
    # sheet name is <chr> "coder_version"
    purrr::map( ~ stringr::str_split_fixed(string = ., "_", 2)) %>%
    purrr::map( ~ list(sid = sid, coder = .[1], version = .[2]))

  ## Read codestamp data into a list of dfs
  ds_raw <- ds_sheets %>%
    purrr::map(~ readxl::read_excel(path = ds_src, sheet = .))

  ## Fold all the lists together and flip it so each element
  #  will be a fully specified raw dataset: src, sid, type, data
  return(
    purrr::transpose(list(
      # ... list of same source file
      ds_src = purrr::rep_along(ds_sheets, ds_src),
      # ... list of SIDs for each sheet
      ds_id = ds_id,
      # ... list of same type ("raw")
      ds_type = purrr::rep_along(ds_sheets, "raw"),
      # ... list of raw codestamp datasets
      data = ds_raw )
    )
  )
}

make_cstamp_datasets <- function(ds_raw) {

  ## Returns a fully specified codestamp dataset ($type = "cstamp") from a
  #  raw dataset.

  dat <- ds_raw$data
  sid <- ds_raw$ds_id$sid

  type <- "focus"

  # Code from here is specific to research phase data. These session comprises
  # phase (= "res"), round, & group -------------------------

  this_round <- wkFocus::wkf_parse_sid(sid)$round
  this_gid   <- wkFocus::wkf_parse_sid(sid)$gid
  this_ses <- filter(study$sessions,
               round == this_round & gid == this_gid)

  # In/Out marks are entered in excel as text of form "mm:ss".
  # must be hours:mins:secs:frames for timecode conversion rountine
  # to work. This is to be compatable with .edl formats exported from video
  # editing software.
  dat <- dat %>%
    mutate(
      In  = paste("00", In, "00", sep = ":"),
      Out = paste("00", Out, "00", sep = ":")
    )

  # The video-style timecodes need to be as POSIXct objects with a standard
  # origin for plotting with ggplot2
  dat <- dat %>%
    mutate(
      In  = wkFocus::wkf_convert_tcode(dat$In, this_ses$frame_rate,
                                       this_ses$start_time)$datetime,
      Out = wkFocus::wkf_convert_tcode(dat$Out, this_ses$frame_rate,
                                       this_ses$start_time)$datetime
    )

  # Need standard factors across the project
  dat <- dat %>%
    mutate(
      round = factor(this_round, levels = pars$rounds, ordered = TRUE),
      gid   = factor(this_gid, levels = pars$gids, ordered = TRUE),
      type  = factor(type, levels = pars$code_types, ordered = TRUE),
      bin   = factor(bin, levels = study$facilitation$code, ordered = TRUE),
      code  = factor(code, levels = study$focus$code, ordered = TRUE)
    )

  # Put in order by 'scope'
  dat <- dat %>%
    select(round, gid, type, bin, In, Out, code)

  # Build the cstamp dataset on top of the raw dataset keeping src, SID same
  ds_raw$ds_type <- "cstamp"
  ds_raw$data <- dat

  return(ds_raw)
}

res1A_focus <- read_raw_datasets("res1A") %>%
  map(~ make_cstamp_datasets(.))
devtools::use_data(res1A_focus, overwrite = TRUE)

res1B_focus <- read_raw_datasets("res1B") %>%
  map(~ make_cstamp_datasets(.))
devtools::use_data(res1B_focus, overwrite = TRUE)

res1C_focus <- read_raw_datasets("res1C") %>%
  map(~ make_cstamp_datasets(.))
devtools::use_data(res1C_focus, overwrite = TRUE)

res2A_focus <- read_raw_datasets("res2A") %>%
  map(~ make_cstamp_datasets(.))
devtools::use_data(res2A_focus, overwrite = TRUE)

res2B_focus <- read_raw_datasets("res2B") %>%
  map(~ make_cstamp_datasets(.))
devtools::use_data(res2B_focus, overwrite = TRUE)

res2C_focus <- read_raw_datasets("res2C") %>%
  map(~ make_cstamp_datasets(.))
devtools::use_data(res2C_focus, overwrite = TRUE)

