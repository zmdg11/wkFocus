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
library(purrr)        # map
library(wkFocus)

## Functions  ----------------------

read_raw_datasets <- function(SID) {

  ## Returns a list of raw wkFocus datasets from raw data in the sheets of
  #  an excel data file.

  ds_src <- file.path(".", "data-raw", paste0(SID, "_focus.xlsx"))

  ## Get specific dataset specs from sheet names
  ds_sheets <- readxl::excel_sheets(ds_src)  # chr[1:n] xxx_vvvvvvvv

  ## Build list of dataset IDs (sid, coder, versrion) from each sheet name
  ds_id <- ds_sheets %>%
    purrr::map( ~ str_split_fixed(string = ., "_", 2)) %>%
    purrr::map( ~ list(sid = SID, coder = .[1], version = .[2]))

  ## Read actual data into a list of dfs
  ds_raw <- ds_sheets %>%
    purrr::map(~ read_excel(path = ds_src, sheet = .))  # list <df, df...>

  ## Fold all the lists together and flip it so each element
  #  will be a fully specified raw dataset
  return(
    purrr::transpose( list(
      ds_src = purrr::rep_along(ds_sheets, ds_src),
      ds_id = ds_id,
      ds_type = purrr::rep_along(ds_sheets, "raw"),
      data = ds_raw )
    )
  )
}

make_cstamp_datasets <- function(ds_raw) {

  ## Returns a fully specified codestamp dataset (a list) from a fully specified
  #  raw dataset.

  df <- ds_raw$data
  sid <- ds_raw$ds_id$sid

  Type <- "cstamp"

  # Code from here is specific to research phase data. These session comprises
  # phase (= "res"), round, & group -------------------------

  Round <- wkFocus::wkf_parse_sid(sid)$round
  GID   <- wkFocus::wkf_parse_sid(sid)$gid

  # In/Out marks must be hours:mins:secs:frames for timecode conversion rountine
  # to work. This is to be compatable with .edl formats exported from video
  # editing software.
  df <- df %>%
    mutate(
      In  = paste("00", In, "00", sep = ":"),
      Out = paste("00", Out, "00", sep = ":"))

  # The video-style timecodes need to be as POSIXct objects with a standard
  # origin for plotting with ggplot2
  df <- df %>%
    mutate(
      In  = wkFocus::wkf_convert_tcode(df$In, pars$fr, pars$t_workshop)$datetime,
      Out = wkFocus::wkf_convert_tcode(df$Out, pars$fr, pars$t_workshop)$datetime)

  # Need standard factors across the project
  df <- df %>%
    mutate(
      Bin   = factor(Bin, levels = pars$facil_codes, ordered = TRUE),
      Code  = factor(Code, levels = pars$focus_codes, ordered = TRUE),
      Round = factor(Round, levels = pars$round_levels, ordered = TRUE),
      GID   = factor(GID, levels = pars$GID_levels, ordered = TRUE),
      Type  = factor(Type, levels = pars$code_types, ordered = TRUE))

  # Build the cstamp dataset
  ds_raw$ds_type <- "cstamp"
  ds_raw$data <- df

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

