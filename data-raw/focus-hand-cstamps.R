# Michael D Garrett.  2017-11-18 Dissertation / Res phase work focus codings
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

## Functions  ----------------------

get_raw_codeset <- function(fname) {

  ## Returns a list of raw wkFocus datasets (name, data pairs) from the raw data
  #  in "file".

  raw_path <- file.path(".", "data-raw", fname)
  cs_desc <- wkFocus::wkf_parse_dspath(fname)

  # Move all sheets in the raw file into a list of raw data frames
  cs_sheets <- readxl::excel_sheets(raw_path)
  cs_raw_dat <- cs_sheets %>%
    purrr::map(~ read_excel(path = raw_path, sheet = .))

  # Datasets need standard wkFocus names

  cs_names <- as.list(paste(cs_desc$name, cs_sheets, "raw", sep = "_"))

  return(purrr::transpose(list(name = cs_names, data = cs_raw_dat)))
}

create_res_cstamps <- function(dataset) {

  ## Returns a single cstamp dataset (name, data pair) from a single raw dataset

  desc <- wkFocus::wkf_parse_dspath(dataset$name)
  df <- dataset$data

  Type  <- desc$type

  # Code from here is specific to research phase data. These session comprises
  # phase (= "res"), round, & group -------------------------

  Round <- wkFocus::wkf_parse_sid(desc$sid)$round
  GID   <- wkFocus::wkf_parse_sid(desc$sid)$gid

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

  desc <- wkFocus::wkf_build_dsdesc(desc, list(stamp = "cstamp"))
  return(list(name = desc$name, data = df))
}

## Read and clean hand-coded data ----------------------

# -------------------------
res1A_focus_hand_cstamp <- "res1A_focus_hand.xlsx" %>%
  get_raw_codeset() %>%
  map(~ create_res_cstamps(.))
devtools::use_data(res1A_focus_hand_cstamp, overwrite = TRUE)

# -------------------------
res1C_focus_hand_cstamp <- "res1C_focus_hand.xlsx" %>%
  get_raw_codeset() %>%
  map(~ create_res_cstamps(.))
devtools::use_data(res1C_focus_hand_cstamp, overwrite = TRUE)
