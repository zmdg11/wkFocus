# Michael D Garrett.  2017-11-18 Dissertation / Res phase work focus codings
#
# Create the focus coding dataset files from the raw Excel files for the res
# phase of the study
#
# Modified: 20180104

## Load packages -------------------------

library(devtools)     # ::use_data
library(dplyr)        # manipulate dataframes
library(readxl)       # read excel files

## Functions  ----------------------

gather_raw_data <- function(fname) {

  ## Returns a list of raw datasets from an excel file (file) in the data-raw folder
  #
  #  Database identifiers are attahced to each dataset as a data frame attribute

  raw_path <- file.path(".", "data-raw", fname)

  # Move all sheets in the raw file into a list of raw data frames
  ds_sheets <- readxl::excel_sheets(raw_path)
  ds_raw_dat <- lapply(ds_sheets, read_excel, path = raw_path)

  # Create dataset descriptors for all data frames using file name and sheet names
  ds_names <- paste(tools::file_path_sans_ext(fname), ds_sheets, sep = "_")
  ds_desc <- lapply(ds_names, wkf_parse_dspath)

  # Turn each descriptor into an attribute of the data frame
  for (i in seq_along(ds_raw_dat)) {
    attr(ds_raw_dat[[i]], "desc") <- ds_desc[[i]]
  }

  return(ds_raw_dat)
}

create_res_cstamp_dat <- function(df) {

  ## Returns a single codestamp dataset from a single raw dataset.

  desc <- attr(df, "desc")
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

  # Set dataset descriptor to indicate a codestamp dataset
  desc <- wkFocus::wkf_build_dsdesc(desc, list(stamp = "cstamp"))
  attr(df, "desc") <- desc

  return(df)
}

## Parameters ----------------------

pars <- wkFocus::wkf_config()  # project-wide parameters

## Read and clean hand-coded data ----------------------

print(paste("Converting", "'focus' codesets in: data-raw folder ..."))

# -------------------------
codeset <- "res1A_focus_hand.xlsx"
print(paste("...", codeset))
ds_raw_dat <- gather_raw_data(codeset)
res1A_focus_hand_cstamp <- lapply(ds_raw_dat, create_res_cstamp_dat)
devtools::use_data(res1A_focus_hand_cstamp, overwrite = TRUE)


# -------------------------
codeset <- "res1C_focus_hand.xlsx"
print(paste("...", codeset))
ds_raw_dat <- gather_raw_data(codeset)
res1C_focus_hand_cstamp <- lapply(ds_raw_dat, create_res_cstamp_dat)
devtools::use_data(res1C_focus_hand_cstamp, overwrite = TRUE)
