library(jsonlite)  # read, write, make JSON arrays

## Configure: Make global decisions once for all ---------------------

wd <- normalizePath(getwd())  # assumes wd is project directory

# Build the params list
pars <- list(

  ## Conventions for where stuff is located
  project_folder = wd,
  # Relative path to external images: one level above project
  image_folder = file.path(wd, "images"),

  # Workflow types for data sets
  ds_type = c("agrAB", "cstamp", "raw", "stack", "tsample"),

  ## Conventions for working with time stamps for code-stamps.
  # `t_workshop` is the conventional beginning for turnig timecodes from video
  # to timestamps as POSIXct objects. All times from the beginning of a video
  # clip are turned into real times by offsetting them from this time.
  t_workshop = paste("2016-08-16", "00:00:00 UTC"),
  t_min = paste("2016-08-16", "00:00:00 UTC"),
  t_max = paste("2016-08-16", "00:20:00 UTC"),
  fr = 25,  # Frame rate used in time-codes (fps)

  ## Factor levels for codestamp variables
  code_types = c("facilitation", "focus"),
  facil_codes = c("Ind", "Fac"),
  focus_codes = c("Rd", "Do", "Rp", "Ds", "O", "Of"),
  gid_levels = c("A", "B", "C"),
  round_levels = c("1", "2"),

  ## Aesthetic conventions for color coding
  # Colors for focus codes: 6-class BrBG (color brewer)
  code_col = c("#8c510a","#d8b365","#FF7F00", "#CD1076","#5ab4ac","#01665e"),
  # Colors for facilitation levels: from colorbrewer2, diverging scheme. Close
  # match to colors used in Adobe Premiere: c("orange", "olivedrab3")
  fac_col = c("#f1a340", "olivedrab3"),  #  facilitation type (Ind, Fac)

  # Need these to size images that come from video stills. I set several
  # standard sizes so they appear consistently within reports, vignettes, ...
  # see note in notes folder
  target_ppi = 150,  # print resolution for images
  miniPW = 640,      # pixel width of diss "mini" video clips (nHD)
  miniPH = 360,      # pixel height of...
  codePW = 940,      # pixel width of diss "code" video clips (qHD)
  codePH = 540,      # pixel height of...

  # Need these to make figures consistent across reports, etc. These are set up
  # for "tufte" style .Rmd files
  fulIW = 6.5,       # inches wide for "full size" page illustrations
  medIW = 4.125,     # inches wide for "medium size"...
  smaIW = 2.0,       # inches wide for "small size"...
  lg_gutter = 0.375, # inches gutter btwn med and sma illustrations
  sm_gutter = 0.25   # inches getter btn sma and sma and sma...
)

# Save config as .json for human viewing.
message("Writing pars as ", file.path(getwd(), "config.json"))
write_json(pars, file.path(getwd(), "config.json"), pretty = TRUE)

# .json file will not maintain these vector names, so do them here
names(pars$code_col) <- pars$focus_codes
names(pars$fac_col) <- pars$facil_codes

# Save as package internal data
devtools::use_data(pars, internal = TRUE, overwrite = TRUE)
