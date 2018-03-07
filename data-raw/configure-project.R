library(jsonlite)
library(wkFocus)

## Get preload decisions from config file
config <- read_json("./data-raw/config.json", simplifyVector = TRUE)

## Configure: Prebuild decisions ---------------------

names(config$facil_cols) <- wkFocus::study$facilitation$code
names(config$focus_cols) <- wkFocus::study$focus$code

# Build the params list
pars <- list(
  ds_types = config$ds_types,
  code_types = config$code_types,
  rounds = config$rounds,
  gids = config$gids,
  workshop_start = as.POSIXct(config$workshop_start, tz = "UTC"),
  defaults = list(
    ses_start = as.POSIXct(config$workshop_start, tz = "UTC"),
    facil_cols = config$facil_cols,
    focus_cols = config$focus_cols,
    pl_duration = as.difftime(config$pl_duration, format = "%T")
  )
)

# Save as package internal data
devtools::use_data(pars, internal = TRUE, overwrite = TRUE)
