library(jsonlite)

## Get preload decisions from config file
config <- read_json("./data-raw/config.json", simplifyVector = TRUE)

## Configure: Prebuild decisions ---------------------

# Build the params list
pars <- list(
  ds_types = config$ds_types,
  code_types = config$code_types,
  rounds = config$rounds,
  gids = config$gids,
  workshop_start = as.POSIXct(config$workshop_start, tz = "UTC")
)

# Save as package internal data
devtools::use_data(pars, internal = TRUE, overwrite = TRUE)
