

read_dataset  <- function(dsname) {

  ## Reads an R .rds file defined by dsname
  #
  #  Args: dsname - list holding the information about the dataset name. Only
  #  $fname and $Stamp are used
  #  Value: returns the dataframe stored in the specified file
  #
  #  Details: $fname determines the file name, $Stamp determines which
  #  subdirectory of the project working data directory will be looked at
  #

  pars <- read_config()

  ## Load data -------

  fn <- paste0(dsname$fname, ".rds")
  df <- readRDS(file.path(pars$data_folder, dsname$Stamp, fn))

  return(df)
}

save_dataset <- function(df, dsname) {

  #  Writes a dataframe as an .rds file using the project-standard dataset path.
  #
  #  Args: df - dataframe to be saved, dsname - list holdng the information
  #  about the dataset name. Only $fname and $Stamp are used
  #  Value:
  #

  pars <- read_config()

  ## Save data -------

  fn <- paste0(dsname$fname, ".rds")
  saveRDS(df, file.path(pars$data_folder, dsname$Stamp, fn))
}
