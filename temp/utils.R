build_toys <- function() {

  ## Utility to build toy data sets and save them to temp/toys

  ## Sample list of two cstamp datasets. Each has a few split-coded sections and
  ## both end at the same time. If you rebuild, you may have to fiddle to get
  ## this to happen

  library(wkFocus)

  pars <- wkf_config()

  n = 6

  ## data 1
  rounds <- factor(rep(1, n), levels = pars$rounds, ordered = TRUE)
  gids   <- factor(rep("A", n), levels = pars$gids, ordered = TRUE)
  types  <- factor(rep("focus", n), levels = pars$code_types, ordered = TRUE)
  bins   <- factor(c("Ind", "Fac", "Ind", "Ind", "Ind", "Ind"),
                  levels = study$facilitation$code, ordered = TRUE)

  Inc <- c("2016-08-16 00:00:00",
           "2016-08-16 00:00:28",
           "2016-08-16 00:01:12",
           "2016-08-16 00:01:19",
           "2016-08-16 00:00:20",
           "2016-08-16 00:00:45")
  Ins <- as.POSIXct(Inc)

  Outc <- c("2016-08-16 00:00:28",
            "2016-08-16 00:01:12",
            "2016-08-16 00:01:19",
            "2016-08-16 00:01:36",
            "2016-08-16 00:00:30",
            "2016-08-16 00:00:58")
  Outs <- as.POSIXct(Outc)

  codes <- factor(c("Rd", "Do", "Rd", "Do", "O", "Rd"),
                  levels = study$focus$code, ordered = TRUE)

  dat1 <- data_frame(round = rounds, gid = gids, type = types, bin = bins,
                   In = Ins, Out = Outs, code = codes)

  ##data2
  rounds <- factor(rep(1, n), levels = pars$rounds, ordered = TRUE)
  gids   <- factor(rep("A", n), levels = pars$gids, ordered = TRUE)
  types  <- factor(rep("focus", n), levels = pars$code_types, ordered = TRUE)
  bins   <- factor(c("Ind", "Fac", "Ind", "Ind", "Ind", "Ind"),
                   levels = study$facilitation$code, ordered = TRUE)

  Inc <- c("2016-08-16 00:00:00",
           "2016-08-16 00:00:20",
           "2016-08-16 00:01:12",
           "2016-08-16 00:01:22",
           "2016-08-16 00:00:15",
           "2016-08-16 00:00:44")
  Ins <- as.POSIXct(Inc)

  Outc <- c("2016-08-16 00:00:20",
            "2016-08-16 00:01:12",
            "2016-08-16 00:01:22",
            "2016-08-16 00:01:36",
            "2016-08-16 00:00:30",
            "2016-08-16 00:00:60")
  Outs <- as.POSIXct(Outc)

  codes <- factor(c("Rd", "Do", "Rd", "Do", "Of", "Rd"),
                  levels = study$focus$code, ordered = TRUE)

  dat2 <- data_frame(round = rounds, gid = gids, type = types, bin = bins,
                     In = Ins, Out = Outs, code = codes)

  #take a look at the two intervals side by side
  bind_cols(dat1[, 5:7], dat2[, 5:7])

  src <- "a dummy dataset"
  id <- list(sid = "toy1", coder = "mdg", version = "20180306")
  type <- "cstamp"

  ds1 <- list(ds_src = "a dummy dataset",
              ds_id = list(sid = "toy1", coder = "mdg", version = "20180308"),
              ds_type = "cstamp",
              data = dat1)

  ds2 <- list(ds_src = "a dummy dataset",
              ds_id = list(sid = "toy2", coder = "slg", version = "20180308"),
              ds_type = "cstamp",
              data = dat2)

  resToy <- list(ds1, ds2)
  save(resToy, file = "./temp/toys/resToy.Rdata")
}

load_toys <- function() {

  ## Utility to load toy data into working memory
  #
  # V 20180213

  # list of two cstamp datasets. t from 00:00 to 01:46. 7 codeintervals
  load("./temp/toys/resToy.Rdata", envir = .GlobalEnv, verbose = T)

}
