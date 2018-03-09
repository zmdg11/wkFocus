## Utility to build toy data sets and save them data/

# Sample list of two cstamp datasets. Each has a few split-coded sections and
#  both end at the same time. If you rebuild, you may have to fiddle to get
#  this to happen

library(tibble)
library(wkFocus)

pars <- wkf_config()

n = 6

## Setup
# ... code round 1, group A, in session toy (toy1A)
phase <- "toy"
round <- 1
group <- "A"
# ... by coders these coders
coders <- c("mdg", "slg")
# ... this version
code_version <- "20180308"

## Make data
sid <- paste0(phase, round, group)
rounds <- factor(rep(round, n), levels = pars$rounds, ordered = TRUE)
gids   <- factor(rep(group, n), levels = pars$gids, ordered = TRUE)
types  <- factor(rep("focus", n), levels = pars$code_types, ordered = TRUE)

# Data set 1 -------------------------
#  1 facilitated codestamp, 5 independent codestamps
bins   <- factor(c("Ind", "Fac", "Ind", "Ind", "Ind", "Ind"),
                levels = study$facilitation$code, ordered = TRUE)
#  In marks for code intervals in this dataset
Ins <- c("2016-08-16 00:00:00",
         "2016-08-16 00:00:28",
         "2016-08-16 00:01:12",
         "2016-08-16 00:01:19",
         "2016-08-16 00:00:20",
         "2016-08-16 00:00:45")
Ins <- as.POSIXct(Ins)
#  Out marks for code intervals in this dataset
Outs <- c("2016-08-16 00:00:28",
          "2016-08-16 00:01:12",
          "2016-08-16 00:01:19",
          "2016-08-16 00:01:36",
          "2016-08-16 00:00:30",
          "2016-08-16 00:00:58")
Outs <- as.POSIXct(Outs)
#  Codes for this dataset
codes <- factor(c("Rd", "Do", "Rd", "Do", "O", "Rd"),
                levels = study$focus$code, ordered = TRUE)

dat1 <- tibble(round = rounds, gid = gids, type = types, bin = bins,
                 In = Ins, Out = Outs, code = codes)

# Data set 2 -------------------------
#  1 facilitated codestamp, 5 independent codestamps
bins   <- factor(c("Ind", "Fac", "Ind", "Ind", "Ind", "Ind"),
                 levels = study$facilitation$code, ordered = TRUE)
#  In marks for code intervals in this dataset
Ins <- c("2016-08-16 00:00:00",
         "2016-08-16 00:00:20",
         "2016-08-16 00:01:12",
         "2016-08-16 00:01:22",
         "2016-08-16 00:00:15",
         "2016-08-16 00:00:44")
Ins <- as.POSIXct(Ins)
#  Out marks for code intervals in this dataset
Outs <- c("2016-08-16 00:00:20",
          "2016-08-16 00:01:12",
          "2016-08-16 00:01:22",
          "2016-08-16 00:01:36",
          "2016-08-16 00:00:30",
          "2016-08-16 00:00:60")
Outs <- as.POSIXct(Outs)
#  Codes for this dataset
codes <- factor(c("Rd", "Do", "Rd", "Do", "Of", "Rd"),
                levels = study$focus$code, ordered = TRUE)

dat2 <- tibble(round = rounds, gid = gids, type = types, bin = bins,
                   In = Ins, Out = Outs, code = codes)

## Take a look at the two intervals side by side

cbind(dat1[, 5:7], dat2[, 5:7])

## Create the metadata for the two datasets

ds1 <- list(ds_src = "a dummy dataset",
            ds_id = list(sid = sid, coder = coders[1], version = code_version),
            ds_type = "cstamp",
            data = dat1)

ds2 <- list(ds_src = "a dummy dataset",
            ds_id = list(sid = sid, coder = coders[2], version = code_version),
            ds_type = "cstamp",
            data = dat2)

toy1A_focus <- list(ds1, ds2)

devtools::use_data(toy1A_focus, overwrite = TRUE)
