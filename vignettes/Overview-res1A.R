## ----setup, message = FALSE, warning = FALSE, echo = FALSE---------------

# Packages -------------------------

# Dataset manipulation
library(dplyr)

# Formating output
library(knitr)
library(pander)

# Chunk options
opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)  
opts_chunk$set(comment = NA)  # do not prepend chunk results with a marker
opts_chunk$set(fig.path = "images/")  # dump a copy of the figures here


## ----session-table-------------------------------------------------------

tmp <- wkFocus::session_tbl %>% 
  dplyr::left_join(wkFocus::group_tbl, by = "GID") %>% 
  dplyr::mutate(Mins = Duration/60) %>% 
  dplyr::select(Round, Group = GID, Phenomenon, Mins)

knitr::kable(tmp, 
             digits = 0,
             caption = "Overview of the six research-phase sessions")

## ----sample-video-frame, echo=FALSE, fig.cap = "Example frame of a coded video showing overlaid information. \\newline Coding information appears as title text overlaying the videorecording of the session. The timecode (lower right) gives minutes and seconds from the beginning of the trimmed recording. The last two digits give the number of video frames passed since the last tick of the seconds timer. All coding videos are encoded at 25 frames per second. This allows synching with other coding software to within 40 ms."----

include_graphics(file.path("images", "res-2-A-coding-eg1-med.png"), dpi = 150)


## ----focus-code-table----------------------------------------------------

# A printable copy of the coding scheme should be available in project storage

pander::pander(wkFocus::focus_code_description, justify = c("left", "left", "left"), 
       split.tables = 100, split.cells = c(2, 13, 70))


