`%like%` <- function (x, pattern) { 
  stringi::stri_detect_regex(x, pattern, case_insensitive=TRUE)
}

#nvm -- this has to be in the UI
#ns <- NS(id)