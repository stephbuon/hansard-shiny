library(tidyverse)

standardize_speaker_cols <- function(hansard) {
  
  hansard$speaker <- tolower(hansard$speaker)
  hansard$speaker <- str_replace(hansard$speaker, "mr ", "mr. ")
  
  hansard$new_speaker <- tolower(hansard$new_speaker)
  hansard$new_speaker <- str_replace(hansard$new_speaker, "mr ", "mr. ")
  
  return(hansard) }
