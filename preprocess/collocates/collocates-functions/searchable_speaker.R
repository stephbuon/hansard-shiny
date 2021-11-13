searchable_speaker <- function(collocates, keyword) {
  
  collocates$new_speaker <- "William_Grenville_777"
  
  collocates <- separate(collocates, new_speaker, into = c("first", "last", "id"), sep = "_", remove = FALSE)
  
  collocates <- collocates %>%
    mutate(clean_new_speaker = paste(first, last))
  
  collocates <- collocates %>%
    select(-first, -last, -id)
  
  return(collocates) }
  