reorder_collocates <- function(collocates) {
  
  collocates <- separate(data = collocates, col = grammatical_collocates, into = c("left", "right"))
  
  collocates <- collocates %>%
    mutate(grammatical_collocates = paste(collocates$right, collocates$left)) %>%
    select(-left, -right)
  
  # get rid of: ½, About—And Right, etc
  collocates <- collocates %>%
    filter(!str_detect(grammatical_collocates, "^ (.*)|^(.*) $"))
  
  return(collocates) }
