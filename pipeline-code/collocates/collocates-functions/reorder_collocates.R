reorder_collocates <- function(collocates) {

collocates <- separate(data = collocates, col = grammatical_collocates, into = c("left", "right"))

collocates <- collocates %>%
  mutate(grammatical_collocates = paste(collocates$right, collocates$left)) %>%
  select(-left, -right)

return(collocates) }
