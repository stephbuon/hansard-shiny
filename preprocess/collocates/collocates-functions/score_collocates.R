score_collocates <- function (collocates) {

collocates <- collocates %>% # row-wise sum
  replace(is.na(.), 0) %>% 
  mutate(combined_score = rowSums(.[2:4]))

neutral <- 0

positive <- collocates %>%
  filter(combined_score > neutral)

positive$sentiment <- "Positive"

negative <- collocates %>%
  filter(combined_score < neutral)

negative$sentiment <- "Negative"

neutral <- collocates %>%
  filter(combined_score == neutral)

neutral$sentiment <- "Neutral"

total <- bind_rows(positive, negative, neutral)

total <- total %>%
  select(n, sentiment, decade, grammatical_collocates)

return(total) }