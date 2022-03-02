score_collocates <- function (collocates, ...) {

  if (... == "speakers") {
    collocates <- collocates %>% # row-wise sum
      replace(is.na(.), 0) %>% 
      mutate(combined_score = rowSums(.[3:5]))
    
    
  } else {
  
collocates <- collocates %>% # row-wise sum
  replace(is.na(.), 0) %>% 
  mutate(combined_score = rowSums(.[2:4])) }

neutral <- 0

positive <- collocates %>%
  filter(combined_score > sum(neutral, .8))
positive$sentiment <- "Positive"

negative <- collocates %>%
  filter(combined_score < sum(neutral, -.8))
negative$sentiment <- "Negative"

neutral <- collocates %>%
  filter(combined_score < sum(neutral, .8),
         combined_score > sum(neutral, -.8))

neutral$sentiment <- "Neutral"

total <- bind_rows(positive, negative, neutral)

total <- total %>%
  select(n, sentiment, decade, grammatical_collocates)#, new_speaker, clean_new_speaker)

# if (keyword == "speakers"){
#   total <- total %>%
#     rename(ngrams = grammatical_collocates)
#   
#   total$new_speaker <- separate(total$new_speaker, c("first", "last", "id"), sep = "_")
#   total <- total %>%
#     mutate(clean_new_speaker = paste(total$first, total$last))
#     
#   total <- total %>%
#     select(-first, -last, -id)
#   
# }

return(total) }