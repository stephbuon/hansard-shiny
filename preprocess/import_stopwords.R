
import_stopwords_as_regex <- function() {
  
  stopwords <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/stopwords_text2vec.csv") 
  
  stopwords <- stopwords %>%
    summarise(all = paste0(stop_word, collapse="|"))
  
  stopwords <- stopwords$all 
  
  return(stopwords) }


clean_data_for_word_embeddings <- function(data_decade_subset) {
  
  data_decade_subset <- data_decade_subset %>%
    filter(!str_detect(ngrams, "[[:digit:]]"))
  
  data_decade_subset$ngrams <- str_replace(data_decade_subset$ngrams, "'s", "")
  
  stopwords <- import_stopwords_as_regex()
  
  data_decade_subset <- data_decade_subset %>%
    filter(!str_detect(ngrams, stopwords))
  
  return(data_decade_subset) }
