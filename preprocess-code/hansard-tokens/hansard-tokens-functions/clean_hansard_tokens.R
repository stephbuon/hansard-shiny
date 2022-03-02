
source("~/projects/hansard-shiny/preprocess/global_functions.R")

clean_hansard_tokens <- function(hansard) {
  
  hansard <- hansard %>%
    filter(!str_detect(ngrams, "[[:digit:]]"))
  
  hansard$ngrams <- str_replace(hansard$ngrams, "'s", "")
  
  stop_words <- import_stopwords_as_regex()
  
  hansard <- hansard %>%
    filter(!str_detect(ngrams, stop_words)) 
  
  return(hansard) }



#h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "_", " ") 
#h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "[:digit:]", "")

#h$clean_new_speaker <- str_to_title(h$clean_new_speaker)

