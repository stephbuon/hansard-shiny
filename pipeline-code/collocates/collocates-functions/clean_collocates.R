
clean_collocates <- function(collocates, keyword) {
  
  collocates <- collocates %>%
    filter(!str_detect(grammatical_collocates, "^[:punct:] | [:punct:]$|^[:digit:]| (.*)[:digit:]$| [:digit:](.*)$|^[:digit:](.*)"))
  
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "[:punct:]", "")
  
  collocates <- collocates %>%
    filter(!str_detect(grammatical_collocates, "^[:alnum:] | [:alnum:]$"))
  
  collocates$grammatical_collocates <- str_to_title(collocates$grammatical_collocates) 
  
  remove <- c("Hon Right", "Lord Noble", "Secretary Chief", "Friend Nobel", "Secretary Chief", "Earl Nobel", "Reading Second",
              "Committee Select", "Marquess Noble", "Marquis Noble", "Lieutenant(.*)Deputy")
  
  for(r in remove){
    collocates <- collocates %>% 
      filter(!(str_detect(grammatical_collocates, regex(r, ignore_case = TRUE)))) }
  }
  
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "Govt$", " Government")
  
  
  if (keyword == "all") {
    remove <- c("attorney", " as$", "^as ", " at$", "^at ", " a$", "^a ", " Ab$", "^Ab ", "^Aad ", " Aad$")
    
    for(r in remove){
      collocates <- collocates %>% 
        filter(!(str_detect(grammatical_collocates, regex(r, ignore_case = TRUE)))) }
    }
  
  if (keyword == "property") {
    
    remove <- c("attorney", "splendor", "splendour")
    
    for(r in remove){
      collocates <- collocates %>% 
        filter(!(str_detect(grammatical_collocates, regex(r, ignore_case = TRUE)))) }
    
    
  }
  
  return(collocates) }