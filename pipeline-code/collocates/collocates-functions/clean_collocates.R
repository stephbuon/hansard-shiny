
clean_collocates <- function(collocates, keyword) {
  
  collocates <- collocates %>%
    filter(!str_detect(grammatical_collocates, "^[:punct:] | [:punct:]$|^[:digit:]| (.*)[:digit:]$| [:digit:](.*)$|^[:digit:](.*)"))
  
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "[:punct:]", "")
  
  collocates <- collocates %>%
    filter(!str_detect(grammatical_collocates, "^(.*) $|^ (.*)$"))
  
  collocates$grammatical_collocates <- str_to_title(collocates$grammatical_collocates) 
  
  # make this cleaner: 
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "Govt", " Government")
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "Gent$", " Gentleman")
  
  remove <- c("Hon(.*)Right", "Lord Nob", "Friend Nob", "Sec(.*)Chief", "Earl Nob", "Committee Select", 
              "Marquess Nob", "Marquis Nob", "Lieutenant(.*)Deputy", "Sec(.*)Nob", "Sec(.*)Under", 
              "Hand Other", "Time Same", "Gentleman Right", "Lord Civil", "Session", "Hear Hear", " such$")
  
  for(r in remove){
    collocates <- collocates %>% 
      filter(!(str_detect(grammatical_collocates, regex(r, ignore_case = TRUE)))) }
  
  
  if (keyword == "all") {
    remove <- c("attorney", " as$", "^as ", " at$", "^at ", " a$", "^a ", " Ab$", "^Ab ", "^Aad ", " Aad$", " First$", " Second$")
    
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