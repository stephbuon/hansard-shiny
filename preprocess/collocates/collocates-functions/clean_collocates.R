clean_collocates <- function(collocates, keyword) {
  
  collocates <- collocates %>%
    filter(!str_detect(grammatical_collocates, "^[:punct:] | [:punct:]$|^[:digit:]| (.*)[:digit:]$| [:digit:](.*)$|^[:digit:](.*)"))
  
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "[:punct:]", "")
  
  # collocates$n_values <- str_count(collocates$grammatical_collocates, "\\s+") + 1
  # 
  # test <- collocates %>%
  #   filter(n_values > 2)
  
  collocates$grammatical_collocates <- str_to_title(collocates$grammatical_collocates) 
  
  # make this cleaner: 
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "^Govt ", "Government ")
  collocates$grammatical_collocates <- str_replace(collocates$grammatical_collocates, "^Gent ", "Gentleman ")
  
  remove <- c("Hon(.*)Right", "Lord Nob", "Friend Nob", "Friend Hon", "Sec(.*)Chief", "Earl Nob", 
              "Marquess Nob", "Marquis Nob", "Lieutenant(.*)Deputy", "Sec(.*)Nob", 
              "Sec(.*)Under", "Hand Other", "Gentleman Right", "Lord Civil", "Rev(.*)Right",
              "Paymaster Nob", "General Attorney")
  
  for(r in remove){
    collocates <- collocates %>% 
      filter(!(str_detect(grammatical_collocates, regex(r, ignore_case = TRUE)))) }
  
  
  if (keyword == "all") {
    remove <- c(" as$", "^as ", " at$", "^at ", " a$", "^a ", " Ab$", "^Ab ", 
                "^Aad ", " Aad$", " First$", " Second$", "^A3 ", "^Abc ", "^In ")
    
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
  
  collocates$grammatical_collocates <- trimws(collocates$grammatical_collocates, which = c("both"))
  
  
  if (keyword == "speakers") {
    
    remove <- c(" as$", "^as ", " at$", "^at ", " a$", "^a ", " Ab$", "^Ab ", 
                "^Aad ", " Aad$", " First$", " Second$", "^A3 ", "^Abc ", "^In ")
    
    for(r in remove){
      collocates <- collocates %>% 
        filter(!(str_detect(grammatical_collocates, regex(r, ignore_case = TRUE)))) }
    
  }

  return(collocates) }