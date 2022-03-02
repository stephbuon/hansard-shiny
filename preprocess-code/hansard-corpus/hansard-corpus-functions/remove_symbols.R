remove_symbols <- function(hansard) {
  
  remove <- c("\\[", "\\]", "\\(", "\\)", "—", "-", "\\.$", "\\*", "\"", "\\,", "\'", "\\>", "^ ", "\\.", " $", "  $")
  remove <- paste0(remove, collapse = '|' )
    
  hansard$debate <- str_replace_all(hansard$debate, remove, "")
  hansard$new_speaker <- str_replace_all(hansard$new_speaker, remove, "") 
    
  return(hansard) }
