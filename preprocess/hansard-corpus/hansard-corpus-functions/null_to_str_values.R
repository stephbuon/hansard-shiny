null_to_str_values <- function(hansard) {
  
  hansard$debate[is.na(hansard$debate)] <- "NA"
  
  return(hansard) }



