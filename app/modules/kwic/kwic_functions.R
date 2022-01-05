set_window_size <- function(df, window_size){
  
  if (window_size != "Full") {
    df$pre <- str_extract(df$pre, paste0("([^\\s]+\\s+){0,", window_size, "}")) # fix this one
    df$post <- str_extract(df$post, paste0("([^\\s]+\\s+){0,", window_size, "}")) }
  else {
    df$pre <- str_extract(df$pre, paste0("([^\\s]+\\s+){0,300}")) # fix this one
    df$post <- str_extract(df$post, paste0("([^\\s]+\\s+){0,300}")) }
  
  return(df) }
