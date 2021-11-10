import_collocates_data <- function(slider, input_vocabulary) {
  collocates_data <- fread(paste0("~/projects/hansard-shiny/app-data/collocates/", "clean_", input_vocabulary, "_adj_noun_collocates.csv"), key = "decade")
  collocates_data <- collocates_data[.(as.numeric(slider))]
  return(collocates_data) }

filter_sentiment_ct <- function(df, sw) {
  if(sw == "All") {
    return(df) }
  df <- df %>%
    filter(sentiment == sw)
  return (df) }

tf_idf_ct <- function(df1, df2, vocabulary, custom_search, fname) {
  
  df <- bind_rows(df1, df2)
  
  df <- df %>%
    bind_tf_idf(grammatical_collocates, decade, n)
  
  df <- df %>%
    select(-n) %>%
    rename(n = tf_idf) 
  
  if (vocabulary != "property") {
    if (custom_search == "") {
      fwrite(df, fname) } }
  
  return(df)}

search_ct <- function(collocates, vals, match_type, custom_search, btnLabel) {
  
  if (vals) { # & textbox != ""
    if (custom_search != "") {
      if (match_type == "include") { 
        collocates <- collocates[grammatical_collocates %like% paste0(custom_search, "(.*)$|^(.*)", custom_search)]}
      else {
        collocates <- collocates[grammatical_collocates %like% paste0("^", custom_search, "\\b|\\b", custom_search, "$")]} } }
  else {
    if (match_type == "include") { 
      collocates <- collocates[grammatical_collocates %like% paste0(btnLabel, "(.*)$|^(.*)", btnLabel)]}
    else {
      collocates <- collocates[grammatical_collocates %like% paste0("^", btnLabel, "\\b|\\b", btnLabel, "$")]} } 
  
  return(collocates)}
