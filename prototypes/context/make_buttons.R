library(tidyverse)
library(text2vec)

input_loop <- function(input, decades, first_range, second_range, make_m, make_decade, ...) {
  
  out <- data.frame()
  il_decades <- decades
  
  for(d in 1:length(il_decades)) {
    fdecade <- il_decades[d] 
    
    table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    rn <- rownames(word_vectors)
    if(input %in% rn) {
      kw = word_vectors[input, , drop = F]
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[first_range:second_range])
      colnames(forplot)[1] <- paste0("similarity")
      
      forplot$word <- rownames(forplot)
      
      if (make_decade == TRUE) {
        
        forplot <- forplot %>%
          mutate(decade = fdecade) %>%
          filter(word == ...) } 
      
      if (make_m == TRUE) {
        rownames(forplot) <- NULL
        
        forplot <- forplot %>%
          select(word) }
      
      out <- bind_rows(out, forplot) } }
  
  if (make_m == TRUE) {
    out <- dplyr::distinct(out)
  }
  
  
  return(out) }


ww <- "tenants"


decades <- c(1800, 1850)


out <- input_loop(ww, decades, 2, 201, make_m = TRUE, make_decade = FALSE)
  


cycle = 0

for(d in 1:length(decades)) {
  
  cycle = cycle + 1
  
  fdecade <- decades[d] 
  
  table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
  # table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_1800.txt")
  
  word_vectors <- as.matrix(read.table(table, as.is = TRUE))
  
  rn <- rownames(word_vectors)
  
  if(ww %in% rn) {
    
    kw = word_vectors[ww, , drop = F]
    
    cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
    
    
    forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:100])#[2:21])
    
    colnames(forplot)[1] <- paste0("similarity", cycle)
    
    forplot$word <- rownames(forplot)
    
   # forplot <- forplot %>%
  #    mutate(decade = fdecade)
    
    rownames(forplot) <- NULL
    
    
    
    out <- left_join(out, forplot, by = "word") }}






out$all_sim <- (out$similarity1 - out$similarity2)

out <- out %>%
  drop_na()

out$all_sim <- abs(out$all_sim)    


 