library(tidyverse)
library(text2vec)
  

out <- data.frame()
  
  decades <- c(1800, 1850)

  for(d in 1:length(decades)) {
    
    
    fdecade <- decades[d] 
    
    table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
   # table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_1800.txt")
    
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    rn <- rownames(word_vectors)
    
    if("church" %in% rn) {
    
    kw = word_vectors["church", , drop = F]
    
    cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")

    
    forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:100])#[2:21])
    
    colnames(forplot)[1] <- paste0("similarity")
    
    forplot$word <- rownames(forplot)
    
    forplot <- forplot %>%
      mutate(decade = fdecade)
     
    rownames(forplot) <- NULL
    
    a <- forplot %>%
      select(word)
    
    out <- bind_rows(out, a)
    

    } 
    
    
    
  } 
  
  
  
  out <- dplyr::distinct(out)
  
  cycle = 0
  
  for(d in 1:length(decades)) {
    
    cycle = cycle + 1
    
    fdecade <- decades[d] 
    
    table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
    # table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_1800.txt")
    
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    rn <- rownames(word_vectors)
    
    if("church" %in% rn) {
      
      kw = word_vectors["church", , drop = F]
      
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      
      
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:100])#[2:21])
      
      colnames(forplot)[1] <- paste0("similarity", cycle)
      
      forplot$word <- rownames(forplot)
      
      forplot <- forplot %>%
        mutate(decade = fdecade)
      
      rownames(forplot) <- NULL
      

      
      out <- left_join(out, forplot, by = "word") }}
      
  
  
  

  
  out$all_sim <- (out$similarity1 - out$similarity2)

  out <- out %>%
    drop_na()

  out$all_sim <- abs(out$all_sim)    
    
 