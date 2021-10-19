library(tidyverse)

a <- read_csv("~/hansard_c19_debate_titles_w_year.csv") %>%
  select(-decade, -group_count)

a <- a %>%
  count(debate, year)

#write_csv(a, "~/debate_title_year_counts.csv")


keywords <- c("croft", "ejectment", "enclosure", "estate", "evict", "inclosure", "landlord", "landown", "property", " rent", "tenant", "tenure")
keywords <- c("manufacturing", "sugar", "drugs", "imported", "agriculture", "soil", "cotton", "corn", "mining", "potato", "farm", "fishery")
#keywords <- c("theft", "vagrancy", "education", "employment", "underemployment", "children")
keywords <- c("overcrowding", "pollution", "construction", "dockyards", "harbours", "docks", "oil", "trains", "steamships", "smoke", "industry", "industries")
keywords <- c("road", "transport", "railway", "shipments", "ship", "passages", "freights", "cargoes", "wrecks", "cars", "locomotives", "highways")

test_1 <- str_replace(keywords, "^", "\\\\b")
test_2 <- str_replace(test_1, "$", "\\\\b")

test <- paste0(test_2, collapse='|' )

all_year_counts <- data.frame()

metadata <- data.frame()

for(i in 1:length(keywords)){
  
  keyword <- keywords[i]
  
  debate_titles_w_keyword_1 <- a %>%
    filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b"), ignore_case = TRUE))) # took "test" out of string argument and added the paste 0 stuff 
  
  
  debate_titles_w_keyword_2 <- a %>%
    filter(str_detect(debate, regex(paste0("^", keyword, "\\b"), ignore_case = TRUE)))
  
  
  debate_titles_w_keyword_3 <- a %>%
    filter(str_detect(debate, regex(paste0("\\b", keyword, "$"), ignore_case = TRUE)))
  
  debate_titles_w_keyword <- bind_rows(debate_titles_w_keyword_1, debate_titles_w_keyword_2, debate_titles_w_keyword_3)
  
  m <- debate_titles_w_keyword %>%
    select(debate, year) %>%
    mutate(keywords = keyword)
  
  metadata <- bind_rows(metadata, m)
  
  year_count <- debate_titles_w_keyword %>%
    group_by(year) %>%
    summarise(debates_per_year = n()) %>%
    arrange(year) %>%
    mutate(keywords = keyword)
  
  all_year_counts <- rbind(all_year_counts, year_count)
}

write_csv(all_year_counts, "~/all_year_counts_property.csv")

words_per_year <- a %>%
  group_by(year) %>%
  summarise(words_per_year = n()) 


#write_csv(words_per_year, "~/words_per_year.csv")

all_year_counts <- all_year_counts %>%
  left_join(words_per_year, by = "year") %>%
  mutate(proportion = debates_per_year/words_per_year)


#test_2 <- left_join(all_year_counts, metadata, by = c("keywords", "year")) # just added

#write_csv(metadata, "~/kw_metadata_industry.csv") # just added

write_csv(all_year_counts, "~/kw_list_property.csv")

rr <- ggplot(data = all_year_counts) +
  geom_col(aes(x = year, 
               y = proportion,
               fill=reorder(keywords, debates_per_year))) + 
  scale_fill_viridis(discrete = TRUE, option = "C")+
  guides(fill = guide_legend(title = "Keywords")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(y = "debates per year as proportion", 
       title = "Count of Debate Titles That Include Keywords")

ggplotly(rr)
