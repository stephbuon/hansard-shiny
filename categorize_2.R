library(tidyverse)
library(viridis)
library(plotly)


hansard <- read_csv("~/hansard_c19_debate_titles_w_year.csv") %>%
  select(-decade)

hansard <- hansard %>%
  rename(year_count = group_count)

all_debate_titles_metadata <- hansard %>%
  select(debate, year, year_count)
write_csv(all_debate_titles_metadata, "~/all_debate_titles_metadata.csv") #(a, "~/metadata_debate_titles.csv")

debate_title_year_counts <- hansard %>%
  select(debate, year, year_count) %>%
  rename(n = year_count)
write_csv(debate_title_year_counts, "~/debate_title_year_counts.csv")


keywords_property <- c("croft", "ejectment", "enclosure", "estate", "evict", "inclosure", "landlord", "landown", "property", " rent", "tenant", "tenure")
keywords_resources <- c("manufacturing", "sugar", "drugs", "lumber", "agriculture", "markets", "cotton", "corn", "mining", "shop", "farm", "fishery")
#keywords <- c("theft", "vagrancy", "education", "employment", "underemployment", "children")
keywords_industry <- c("overcrowding", "pollution", "construction", "dockyards", "harbours", "docks", "oil", "trains", "steamships", "smoke", "industry", "industries")
keywords_transportation <- c("road", "transport", "railway", "shipments", "ship", "passages", "freights", "cargoes", "wrecks", "cars", "locomotives", "highways")

keywords <- switch(2, keywords_property, keywords_resources, keywords_industry, keywords_transportation)
subject <- "resources"

#test_1 <- str_replace(keywords, "^", "\\\\b")
#test_2 <- str_replace(test_1, "$", "\\\\b")
#test <- paste0(test_2, collapse='|' )

all_year_counts <- data.frame()

metadata <- data.frame()

for(i in 1:length(keywords)){
  
  keyword <- keywords[i]
  
  ddebate_titles_w_keyword <- hansard %>%
    filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b|^", keyword, "\\b|\\b", keyword, "$"), ignore_case = TRUE)))
  
  
  m <- debate_titles_w_keyword %>%
    select(debate, year) %>%
    mutate(keywords = keyword)
  
  metadata <- bind_rows(metadata, m)
  
  year_count <- debate_titles_w_keyword %>%
    group_by(year) %>%
    summarise(debates_per_year = sum(year_count)) %>% # change to group count 
    arrange(year) %>%
    mutate(keywords = keyword)
  
  all_year_counts <- rbind(all_year_counts, year_count)
}

write_csv(all_year_counts, paste0("~/all_year_counts_", subject, ".csv"))
write_csv(metadata, paste0("~/kw_metadata_", subject, ".csv")) # just added


words_per_year <- hansard %>%
  group_by(year) %>%
  summarise(words_per_year = sum(year_count)) 
write_csv(words_per_year, "~/words_per_year.csv")


all_year_counts <- all_year_counts %>%
  left_join(words_per_year, by = "year") %>%
  mutate(proportion = debates_per_year/words_per_year)
write_csv(all_year_counts, paste0("~/kw_list_", subject, ".csv"))

#test_2 <- left_join(all_year_counts, metadata, by = c("keywords", "year")) # just added


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
