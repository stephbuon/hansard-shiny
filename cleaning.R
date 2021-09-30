library(tidyverse)
library(lubridate)

nodes <- read_csv("~/projects/hansard-shiny/gladstone_nodes.csv") %>%
  select(-X1, -variable)
edges <- read_csv("~/projects/hansard-shiny/gladstone_edges.csv") %>%
  select(-X1, -speechdate)

edges <- edges %>%
  rename(to_name = triple,
         from_name = speaker)

decade <- 10

nodes <- nodes %>%
  mutate(decade = year - year %% decade) %>%
  select(-year)

edges <- edges %>%
  mutate(decade = year - year %% decade) %>%
  select(-year)

edges$to_name <- str_replace_all(edges$to_name, "\\[", "")
edges$to_name <- str_replace_all(edges$to_name, "\\]", "")
edges$to_name <- str_replace_all(edges$to_name, "\\(", "")
edges$to_name <- str_replace_all(edges$to_name, "\\)", "")

nodes <- unique(nodes) # wont need when I fix decade stuff at source



nodes <- nodes %>%
  filter(decade == 1820)



edges <- edges %>%
  filter(decade == 1820) %>%
  add_count(to_name) %>%
  top_n(20) %>%
  select(-n)


#visNetwork(nodes, edges)
