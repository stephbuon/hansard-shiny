library(tidyverse)

conerns <- read_csv("~/concerns.csv")

concerns <- unique(concerns)

concerns <- concerns %>%
  filter(str_detect(concerns, "(.*) (.*)", negate = TRUE))

write_csv(concerns, "~/single_word_concerns.csv")
