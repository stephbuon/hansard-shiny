nations <- read_csv("~/concerns.csv")

nations <- unique(nations)

test <- nations %>%
  filter(str_detect(concerns, "(.*) (.*)", negate = TRUE))

#nations <- nations %>% 
#  filter(!str_detect(nations, "\\'s"))


write_csv(test, "concerns.csv")
