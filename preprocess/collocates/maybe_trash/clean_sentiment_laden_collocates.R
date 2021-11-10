# COME BACK TO THIS 

library(tidyverse)
library(lubridate)

#property_collocates <- read_csv("~/projects/hansard-shiny/cleaned_collocates_sentiment_laden_noun_modifiers_property_keywords_07222021.csv")
property_collocates <- read_csv("~/projects/hansard-shiny/cleaned_collocates_sentiment_laden_noun_modifiers_concerns_keywords.csv")

decade <- 10

neutral <- 0

property_collocates <- property_collocates %>%
  mutate(decade = year - year %% decade) %>%
  select(-year)

property_collocates <- property_collocates %>%
  group_by(grammatical_collocates, decade) %>%
  add_count() %>%
  unique() %>%
  ungroup()

positive <- property_collocates %>%
  filter(combined_score > neutral)

positive$sentiment <- "Positive"

negative <- property_collocates %>%
  filter(combined_score < neutral)

negative$sentiment <- "Negative"

total <- bind_rows(positive, negative)

total <- total %>%
  select(n, sentiment, decade, grammatical_collocates)

write_csv(total, "~/projects/hansard-shiny/data/collocates/concerns_collocates.csv")



a <- read_csv("~/projects/hansard-shiny/data/collocates/concerns_collocates.csv") 

a <- a %>%
  filter(n > 5)

a <- a %>%
  filter(str_detect(grammatical_collocates, "right|general|honour|care|favour|advanced|interest", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "with| the$| of$| like$| as$| in$| i$| a$| was$ | by$", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "—|:|\\.|\\(|\\)|,|-|\"|£|\\;|\\?|\\'s", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "campbell|baron|peel", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, " on$| are$| be$| see$| were$| to$| so$| and$| too$| than$| that$|direct| for$| from$| got$| at$| an$| or$| its$| if$| had$| upon| about| real| first| either| certain|^ground | mean|^men |^man |woman |women |chairman |clergyman ", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, " have$| he$| gives$| loyal| respecting|hand | made| has$| no$| confirmed| should| only| new| between| want| this| said| naturally| took|closed|bench|advance|importance |manner ", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, " was$| join$| was|measure| attend|important| find| is$| what|used$|appointed | increase|form|hoped|making| how| would| ask| met| fit$| few$| under$| into$| high$| thorough| though| motion| major| put| grown| hard| higher", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "such|wasted|holding|dust|command|bounty|miss|rose| old$| it$| just$| kind$| know$|merchants |taking | little| look| like| ought| not| unfortunate| large| half| good| can| take| becom| being| begin", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "leave |evening |contributed| accept|profits|shot |established|subject |endowment|portion|suggest|value |works| impress| really| quote| say$| says$| suppose|material|masters| seem| fulfil", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "having$|who$|taken| beg$| specified| might| closed| been| 000| mere| much| noble| most| create| contain|aware| any$| giving| great| grey| give| true| o'| hon| distinguish| entitl| approach", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, "those|these| deem| example| all$| long| make| more|lead| but$| one$| ones$| other| sir$| am$| also$| sit$| seat$| beneficial| low$| lower$|speculation| practic| up$| unable| obtain", negate = T))

a <- a %>%
  filter(str_detect(grammatical_collocates, " late$|lately |penny| recommend| small| because| always| after| actual| told| extend| very| own$| held$| come$| meet$| admit| free| indulg| perfect| even| introduc| lose| lost| diseased", negate = T))


write_csv(a, "~/projects/hansard-shiny/data/collocates/concerns_collocates.csv")
