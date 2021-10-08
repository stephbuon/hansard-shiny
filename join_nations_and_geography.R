
ncc <- read_csv("~/nations_concerns_09052021.csv")

cn <- read_csv("~/collaborative_nations.csv") %>%
  select(Nation_Base) %>%
  rename(nation = Nation_Base)
  
geo <- read_csv("~/collaborative_nations.csv") %>%
  select(Geography) %>%
  rename(geograpthy = Geography)

cn$sentence_id <- seq.int(nrow(cn))
geo$sentence_id <- seq.int(nrow(geo))

cn <- cn %>%
  distinct(nation, .keep_all= T) %>%
  drop_na()

cleaned <- left_join(cn, geo, on = sentence_id)


out <- left_join(ncc, cleaned, on = nation)

