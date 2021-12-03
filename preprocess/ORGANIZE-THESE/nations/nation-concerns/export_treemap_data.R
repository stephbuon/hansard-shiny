
test <- read_csv("~/projects/hansard-shiny/nations_concerns_counts_10072021.csv") %>%
  drop_na(geograpthy)

test <- subset(test, nation != geograpthy)

test <- test %>%
  filter(!str_detect(concern, "World")) %>%
  filter(!str_detect(geograpthy, "World"))  %>%
  filter(!str_detect(geograpthy, "Ocean"))

test <- within(test, {
  f <- nation == 'France' & geograpthy == 'Britain'
  nation[f] <- 'France'
  geograpthy[f] <- 'Europe'
}) 

test$nation <- str_replace_all(test$nation, "_", " ")


test$geograpthy <- str_replace(test$geograpthy, "Britain", "UK")
test$geograpthy <- str_replace(test$geograpthy, "Australia", "Oceania")

dec <- 1830

test_2 <- test %>%
  filter(decade == dec) %>%
  rename(labels = concern,
         parents = nation) %>%
  select(-sentence_id, -n)

label_count <- test_2 %>%
  count(parents, labels, decade, geograpthy) %>%
  rename(parents_concerns_count = n)

parents_count <- test_2 %>%
  count(parents) %>%
  rename(parents_count = n)

all <- left_join(label_count, parents_count)

geography_count <- test_2 %>%
  count(geograpthy) %>%
  rename(geography_count = n) 


all_2 <- left_join(all, geography_count)


labels <- all_2$labels
parents <- all_2$parents
geography <- all_2$geograpthy

concerns_count <- all_2$parents_concerns_count
parents_count <- all_2$parents_count
geography_count <- all_2$geography_count


##################
a <- append(labels, parents)
a <- append(a, geography)

b <- append(parents, geography)
ll <- length(labels)
g <- replicate(ll, "")

b <- append(b, g)

d <- append(concerns_count, parents_count)
d <- append(d, geography_count)


#a <- append("", a)
#b <- append("", b)
#d <- append(464, d)

#############################3


c <- data.frame(a, b, d)

c <- c %>%
  distinct() 

a <- c$a
b <- c$b
values <- c$d

ggg <- length(a) + 1
values <- append(ggg, values)

a <- append("", a)
b <- append("", b)


fig <- plot_ly(
  type="treemap",
  labels = a,
  parents = b, 
  values = values
  
)
fig


write_csv(c, paste0("~/projects/hansard-shiny/treemap_", dec, ".csv"))
