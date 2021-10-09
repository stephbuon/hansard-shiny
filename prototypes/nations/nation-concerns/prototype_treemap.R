
test <- read_csv("~/projects/hansard-shiny/nations_concerns_counts_10062021.csv") 

test_2 <- test %>%
  filter(decade == 1800) %>%
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


labels <- all$labels
parents <- all$parents
#geography <- all_2$geograpthy

concerns_count <- all$parents_concerns_count
parents_count <- all$parents_count
#geography_count <- all_2$geography_count


##################
a <- append(labels, parents)
l <- length(labels)
f <- replicate(l, "")
b <- append(parents, f)
d <- append(concerns_count, parents_count)

a <- append("", a)
b <- append("", b)
d <- append(l, d)

#############################3


c <- data.frame(a, b, d)

c <- c %>%
  distinct()

zz <- c

a <- c$a
b <- c$b
values <- c$d



fig <- plot_ly(
  type="treemap",
  #labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  #parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
  #ids = ids,
  labels = a,
  parents = b, 
  values = values
  
)
fig

