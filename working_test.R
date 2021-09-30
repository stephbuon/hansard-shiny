
from <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
to <- c(3, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11)
from_name <- c('Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Dummy Name', 'Dummy Name', 'Dummy Name', 'Dummy Name', 'Dummy Name') 
to_name <- c('corporation-discharge-duty', 'friend-be-willing', 'who-not-see-on-occasion', 'planter-be-commendable', 'what-not-occur-with-respect', 'it-be-in-opinion', 'it-be-in-opinion', 'it-be-in-opinion', 'he-talks-to-members', 'he-vote-on-bill', 'he-return-to-court')
e1 <- data.frame(from_name, to_name, from, to)

id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
group <- c('speaker', 'speaker', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple')
label <- c('Mr. Gladstone', 'Dummy Name', 'corporation-discharge-duty', 'friend-be-willing', 'who-not-see-on-occasion', 'planter-be-commendable', 'what-not-occur-with-respect', 'it-be-in-opinion', 'he-talks-to-members', 'he-vote-on-bill', 'he-return-to-court')
decade <- c('1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800')
n1 <- data.frame(id, group, label)


