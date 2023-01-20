library(dplyr)

set.seed(1)
test1 <- data.frame(ID = 1:20, Var1 = sample(c(1, 0), size = 20, replace = TRUE), Var2 = sample(c("A", "B"), size = 20, replace = TRUE),
                    Var3 = sample(c(NA, "Special", "Needs"), size = 20, replace = TRUE), New = rep(NA_character_, 20), stringsAsFactors = FALSE)

test2 <- data.frame(ID = sample(9:20, replace = FALSE, size = 8), New = sample(c("Foo", "Bar"), replace = TRUE, size = 12), stringsAsFactors = FALSE)

test1
test2
test2 <- test2[order(test2$ID), ]


test2$Var1 <- NA
test2$Var1[which(test2$ID %in% test1$ID)] <- test1$Var1[which(test1$ID %in% test2$ID)]

test3 <- merge(test1, test2, by = names(test2), all = TRUE)
test3

test4 <- left_join(test1, test2, by = c("ID", "Var1", "New"))
test4


test1$Var1 <- as.numeric(test1$Var1)
str(test3)
names(test2)

##### dplyr stuff

data("starwars")
mumma <- starwars %>% filter(species != "Droid" ) %>% mutate(bmi = mass / ((height / 100) ^ 2), fake = mass * 2) %>% select(-one_of(c("films", "starships", "vehicles")))
str(mumma)
mumma <- as.data.frame(mumma)
str(mumma)

