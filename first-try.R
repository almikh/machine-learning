train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv")

table(train$Survived)
prop.table(table(train$Survived))

test$Survived <- rep(0, 418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)