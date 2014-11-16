train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv")

train$Survived <- as.factor(train$Survived)

# эти пакеты нужны для красивого отображения деревьев
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

tree <- rpart(Survived ~ Sex+Age+Pclass+Parch, data = train, method="class")
fancyRpartPlot(tree)

prediction <- predict(tree, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "simple-dtree.csv", row.names = FALSE)
