train <- read.csv("train.csv")
test <- read.csv("test.csv")

library(rpart)
# install.packages('randomForest')
library(randomForest)
# install.packages('party')
library(party)

test$Survived <- NA
union <- rbind(train, test)
transformNames <- function (sampling) {
  sampling$Name <- as.character(sampling$Name)
  
  transformFunc <- function (x) { strsplit(x, split='[,.]')[[1]][2] }
  sampling$Title <- sapply(sampling$Name, FUN=transformFunc)
  sampling$Title <- sub(' ', '', sampling$Title)
  sampling$Title <- factor(sampling$Title)
  
  table(sampling$Title)
  
  sampling$Title[sampling$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  sampling$Title[sampling$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  sampling$Title[sampling$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

  sampling$Title <- factor(sampling$Title)
  return (sampling)
}
 
checkFamily <- function(sampling) {
  sampling$FamilySize <- sampling$SibSp + sampling$Parch + 1
  
  sampling$isMother <- 0
  for(i in 1:nrow(sampling)) {
    if (sampling$Title[i]=="Mrs" && sampling$Parch[i]>0) {
      sampling$isMother <- 1
    }
  }
  
  return (sampling)
}

checkFamilyID <- function(sampling) {
  extractFunc <- function (x) { strsplit(x, split='[,.]')[[1]][1] }
  
  sampling$Surname <- sapply(sampling$Name, FUN=extractFunc)
  sampling$FamilyID <- paste(as.character(sampling$FamilySize), sampling$Surname, sep="")
  sampling$FamilyID[sampling$FamilySize <= 2] <- 'Small'
  
  famIDs <- data.frame(table(sampling$FamilyID))
  famIDs <- famIDs[famIDs$Freq <= 2, ]
  
  sampling$FamilyID[sampling$FamilyID %in% famIDs$Var1] <- 'Small'
  sampling$FamilyID <- factor(sampling$FamilyID)
  
  return (sampling)
}


# transform 'Name' in Mr, Sir, Mrs and etc.
union <- transformNames(union)

# add field 'isMother' and 'FamilySize'
union <- checkFamily(union)

# add filed FamilyID
union <- checkFamilyID(union)

summary(union$Age)

interpolateAge <- function(sampling) {
  ageData <- sampling[!is.na(sampling$Age), ]
  ageVars <- Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize
  ageTree <- rpart(ageVars, data=ageData, method="anova")
  sampling$Age[is.na(sampling$Age)] <- predict(ageTree, sampling[is.na(sampling$Age),])
  return (sampling)
}
interpolateFare <- function(sampling) {
  sampling$Fare[is.na(sampling$Fare)] <- median(sampling$Fare, na.rm=TRUE)
  return (sampling)
}

# interp. fare
summary(union$Fare)
union <- interpolateFare(union)

# interp. age
union <- interpolateAge(union)

train <- union[1:891, ]
test <- union[892:1309, ]

# random forests (first variant)
set.seed(415)
 
vars <- as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + isMother + FamilySize
forest <- randomForest(vars, data=train, importance=TRUE, ntree=2000)
varImpPlot(forest)
 
prediction <- predict(forest, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "simple-forest.csv", row.names = FALSE)

# random forests (second variant)
set.seed(415)

vars <- as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + isMother + Embarked + FamilySize + FamilyID
ci.forest <- cforest(vars, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

prediction <- predict(ci.forest, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "ci-forest.csv", row.names = FALSE)
