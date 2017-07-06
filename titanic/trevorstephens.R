

###################################### PART 1 ######################################

  # Trevor Stephens - 9 Jan 2014
  # Titanic: Getting Started With R - Part 1: Booting up in R
  # Full guide available at http://trevorstephens.com/


# Set working directory and import datafiles
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
# Examine structure of dataframe
  str(train)
  
# Look at number of people who survived
  table(train$Survived)
  prop.table(table(train$Survived))
  
# Create new column in test set with our prediction that everyone dies
  test$Survived <- rep(0, 418)
  
# Create submission dataframe and output to file
  submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
  write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  
  

###################################### PART 2 ######################################
  
  # Titanic: Getting Started With R - Part 2: The gender-class model

    
# Set working directory and import datafiles
  rm(list=ls())
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")  
  
# Look at gender patterns
  summary(train$Sex)
  prop.table(table(train$Sex, train$Survived))
  prop.table(table(train$Sex, train$Survived), 1)
  
# Create new column in test set with our prediction that everyone dies
  test$Survived <- 0
# Update the prediction to say that all females will survive
  test$Survived[test$Sex == 'female'] <- 1
  
# Create submission dataframe and output to file
  submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
  write.csv(submit, file = "gendermodel.csv", row.names = FALSE)
  
# Look at age patterns
  summary(train$Age)
  train$Child <- 0
  train$Child[train$Age < 18] <- 1
  aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
  aggregate(Survived ~ Child + Sex, data=train, FUN=length)
  aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
  
# Look at class and fare patterns
  train$Fare2 <- '30+'
  train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
  train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
  train$Fare2[train$Fare < 10] <- '<10'
  aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#ADDED FUNCTION FROM COMMENTS STATNG TO CHECK THE COUNT OF PASSENGERS AS WELL
  aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {paste(length(x), sum(x)/length(x))})
  
# Create new column in test set with our prediction that everyone dies
  test$Survived <- 0
# Update the prediction to say that all females will survive
  test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
  test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
  
# Create submission dataframe and output to file
  submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
  write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  
  
###################################### PART 3 ######################################

  # Titanic: Getting Started With R - Part 3: Decision Trees

  
# Set working directory and import datafiles
  rm(list=ls())
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
# Install and load required packages for fancy decision tree plotting
  install.packages('rattle')
  install.packages('rpart.plot')
  install.packages('RColorBrewer')
  library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  
# Recreate the gender model
  fit <- rpart(Survived ~ Sex, data=train, method="class")
  fancyRpartPlot(fit)
  
# Build a deeper tree
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
  # Plot it with base-R
  plot(fit)
  text(fit)
# And then make it look better with fancyRpartPlot!
  fancyRpartPlot(fit)
  
# Now let's make a prediction and write a submission file
  Prediction <- predict(fit, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
  
# Let's unleash the decision tree and let it grow to the max
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
               method="class", control=rpart.control(minsplit=2, cp=0))
  fancyRpartPlot(fit)
  
# Now let's make a prediction and write a submission file
  Prediction <- predict(fit, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "myfullgrowntree.csv", row.names = FALSE)
  
# Manually trim a decision tree
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
               method="class", control=rpart.control(minsplit=2, cp=0.005))
  new.fit <- prp(fit,snip=TRUE)$obj
  fancyRpartPlot(new.fit)  
  
  

  
  
  
  
  
  
  
  
  
###################################### PART 4 ######################################

  # Titanic: Getting Started With R - Part 4: Feature Engineering

  
# Set working directory and import datafiles
  rm(list=ls())
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
# Install and load required packages for fancy decision tree plotting
  library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  
# What's in a name?
  train$Name[1]
  
# Join together the test and train sets for easier feature engineering
  test$Survived <- NA
  combi <- rbind(train, test)
  
# Convert to a string
  combi$Name <- as.character(combi$Name)
  # What's in a name, again?
  combi$Name[1]
  
# Find the indexes for the tile piece of the name
  strsplit(combi$Name[1], split='[,.]')
  strsplit(combi$Name[1], split='[,.]')[[1]]
  strsplit(combi$Name[1], split='[,.]')[[1]][2]
  
# Engineered variable: Title
  combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2]  # Won't work!
  combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  combi$Title <- sub(' ', '', combi$Title)
# Inspect new feature
  table(combi$Title)
# Combine small title groups
  combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
  combi$Title <- factor(combi$Title)
  
# Engineered variable: Family size
  combi$FamilySize <- combi$SibSp + combi$Parch + 1
  
# Engineered variable: Family
  combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
  combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
  table(combi$FamilyID)
# Delete erroneous family IDs
  famIDs <- data.frame(table(combi$FamilyID))
  famIDs <- famIDs[famIDs$Freq <= 2,]
  combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
  combi$FamilyID <- factor(combi$FamilyID)
  
# Split back into test and train sets
  train <- combi[1:891,]
  test <- combi[892:1309,]
  
# Build a new tree with our new features
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data=train, method="class")
  fancyRpartPlot(fit)
  
# Now let's make a prediction and write a submission file
  Prediction <- predict(fit, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "engineeredfeaturestree.csv", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  
###################################### PART 5 ######################################

  # Titanic: Getting Started With R - Part 5: Random Forests

  
# Set working directory and import datafiles
  rm(list=ls())
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
# Install and load required packages for decision trees and forests
  library(rpart)
  library(randomForest)
  library(party)
  
# Join together the test and train sets for easier feature engineering
  test$Survived <- NA
  combi <- rbind(train, test)
  
# Convert to a string
  combi$Name <- as.character(combi$Name)
  
# Engineered variable: Title
  combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
  combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
  combi$Title <- factor(combi$Title)
  
# Engineered variable: Family size
  combi$FamilySize <- combi$SibSp + combi$Parch + 1
  
# Engineered variable: Family
  combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
  combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
  famIDs <- data.frame(table(combi$FamilyID))
  famIDs <- famIDs[famIDs$Freq <= 2,]
  combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
  combi$FamilyID <- factor(combi$FamilyID)
  
# Fill in Age NAs
  summary(combi$Age)
  Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                  data=combi[!is.na(combi$Age),], method="anova")
  combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
  summary(combi)
# Fill in Embarked blanks
  summary(combi$Embarked)
  which(combi$Embarked == '')
  combi$Embarked[c(62,830)] = "S"
  combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
  summary(combi$Fare)
  which(is.na(combi$Fare))
  combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
  
# New factor for Random Forests, only allowed <32 levels, so reduce number
  combi$FamilyID2 <- combi$FamilyID
# Convert back to string
  combi$FamilyID2 <- as.character(combi$FamilyID2)
  combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
  combi$FamilyID2 <- factor(combi$FamilyID2)
  
# Split back into test and train sets
  train <- combi[1:891,]
  test <- combi[892:1309,]
  
# Build Random Forest Ensemble
  set.seed(415)
  fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                      data=train, importance=TRUE, ntree=2000)
# Look at variable importance
  varImpPlot(fit)
# Now let's make a prediction and write a submission file
  Prediction <- predict(fit, test)
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "firstforest.csv", row.names = FALSE)
  
# Build condition inference tree Random Forest
  set.seed(415)
  fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                 data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
  Prediction <- predict(fit, test, OOB=TRUE, type = "response")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "ciforest.csv", row.names = FALSE)  
  
  
  
  
  
    