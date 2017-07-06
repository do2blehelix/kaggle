


# ----------------------------- IMPORTING THE DATA --------------------------------------------- #

#import the datasets
train <- read.csv("train.csv") ; test <- read.csv("test.csv")

#combine both to get one data
test$Survived <- NA
combi <- rbind(train, test)

#summary
summary(combi)
str(combi)












