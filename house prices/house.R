


# ----------------------------- IMPORTING THE DATA --------------------------------------------- #

#import the datasets
train <- read.csv("house prices\\train.csv") ; test <- read.csv("house prices\\test.csv")

#combine both to get one data
test$SalePrice <- NA
combi <- rbind(train, test)

#summary
summary(combi)
str(combi)








#............. Decision Tree ...........................#

library(rpart) ; library(rattle)
model.dt <- rpart(SalePrice ~ . , data=train)

fancyRpartPlot(model.dt)

fancyRpartPlot(model.dt)
plot(model.dt)
