


# ----------------------------- IMPORTING THE DATA --------------------------------------------- #

#import the datasets
train <- read.csv("train.csv") ; test <- read.csv("test.csv")

#combine both to get one data
test$SalePrice <- NA
combi <- rbind(train, test)

#summary
summary(combi)
str(combi)







# ----------------------------- VARIABLE TREATMENT --------------------------------------------- #  


#MSZoning : replacing NA by mode = RL 
combi$MSZoning[is.na(combi$MSZoning)] <- "RL"


#LotFrontage : replacing NA by median
combi$LotFrontage[is.na(combi$LotFrontage)] <- median(combi$LotFrontage, na.rm = TRUE)







# ----------------------------- FEATURE ENGINEERING I --------------------------------------------- #  





