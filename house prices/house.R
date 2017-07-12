


# ----------------------------- IMPORTING THE DATA --------------------------------------------- #

#import the datasets
train <- read.csv("house prices\\train.csv") ; test <- read.csv("house prices\\test.csv")

#combine both to get one data
test$SalePrice <- NA
combi <- rbind(train, test)

#summary
summary(combi)
str(combi)







# ----------------------------- VARIABLE TREATMENT --------------------------------------------- #  


#MSZoning : replacing NA by mode = RL 
combi$MSZoning[is.na(combi$MSZoning)] <- "RL"


#LotFrontage : imputing NA based on LotFrontage data (simple linear regression)
combi$LotFrontage[is.na(combi$LotFrontage)] <- predict(lm(LotFrontage ~ LotArea , data=combi))


#Alley : replacing NA by 'No' Alley Access
combi$Alley <- factor(ifelse(is.na(combi$Alley), "No", paste(combi$Alley)), levels = c(levels(combi$Alley), "No"))


#Utilities : replacing by Mode=AllPub
combi$Utilities[is.na(combi$Utilities)] <- "AllPub"


#Exterior1st & Exterior2nd : Wd Sdng as all Roof Material with 'Tar&Grv' and remodeled recently have the same 
combi[which(combi$RoofMatl == "Tar&Grv"),]
combi$Exterior1st [is.na(combi$Exterior1st)] <- "Wd Sdng"
combi$Exterior2nd [is.na(combi$Exterior2nd)] <- "Wd Sdng"



#MasVnrType : 
combi$MasVnrType

table(combi$RoofMatl , addNA(combi$MasVnrType))

table(combi$MasVnrArea , (combi$MasVnrType)

summary(combi$MasVnrType)

summary(combi)




# ----------------------------- FEATURE ENGINEERING I --------------------------------------------- #  



