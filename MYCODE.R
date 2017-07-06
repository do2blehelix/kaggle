

# ----------------------------- IMPORTING THE DATA --------------------------------------------- #

#import the datasets
  train <- read.csv("train.csv") ; test <- read.csv("test.csv")

#combine both to get one data
  test$Survived <- NA
  combi <- rbind(train, test)

#summary
  summary(combi)
  str(combi)
  

  
  
  
  

# ----------------------------- FEATURE ENGINEERING I --------------------------------------------- #  
  
#TITLE : extracting the title
  combi$Name <- as.character(combi$Name)
  combi$title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  combi$title <- sub(' ', '', combi$title)
  table(combi$title)
#combining the titles under groups  
  combi$title[combi$title %in% c('Don', 'Dona', 'Jonkheer', 'the Countess', 'Sir', 'Lady')] <- 'Honorary'
  combi$title[combi$title %in% c('Capt', 'Col', 'Major')] <- 'Military'
  combi$title <- factor(combi$title)
  table(combi$title)


#FAMILY : combine no of family members
  combi$fam.mem <- combi$SibSp + combi$Parch + 1
  combi$fam.yn <- ifelse(combi$fam.mem > 1, 1, 0)
  combi$fam.size <- 'single'
  combi$fam.size[which(combi$fam.mem > 1 & combi$fam.mem < 5)] <- 'small'
  combi$fam.size[which(combi$fam.mem > 4)] <- 'large'
  combi$fam.size <- as.factor(combi$fam.size)
  

#TICKET : check no of persons having same tkt no. also divide ticket into mumeric and special
  x <- aggregate(Ticket ~ combi$Ticket , combi , FUN = "length")
  names(x) <- c("Ticket" , "nos.tkt")
  combi <- merge(x, combi, by = "Ticket") ; rm(x)
  combi$sp.tkt <- substr(combi$Ticket,1,1)
  combi$sp.tkt <- as.factor(ifelse(is.na(as.numeric(combi$sp.tkt))==TRUE, "spl", "reg"))
  
  
#Marriage : Check if married
  combi$married[combi$title %in% c('Miss', 'Master', "Mlle", 'Rev' )] <- 'No'
  combi$married[is.na(combi$married)] <- 'Yes' 
  combi$married <- as.factor(combi$married)
  
#Siblings : No of siblings if married
  combi$sib <- ifelse(combi$married == "Yes", combi$SibSp - 1, combi$SibSp)
  combi$sib <- ifelse(combi$sib < 0, 0, combi$sib)
    
    
    
    
      
# ----------------------------- VARIABLE TREATMENT --------------------------------------------- #    

  
#FARE : substitute values for NA = from S
  combi$Fare[is.na(combi$Fare)] <- 
    median(combi$Fare[which(combi$Pclass==3 & combi$Embarked == "S")], na.rm = TRUE)

  
#EMBARKMENT : replacing missing values in port of embarkment (common sense)
  combi$Embarked[combi$Embarked==""]  <- "C"  


#CABIN : generating deck // generating cabin no (multiple cabins and missing = 0)
  combi$Cabin <- as.character(combi$Cabin)
  combi$Cabin[combi$Cabin == ""] <- "X00"
  combi$cab.deck <- as.factor(substr(combi$Cabin,1,1))
  combi$cab.no <- as.numeric(substr(combi$Cabin,2,4))
  combi$cab.no[is.na(combi$cab.no)] <- 0
  

#check missing values
  colSums(is.na(combi))


#AGE : impute using GBM
  library(caret)
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) 
  pred.age <- train(Age ~ nos.tkt + Pclass + Sex + SibSp + Parch + Fare + Embarked +
               title + fam.mem + fam.yn + fam.size + sp.tkt + married + sib + cab.deck + cab.no
                , data = combi[!is.na(combi$Age),], method = "gbm", trControl = fitControl, verbose = FALSE)
  
  combi$Age[is.na(combi$Age)] <- predict(pred.age , combi)
  
  
  
  
  
# #AGE : impute NA using MICE
#   library(mice)
#   mice_mod <- mice(combi[, !names(combi) %in% 
#                c('PassengerId','Name','Ticket','Cabin','fam.size','Survived')]
#                    , m=1,maxit=1,meth='pmm',seed=32) 
#   mice_output <- complete(mice_mod)
#    combi$Age <- mice_output$Age


#AGE : fill Age[NA] using dTree
  # library(rpart)
  # Agefit <- rpart(Age ~ nos.tkt + Pclass + Sex + Age + Fare + Embarked + title + fam.mem + fam.yn + fam.size
  #                   + sp.tkt + cab.deck + cab.no ,
  #                 data=combi[!is.na(combi$Age),], method="anova")
  # combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
  
#fill NA in Age using measures     
  #combi$Age[is.na(combi$Age)]  <- median(combi$Age, na.rm=TRUE)

  
  
        
  
  
    
# ----------------------------- MODEL BUILDING --------------------------------------------- # 
    
# divide datasets back  
  train <- combi[!is.na(combi$Survived),]
  test <- combi[is.na(combi$Survived),]    

      
vars <- c("Pclass" , "Sex" , "Age" , "SibSp" , "Parch" , "Fare" , "Embarked" , 
            "nos.tkt" , "title" , "fam.mem" , "fam.yn" , "fam.size" , "sp.tkt" , 
              'married' , "sib" , "cab.deck" , "cab.no")
fmla <- as.formula(paste("Survived ~ ", paste(vars, collapse= "+")))







#............. Logistic ...........................#

#logistic regression model
  model.lr <- glm(fmla, data = train , family = binomial("logit")) 
  summary(lodel.lr)
  
#predict values and cross check
  train$predicted <- predict(model.lr, newdata = train, type="response")
  t <- as.matrix(table(Actual = train$Survived , Predicted = train$predicted > 0.60)) ; t
  (t[1,1] + t[2,2]) / (t[1,1]+t[1,2]+t[2,1]+t[2,2])

#run on train data and export
  test$predict <- predict(model.lr, newdata = test, type="response")
  test$predict <- ifelse(test$predict > 0.60, 1 , 0)
  submit <- data.frame(PassengerId = test$PassengerId, Survived = test$predict)
  write.csv(submit, file = "OUT.csv", row.names = FALSE)
  
  
      
  
  
  
#............. Random Forest ...........................#
  
  library(randomForest)
  
  model.rf <- randomForest(as.factor(Survived) ~ nos.tkt + Pclass + Sex + Age + Fare + Embarked + 
                            title + fam.mem + fam.yn + fam.size + sp.tkt + cab.deck + cab.no
                           , data=train, importance=TRUE, ntree=2000)
  varImpPlot(model.rf)
  
    Predicted <- predict(model.rf, test)
        submit <- data.frame(PassengerId = test$PassengerId, Survived = Predicted)
        write.csv(submit, file = "OUT.csv", row.names = FALSE)
  
  
  
        

#............. Decision Tree ...........................#

  library(rpart) ; library(rattle)
  model.dt <- rpart(fmla, data=train, method="class")
  
  fancyRpartPlot(model.dt)
  
    Predicted <- predict(model.dt, test, type = "class")
      submit <- data.frame(PassengerId = test$PassengerId, Survived = Predicted)
      write.csv(submit, file = "OUT.csv", row.names = FALSE)
    
  
  



#............. SVM ...........................#  

  library(e1071)    
      
  model.sv <- svm(fmla, data=train)
  summary(model.svm)
  
    Predicted <- predict(model.sv, test)
    Predicted <- ifelse(Predicted > 0.5 , 1 , 0)
      submit <- data.frame(PassengerId = test$PassengerId, Survived = Predicted)
      write.csv(submit, file = "OUT.csv", row.names = FALSE)
      
      
      

            
      
#............. GBM ...........................#  
      
  library(caret)
  
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) 
      
  model.gb <- train(fmla , data = train, method = "gbm", trControl = fitControl, verbose = FALSE)    

  Predicted <- predict(model.gb, test)
  Predicted <- ifelse(Predicted > 0.6 , 1 , 0)
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Predicted)
  write.csv(submit, file = "OUT.csv", row.names = FALSE)  
      