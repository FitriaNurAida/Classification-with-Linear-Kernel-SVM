# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_classification.csv")
View(movie)
str(movie)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE) #imputasi with mean because it is numerical variable
movie$Start_Tech_Oscar <- as.factor(movie$Start_Tech_Oscar)

# Test-Train Split #
install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
trainc = subset(movie,split == TRUE)
testc = subset(movie,split == FALSE)

#################################### LINEAR SVM #######################################
install.packages('e1071')
library (e1071)

# Modeling #
svmfit = svm (Start_Tech_Oscar~., data=trainc , kernel = "linear", cost =1 ,scale = TRUE)
summary (svmfit)
svmfit$index #index of support vectors

# Predicting #
pred=predict (svmfit ,testc)
cm <-table(predict = pred, truth = testc$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

# Tuning Parameter
set.seed(0)
tunesvm <- tune(svm, Start_Tech_Oscar~., data=trainc, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)))
bestmodel <- tunesvm$best.model
summary(bestmodel) # the best c is 1

prednew <- predict (bestmodel,testc)
cm <-table(predict = prednew, truth = testc$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy
