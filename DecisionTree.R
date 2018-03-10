library(caTools)
library(rpart)
library(rpart.plot)
health <- read.csv("G:/Big Data Analytics with R/Project Discussion_Workng on Case study/Health_claim.csv")
health <- subset(health, Claim_amount > 0)
health
health <- na.omit(health)
unique(health)
df_h <- data.frame(health)
df_h
head(df_h)
summary(df_h)
str(df_h)
dim(df_h)

#Splitting the data into 80/20
set.seed(1000)
split = sample.split(df_h, SplitRatio = 0.8)
train = subset(df_h, split == TRUE)
test = subset(df_h, split == FALSE)

#Implementing Decision Tree
tree1 <- rpart(age~., data = train, method = "class")
tree1
rpart.plot(tree1, type = 4, extra = 103)
pred = predict(tree1, test[,-2], type = "class")
pred

#Comparing the Actual and Predicted Data
result1 <- data.frame("Actual" = test[,2], "Predicted" = pred)
result1
#Confusion Matrix to calculate Accuracy 
confMat1 <- table(test[,2], pred)
confMat1
#Accuracy of the model
accu1 <- sum(diag(confMat1)) / nrow(df_h) 
accu1 #The accuracy is 0.01062164

tree2 <- rpart(Days_admitted~ +Num_medical_bills, data = train, method = "class")
tree2
rpart.plot(tree2, type = 4, extra = 103)
pred1 = predict(tree2, test[,-4], type = "class")
pred1

#Comparing the Actual and Predicted Data
result2 <- data.frame("Actual" = test[,4], "Predicted" = pred1)
result2
#Confusion Matrix to calculate Accuracy 
confMat2 <- table(test[,4], pred1)
confMat2
##Accuracy of the model
accu2 <- sum(diag(confMat2)) / nrow(df_h)
accu2 ##The accuracy is 0.06779231

tree3 <- rpart(Days_admitted~+Claim_amount, data = train, method = "class")
tree3
rpart.plot(tree3, type = 4, extra = 103)

pred2 = predict(tree3, test[, -4], type = "class")
pred2
#Comparing the Actual and Predicted Data
result3 <- data.frame("Actual" = test[,4], "Predicted" = pred2) 
result3
#Confusion Matrix to calculate Accuracy of the Model
confMat3 <- table(test[,4], pred2)
confMat3
##Accuracy of the model
accu3 <- sum(diag(confMat3)) / nrow(df_h)
accu3 ##The accuracy is  0.06779231

tree4 <- rpart(Num_medical_bills ~ Claim_amount, data = train, method = "class")
tree4
rpart.plot(tree4, type = 4, extra = 103)
pred3 = predict(tree4, test[,- 5], type = "class")
pred3
#Comparing the Actual and Predicted Data
result4 <- data.frame("Actual" = test[,5], "Predicted" = pred3)
result4
#Confusion Matrix to calculate Accuracy
confMat4 <- table(test[,5], pred3)
confMat4
##Accuracy of the model
accu4 <- sum(diag(confMat4)) / nrow(df_h)
accu4 ##The accuracy is  0.1358385
