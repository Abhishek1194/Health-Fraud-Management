library(randomForest)
df <- read.csv("G:/Big Data Analytics with R/Project Discussion_Workng on Case study/Health_claim.csv")
df <- subset(df, Claim_amount > 0)
df
df <- na.omit(df)
unique(df)
df_health <- data.frame(df)
df_health
head(df_health)
str(df_health)
summary(df_health)
dim(df_health)

#Splitting the data into 80/20
set.seed(1000)
split = sample.split(df_health, SplitRatio = 0.80)
train = subset(df_health, split == T)
test = subset(df_health, split == F)

#Implementing Random Forest
forest1 <- randomForest(age~., data = train, method = "class", ntree = 10)
forest1
pred1 <- predict(forest1, test[,-2], type = "class")
pred1
#Confusion Matrix to calculate Accuracy
confMat <- table(test[,2], pred1)
confMat
#Accuracy of the model
accuracy1 <- sum(diag(confMat)) / nrow(df_health)
accuracy1 #The accuracy of the model is 0

forest2 <- randomForest(Days_admitted ~ Num_medical_bills, data = train, method = "class", ntree = 10)
forest2
pred2 <- predict(forest2, test[,-4], type = "class")
pred2
#Confusion Matrix to calculate Accuracy 
confMat <- table(test[,4], pred2)
confMat
#Accuracy of the Model
accuracy2 <- sum(diag(confMat)) / nrow(df_health)
accuracy2

forest3 <- randomForest(Days_admitted~ Claim_amount, data = train, method = "class", ntree = 10)
forest3
pred3 <- predict(forest3, test[, -4], type = "class")
pred3
#Confusion Matrix to calculate Accuracy 
confMat <- table(test[,4], pred3)
confMat
#Accuracy of the Model
accuracy3 <- sum(diag(confMat)) / nrow(df_health)
accuracy3

forest4 <- randomForest(Num_medical_bills~Claim_amount, data = train, method = "class", ntree = 10)
forest4
pred4 <- predict(forest4, test[,-5], type = "class")
pred4
#Confusion Matrix to calculate Accuracy 
confMat <- table(test[,5], pred4)
confMat
#Accuracy of the Model
accuracy4 <- sum(diag(confMat)) / nrow(df_health)
accuracy4
