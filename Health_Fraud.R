library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(tree)
df <- read.csv("G:/Big Data Analytics with R/Project Discussion_Workng on Case study/Health_claim.csv")
health <- subset(df, Claim_amount > 0)

str(df)
is.numeric(df)
is.character(df)
dataframe <- data.frame(df$Patient_id, df$Distance_from_clinic, df$Num_medical_bills, df$Claim_amount)
dataframe
View(df)
attach(dataframe)
summary(dataframe)
str(dataframe)

#Simple Plotting of the data
plot(Patient_id, Claim_amount)
plot(Distance_from_clinic, Num_medical_bills)

hist(Claim_amount)
hist(Num_medical_bills)
hist(Distance_from_clinic)

boxplot(Distance_from_clinic)
boxplot(Num_medical_bills)
boxplot(Claim_amount)

#Mean of the data
mean(Claim_amount, na.rm = T)
mean(Distance_from_clinic, na.rm = T)
mean(Num_medical_bills, na.rm = T)

#Finding the quantile of the model
quantile(Distance_from_clinic, probs = c(0,.25,.50,.75,1), na.rm = T)
#Inter-Quartile Range
IQR_D <- 8300 - 3500
IQR_D
#Outliers
outlier1 <- 3500 - 1.5*IQR_D
outlier1
outlier2 <- 8300 + 1.5*IQR_D
outlier2
summary(Distance_from_clinic)

quantile(Num_medical_bills, probs = c(0,.25,.5,.75,1), na.rm = T)
IQR_N <- 2 - 1
IQR_N
outlier3 <- 1 - 1.5*IQR_N
outlier4 <- 2 + 1.5*IQR_N
outlier3
outlier4
summary(Num_medical_bills)

quantile(Claim_amount, probs = c(0,.25,.5,.75,1), na.rm = T)
IQR_C <- 2851.75 - 279
IQR_C
outlier5 <- 279 - 1.5*IQR_C 
outlier6 <- 2851.75 + 1.5*IQR_C
outlier5
outlier6
summary(Claim_amount)

ggplot(data = dataframe, aes(x = Patient_id, y = Distance_from_clinic))+geom_point()
ggplot(data = dataframe, aes(x = Patient_id, y = Num_medical_bills))+geom_point()

df1 <- na.omit(df)
#clusters
set.seed(1000)
hCluster <- kmeans(df1[,3:6], 3, nstart = 20)
hCluster

table(hCluster$cluster, df1$Distance_from_clinic)
hCluster$cluster <- as.factor(hCluster$cluster)
ggplot(df1, aes(Patient_id, Distance_from_clinic, color = hCluster$cluster))+geom_point()
ggplot(df1, aes(Patient_id, Num_medical_bills, color = hCluster$cluster))+geom_point()

resultdf <- cbind(Distance_from_clinic, potential_fraud = hCluster$cluster)
resultdf
plot(resultdf)


#Supervised
set.seed(1000)
split = sample.split(df, SplitRatio = 0.80)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)
nrow(train)
nrow(test)
str(train)
str(test)

#Train
model <- glm(age~ Distance_from_clinic+Days_admitted+Num_medical_bills, data = train, family = "gaussian")
model
plot(train$age, train$Patient_id)
plot(train$age, train$Distance_from_clinic)
plot(train$age, train$Days_admitted)
plot(train$age, train$Num_medical_bills)
plot(train$age, train$Claim_amount)
#predict
pred = predict(model, df1[,-2], type = "response")
pred

#rounding the results
pred = round(pred)
head(pred)
head(train)

resdf <- data.frame("Actual" = df1[,2], "Predicted" = pred)
resdf

#accuracy by conF Matrix
confMat = table(df1[,2], pred)
confMat
pred

#calculate the accuracy
accuracy <- sum(diag(confMat))/nrow(df1)
accuracy
summary(model)

