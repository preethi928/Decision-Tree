library(C50)
library(caret)

fraud<-read.csv("C:\\Data_science\\EXCLER\\My Assignments\\Decision\\Fraud_check.csv")
str(fraud)
View(fraud)
#NA VALUES
is.na(fraud)
sum(is.na(fraud))
#No NA VALUES

#visulization
plot(fraud$Undergrad)
plot(fraud$Taxable.Income)
#lets check outliers

boxplot(fraud$Taxable.Income)
#outliers NONE

#lets do data partition and set seed 
df<-createDataPartition(fraud$Taxable.Income,p=.70,list = F)

set.seed(7)

#train and test data

fraud_train<-fraud[0:300,]
fraud_test<-fraud[301:600,]

#lets build model
model1<-C5.0(as.factor(fraud_train$Taxable.Income)~.,data = fraud_train)
#summary of the model
summary(model1)

#prediction
pred_fraud<-predict.C5.0(model1,fraud_test)
pred_fraud

#accuracy check 
a<-table(fraud_test$Taxable.Income,pred_fraud)
sum(diag(a))/sum(a)

#lets plot model
plot(model1)

#lets plot hist to check taxable income

hist(fraud$Taxable.Income)
#there no much of data which is that risky , more of the data is safe .
#more of the taxable income is >30000